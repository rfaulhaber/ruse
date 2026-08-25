//! `eqv?` and the non-cyclic `equal?`.
//!
//! `eq?` needs no function: it is `Value`'s bitwise `PartialEq`. `eqv?` widens it by
//! numeric value identity within a representation; `equal?` widens that structurally.
//! The M3 `equal?` does not yet detect cycles (M9 adds the datum-label machinery it
//! shares with `write`), so handing it a mutated circular structure loops; the
//! conformance shim only compares reader-built data, which cannot be cyclic until M6.

use crate::gc::Heap;
use crate::value::Value;
use crate::value::layout::HeapTag;
use crate::value::object::{Bignum, Bytevector, Pair, Str, Vector};

/// R7RS `eqv?`.
pub fn eqv(heap: &Heap, a: Value, b: Value) -> bool {
    // Bitwise identity covers immediates (fixnums, chars, singletons, flonums — where
    // IEEE demands `(eqv? 0.0 -0.0)` ⇒ #f, and distinct bits deliver exactly that),
    // interned symbols, and same-object heap pointers.
    if a == b {
        return true;
    }
    // The one same-value-different-object case in the M3 numeric slice: two bignum
    // allocations. A bignum can never be `eqv?` to a fixnum — canonical demotion in
    // `Heap::integer_from_big` means an in-range value is always represented immediate.
    match (heap.get::<Bignum>(a), heap.get::<Bignum>(b)) {
        (Some(x), Some(y)) => x.value == y.value,
        _ => false,
    }
}

/// R7RS `equal?`, non-cyclic (M3).
///
/// Worklist-driven, like every other structural walk in this crate: reader-built lists
/// can be hundreds of thousands of cells long, and recursing once per cell would
/// overflow the Rust stack long before the Scheme data ran out.
pub fn equal(heap: &Heap, a: Value, b: Value) -> bool {
    let mut work: Vec<(Value, Value)> = vec![(a, b)];
    while let Some((a, b)) = work.pop() {
        if eqv(heap, a, b) {
            continue;
        }
        let (Some(ta), Some(tb)) = (heap.tag_of(a), heap.tag_of(b)) else {
            return false;
        };
        if ta != tb {
            return false;
        }
        match ta {
            HeapTag::Pair => {
                let (Some(pa), Some(pb)) = (heap.get::<Pair>(a), heap.get::<Pair>(b)) else {
                    return false;
                };
                work.push((pa.cdr, pb.cdr));
                work.push((pa.car, pb.car));
            }
            HeapTag::Str => {
                let (Some(sa), Some(sb)) = (heap.get::<Str>(a), heap.get::<Str>(b)) else {
                    return false;
                };
                if sa.chars != sb.chars {
                    return false;
                }
            }
            HeapTag::Vector => {
                let (Some(va), Some(vb)) = (heap.get::<Vector>(a), heap.get::<Vector>(b)) else {
                    return false;
                };
                if va.elems.len() != vb.elems.len() {
                    return false;
                }
                for (&x, &y) in va.elems.iter().zip(&vb.elems) {
                    work.push((x, y));
                }
            }
            HeapTag::Bytevector => {
                let (Some(xa), Some(xb)) = (heap.get::<Bytevector>(a), heap.get::<Bytevector>(b))
                else {
                    return false;
                };
                if xa.bytes != xb.bytes {
                    return false;
                }
            }
            // Everything else — symbols, closures, records, cells — is equal? only when
            // eqv?, which already said no.
            _ => return false,
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::FIXNUM_MAX;

    fn fix(n: i64) -> Value {
        Value::fixnum(n).unwrap()
    }

    #[test]
    fn eqv_is_eq_plus_number_value_identity() {
        let mut heap = Heap::new();

        assert!(eqv(&heap, fix(7), fix(7)));
        assert!(!eqv(&heap, fix(7), fix(8)));
        assert!(eqv(&heap, Value::char('q'), Value::char('q')));
        assert!(eqv(&heap, Value::NIL, Value::NIL));

        // Two separately built bignums of one value.
        let a = heap.integer(FIXNUM_MAX + 1);
        let b = heap.integer(FIXNUM_MAX + 1);
        assert_ne!(a, b, "distinct allocations are not eq?");
        assert!(eqv(&heap, a, b), "but they are eqv?");

        // IEEE zeros are distinguishable; the canonical NaN is itself.
        assert!(!eqv(&heap, Value::flonum(0.0), Value::flonum(-0.0)));
        assert!(eqv(&heap, Value::flonum(f64::NAN), Value::flonum(f64::NAN)));

        // eqv? never crosses representations or types.
        assert!(!eqv(&heap, fix(1), Value::flonum(1.0)));
        let s1 = heap.string("x");
        let s2 = heap.string("x");
        assert!(!eqv(&heap, s1, s2), "string eqv? is identity");
        assert!(eqv(&heap, s1, s1));
    }

    #[test]
    fn equal_compares_structure() {
        let mut heap = Heap::new();

        let s1 = heap.string("moin");
        let s2 = heap.string("moin");
        assert!(equal(&heap, s1, s2));

        let l1 = {
            let inner = heap.string("deep");
            let v = heap.vector(vec![inner, fix(2)]);
            heap.cons(v, Value::NIL)
        };
        let l2 = {
            let inner = heap.string("deep");
            let v = heap.vector(vec![inner, fix(2)]);
            heap.cons(v, Value::NIL)
        };
        assert!(equal(&heap, l1, l2));

        let l3 = {
            let inner = heap.string("deep");
            let v = heap.vector(vec![inner, fix(3)]);
            heap.cons(v, Value::NIL)
        };
        assert!(!equal(&heap, l1, l3));

        let bv1 = heap.bytevector(vec![1, 2, 3]);
        let bv2 = heap.bytevector(vec![1, 2, 3]);
        let bv3 = heap.bytevector(vec![1, 2]);
        assert!(equal(&heap, bv1, bv2));
        assert!(!equal(&heap, bv1, bv3));

        // Different types and different lengths are unequal, not errors.
        assert!(!equal(&heap, s1, bv1));
        let short = heap.vector(vec![fix(1)]);
        let long = heap.vector(vec![fix(1), fix(1)]);
        assert!(!equal(&heap, short, long));
    }

    /// The reason `equal?` is worklist-driven.
    #[test]
    fn very_long_lists_compare_without_recursing() {
        const CELLS: usize = if cfg!(miri) { 2_000 } else { 200_000 };
        let mut heap = Heap::new();
        let mut a = Value::NIL;
        let mut b = Value::NIL;
        for i in 0..CELLS {
            a = heap.cons(fix(i as i64), a);
            b = heap.cons(fix(i as i64), b);
        }
        assert!(equal(&heap, a, b));
        let c = heap.cons(fix(-1), a);
        assert!(!equal(&heap, c, b));
    }
}

//! Arithmetic opcodes and comparisons over the numeric tower.
//!
//! Every opcode function is layered the same way. A fixnum fast path answers the common case
//! without leaving the register file; anything else is [`unbox`]ed to a heap-free
//! [`Num`], computed by the single implementation in [`crate::num`], and boxed back
//! through [`Heap::number`]. [`unary`] and [`binary`] are that second path, factored out so
//! the numeric natives are written against the same unbox/compute/box contract instead of
//! growing their own. Fast-path results still come back through
//! [`Heap::integer`]/[`Heap::integer_from_big`], so the fixnum/bignum boundary has exactly
//! one owner.
//!
//! Exactness rules, the exact comparison across representations and the numeric edge
//! cases are [`Num`]'s; this module only maps its [`NumError`]s onto typed VM errors.

use std::cmp::Ordering;

use num_bigint::BigInt;

use crate::gc::Heap;
use crate::num::{Num, NumError, NumResult};
use crate::rt::wrong_type;
use crate::value::Value;
use crate::value::layout::HeapTag;
use crate::value::object::{Bignum, Compnum, CompnumRepr, Ratnum};
use crate::vm::error::{VmError, VmErrorKind};

/// Whether `v` is a number of any representation.
pub fn is_number(heap: &Heap, v: Value) -> bool {
    v.is_fixnum()
        || v.is_flonum()
        || matches!(
            heap.tag_of(v),
            Some(HeapTag::Bignum | HeapTag::Ratnum | HeapTag::Compnum)
        )
}

/// The tower value behind `v`, or `None` when `v` is not a number. The inverse of
/// [`Heap::number`].
pub fn unbox(heap: &Heap, v: Value) -> Option<Num> {
    if let Some(n) = v.as_fixnum() {
        return Some(Num::int(n));
    }
    if let Some(x) = v.as_flonum() {
        return Some(Num::flo(x));
    }
    match heap.tag_of(v)? {
        HeapTag::Bignum => heap.get::<Bignum>(v).map(|b| Num::int(b.value.clone())),
        HeapTag::Ratnum => heap
            .get::<Ratnum>(v)
            .map(|r| Num::from_ratio(r.value.clone())),
        HeapTag::Compnum => heap.get::<Compnum>(v).map(|c| match &c.value {
            // Boxed from a canonical `Num`, so the exact variant needs no re-canonicalising.
            CompnumRepr::Exact(z) => Num::ExactComplex(z.clone()),
            CompnumRepr::Inexact(z) => Num::inexact_complex(*z),
        }),
        _ => None,
    }
}

/// The VM error for a numeric failure inside the operation named `op`.
pub(crate) fn num_err(op: &'static str, e: NumError) -> VmError {
    VmError::new(match e {
        NumError::DivisionByZero => VmErrorKind::DivisionByZero { op },
        e => VmErrorKind::NumericDomain {
            op,
            detail: e.detail(),
        },
    })
}

/// `v` as a tower number, or the wrong-type error naming `op`.
fn operand(heap: &Heap, op: &'static str, v: Value) -> Result<Num, VmError> {
    unbox(heap, v).ok_or_else(|| wrong_type(heap, op, "a number", v))
}

/// Applies a one-operand tower operation to a Scheme value: unbox, compute, box.
pub(crate) fn unary(
    heap: &mut Heap,
    op: &'static str,
    v: Value,
    f: impl FnOnce(&Num) -> NumResult,
) -> Result<Value, VmError> {
    let x = operand(heap, op, v)?;
    let r = f(&x).map_err(|e| num_err(op, e))?;
    Ok(heap.number(r))
}

/// Applies a two-operand tower operation to Scheme values: unbox, compute, box. The left
/// operand is checked first, so it is the one a wrong-type error names when both are bad.
pub(crate) fn binary(
    heap: &mut Heap,
    op: &'static str,
    a: Value,
    b: Value,
    f: impl FnOnce(&Num, &Num) -> NumResult,
) -> Result<Value, VmError> {
    let x = operand(heap, op, a)?;
    let y = operand(heap, op, b)?;
    let r = f(&x, &y).map_err(|e| num_err(op, e))?;
    Ok(heap.number(r))
}

/// `ADD`: `a + b`.
pub fn add(heap: &mut Heap, a: Value, b: Value) -> Result<Value, VmError> {
    if let (Some(x), Some(y)) = (a.as_fixnum(), b.as_fixnum()) {
        // Two 48-bit operands cannot overflow an i64 addition; `integer` promotes any
        // result past the fixnum boundary to a bignum.
        return Ok(heap.integer(x + y));
    }
    binary(heap, "+", a, b, |x, y| Ok(x.add(y)))
}

/// `SUB`: `a - b`.
pub fn sub(heap: &mut Heap, a: Value, b: Value) -> Result<Value, VmError> {
    if let (Some(x), Some(y)) = (a.as_fixnum(), b.as_fixnum()) {
        return Ok(heap.integer(x - y));
    }
    binary(heap, "-", a, b, |x, y| Ok(x.sub(y)))
}

/// `MUL`: `a * b`.
pub fn mul(heap: &mut Heap, a: Value, b: Value) -> Result<Value, VmError> {
    if let (Some(x), Some(y)) = (a.as_fixnum(), b.as_fixnum()) {
        // A 48×48-bit product can overflow i64; the checked multiply routes that case
        // through the bignum path instead of wrapping (R7RS integers never wrap).
        if let Some(p) = x.checked_mul(y) {
            return Ok(heap.integer(p));
        }
        return Ok(heap.integer_from_big(BigInt::from(x) * BigInt::from(y)));
    }
    binary(heap, "*", a, b, |x, y| Ok(x.mul(y)))
}

/// `DIV`: `a / b`, exact when both operands are (`(/ 1 3)` is `1/3`).
pub fn div(heap: &mut Heap, a: Value, b: Value) -> Result<Value, VmError> {
    if let (Some(x), Some(y)) = (a.as_fixnum(), b.as_fixnum())
        && y != 0
        && x % y == 0
    {
        // `FIXNUM_MIN / -1` is one past FIXNUM_MAX; `integer` promotes it.
        return Ok(heap.integer(x / y));
    }
    binary(heap, "/", a, b, Num::div)
}

/// `QUOT`: `truncate-quotient` of `a` and `b`.
pub fn quot(heap: &mut Heap, a: Value, b: Value) -> Result<Value, VmError> {
    if let (Some(x), Some(y)) = (a.as_fixnum(), b.as_fixnum())
        && y != 0
    {
        // As in `div`, the one quotient that leaves the fixnum range is promoted.
        return Ok(heap.integer(x / y));
    }
    binary(heap, "quotient", a, b, Num::quotient)
}

/// `NEG`: `-a`.
pub fn neg(heap: &mut Heap, a: Value) -> Result<Value, VmError> {
    if let Some(n) = a.as_fixnum() {
        // `-FIXNUM_MIN` exceeds FIXNUM_MAX but fits an i64; `integer` promotes it.
        return Ok(heap.integer(-n));
    }
    unary(heap, "-", a, |x| Ok(x.neg()))
}

/// `ADDI`: `a + imm`, the loop-counter fast path.
pub fn addi(heap: &mut Heap, a: Value, imm: i8) -> Result<Value, VmError> {
    if let Some(n) = a.as_fixnum() {
        return Ok(heap.integer(n + i64::from(imm)));
    }
    unary(heap, "+", a, |x| Ok(x.add(&Num::int(imm))))
}

/// Orders two operands for a relational comparison. `None` means a NaN is involved, which
/// makes every relation false.
fn compare(heap: &Heap, op: &'static str, a: Value, b: Value) -> Result<Option<Ordering>, VmError> {
    let x = operand(heap, op, a)?;
    let y = operand(heap, op, b)?;
    x.cmp(&y).map_err(|e| num_err(op, e))
}

/// `NUMEQ`: numeric `=` across representations.
pub fn num_eq(heap: &Heap, a: Value, b: Value) -> Result<bool, VmError> {
    if let (Some(x), Some(y)) = (a.as_fixnum(), b.as_fixnum()) {
        return Ok(x == y);
    }
    // `=` is the one comparison complex numbers take part in, so it bypasses `compare`.
    let x = operand(heap, "=", a)?;
    let y = operand(heap, "=", b)?;
    Ok(x.num_eq(&y))
}

/// `NUMLT`: numeric `<`.
pub fn num_lt(heap: &Heap, a: Value, b: Value) -> Result<bool, VmError> {
    if let (Some(x), Some(y)) = (a.as_fixnum(), b.as_fixnum()) {
        return Ok(x < y);
    }
    Ok(compare(heap, "<", a, b)? == Some(Ordering::Less))
}

/// `NUMLE`: numeric `<=`.
pub fn num_le(heap: &Heap, a: Value, b: Value) -> Result<bool, VmError> {
    if let (Some(x), Some(y)) = (a.as_fixnum(), b.as_fixnum()) {
        return Ok(x <= y);
    }
    Ok(matches!(
        compare(heap, "<=", a, b)?,
        Some(Ordering::Less | Ordering::Equal)
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::{FIXNUM_MAX, FIXNUM_MIN};
    use crate::vm::error::VmErrorKind;

    fn fix(n: i64) -> Value {
        Value::fixnum(n).unwrap()
    }

    #[test]
    fn fixnum_arithmetic_stays_immediate() {
        let mut heap = Heap::new();
        assert_eq!(add(&mut heap, fix(2), fix(3)).unwrap(), fix(5));
        assert_eq!(sub(&mut heap, fix(2), fix(3)).unwrap(), fix(-1));
        assert_eq!(mul(&mut heap, fix(7), fix(-6)).unwrap(), fix(-42));
        assert_eq!(neg(&mut heap, fix(9)).unwrap(), fix(-9));
        assert_eq!(addi(&mut heap, fix(41), 1).unwrap(), fix(42));
        assert_eq!(addi(&mut heap, fix(0), -128).unwrap(), fix(-128));
        assert_eq!(heap.live_objects(), 0, "no allocation on the fast path");
    }

    #[test]
    fn overflow_promotes_to_bignum_instead_of_wrapping() {
        let mut heap = Heap::new();

        let top = fix(FIXNUM_MAX);
        let sum = add(&mut heap, top, fix(1)).unwrap();
        assert!(sum.is_heap(), "FIXNUM_MAX + 1 must be a bignum");
        assert_eq!(heap.integer_to_i64(sum), Some(FIXNUM_MAX + 1));

        // A product that overflows i64 itself, not just the fixnum range.
        let big = mul(&mut heap, top, top).unwrap();
        assert!(big.is_heap());
        let expected = BigInt::from(FIXNUM_MAX) * BigInt::from(FIXNUM_MAX);
        assert_eq!(heap.get::<Bignum>(big).unwrap().value, expected);

        let bottom = neg(&mut heap, fix(FIXNUM_MIN)).unwrap();
        assert!(bottom.is_heap(), "-FIXNUM_MIN is one past FIXNUM_MAX");
        assert_eq!(heap.integer_to_i64(bottom), Some(-FIXNUM_MIN));
    }

    #[test]
    fn bignum_results_demote_when_they_fit() {
        let mut heap = Heap::new();
        let big = heap.integer(FIXNUM_MAX + 1);
        let back = sub(&mut heap, big, fix(1)).unwrap();
        assert_eq!(back, fix(FIXNUM_MAX), "results at the boundary demote");
    }

    #[test]
    fn inexactness_is_contagious() {
        let mut heap = Heap::new();
        let x = add(&mut heap, fix(1), Value::flonum(0.5)).unwrap();
        assert_eq!(x.as_flonum(), Some(1.5));

        let big = heap.integer(FIXNUM_MAX + 1);
        let y = mul(&mut heap, big, Value::flonum(2.0)).unwrap();
        assert_eq!(y.as_flonum(), Some((FIXNUM_MAX + 1) as f64 * 2.0));

        assert_eq!(
            addi(&mut heap, Value::flonum(1.25), 2).unwrap().as_flonum(),
            Some(3.25)
        );
    }

    #[test]
    fn comparisons_cross_representations() {
        let mut heap = Heap::new();
        let big = heap.integer(FIXNUM_MAX + 1);

        assert!(num_lt(&heap, fix(FIXNUM_MAX), big).unwrap());
        assert!(!num_lt(&heap, big, big).unwrap());
        assert!(num_le(&heap, big, big).unwrap());
        assert!(num_eq(&heap, fix(1), Value::flonum(1.0)).unwrap());
        assert!(num_lt(&heap, Value::flonum(0.5), fix(1)).unwrap());
    }

    /// The suite's 2⁵³ trap: 9007199254740993 has no f64 representation, and rounding
    /// it for the comparison would call it equal to 9007199254740992.0.
    #[test]
    fn mixed_exactness_comparisons_do_not_round_through_f64() {
        let mut heap = Heap::new();
        let over = heap.integer(9_007_199_254_740_993); // 2^53 + 1, exactly
        let at = Value::flonum(9_007_199_254_740_992.0); // 2^53

        assert!(!num_eq(&heap, at, over).unwrap());
        assert!(!num_eq(&heap, over, at).unwrap());
        assert!(num_lt(&heap, at, over).unwrap());
        assert!(!num_lt(&heap, over, at).unwrap());
        assert!(num_le(&heap, at, over).unwrap());

        // An exactly representable value still compares equal across exactness.
        let exact = heap.integer(9_007_199_254_740_992);
        assert!(num_eq(&heap, at, exact).unwrap());
        assert!(num_le(&heap, exact, at).unwrap());
    }

    #[test]
    fn fractional_and_infinite_flonums_compare_exactly_against_integers() {
        let mut heap = Heap::new();
        let two = fix(2);
        let neg_two = fix(-2);

        assert!(num_lt(&heap, two, Value::flonum(2.5)).unwrap());
        assert!(!num_lt(&heap, Value::flonum(2.5), two).unwrap());
        assert!(num_lt(&heap, Value::flonum(-2.5), neg_two).unwrap());
        assert!(!num_lt(&heap, neg_two, Value::flonum(-2.5)).unwrap());
        assert!(num_le(&heap, Value::flonum(2.0), two).unwrap());
        assert!(!num_eq(&heap, two, Value::flonum(2.5)).unwrap());
        assert!(num_eq(&heap, fix(0), Value::flonum(-0.0)).unwrap());

        let big = heap.integer(FIXNUM_MAX + 1);
        assert!(num_lt(&heap, big, Value::flonum(f64::INFINITY)).unwrap());
        assert!(num_lt(&heap, Value::flonum(f64::NEG_INFINITY), big).unwrap());
        assert!(!num_lt(&heap, Value::flonum(f64::INFINITY), big).unwrap());
        assert!(!num_eq(&heap, big, Value::flonum(f64::INFINITY)).unwrap());
    }

    #[test]
    fn nan_compares_false_under_every_predicate() {
        let heap = Heap::new();
        let nan = Value::flonum(f64::NAN);
        assert!(!num_eq(&heap, nan, nan).unwrap());
        assert!(!num_lt(&heap, nan, Value::flonum(1.0)).unwrap());
        assert!(!num_le(&heap, nan, nan).unwrap());
    }

    #[test]
    fn non_numbers_are_refused_with_the_operation_name() {
        let mut heap = Heap::new();
        let s = heap.string("three");
        let err = add(&mut heap, fix(1), s).unwrap_err();
        assert_eq!(
            err.kind,
            VmErrorKind::WrongType {
                op: "+",
                expected: "a number",
                got: "string"
            }
        );
        assert!(num_lt(&heap, Value::TRUE, fix(1)).is_err());
    }

    #[test]
    fn is_number_covers_every_numeric_representation() {
        let mut heap = Heap::new();
        let big = heap.integer(FIXNUM_MAX + 1);
        let half = heap.number(Num::rational(1.into(), 2.into()).unwrap());
        let i = heap.number(Num::rectangular(&Num::int(0), &Num::int(1)).unwrap());
        assert!(is_number(&heap, fix(0)));
        assert!(is_number(&heap, Value::flonum(f64::NAN)));
        assert!(is_number(&heap, big));
        assert!(is_number(&heap, half));
        assert!(is_number(&heap, i));
        let s = heap.string("no");
        assert!(!is_number(&heap, s));
        assert!(!is_number(&heap, Value::TRUE));
    }

    #[test]
    fn div_is_exact_and_quot_truncates() {
        let mut heap = Heap::new();
        let v = div(&mut heap, fix(1), fix(3)).unwrap();
        assert_eq!(
            unbox(&heap, v),
            Some(Num::rational(1.into(), 3.into()).unwrap())
        );
        assert_eq!(div(&mut heap, fix(6), fix(3)).unwrap(), fix(2));
        assert_eq!(quot(&mut heap, fix(-13), fix(4)).unwrap(), fix(-3));
        assert!(matches!(
            div(&mut heap, fix(1), fix(0)).unwrap_err().kind,
            VmErrorKind::DivisionByZero { op: "/" }
        ));
        assert!(matches!(
            quot(&mut heap, fix(5), fix(0)).unwrap_err().kind,
            VmErrorKind::DivisionByZero { op: "quotient" }
        ));
        assert!(matches!(
            quot(&mut heap, Value::flonum(5.5), fix(2))
                .unwrap_err()
                .kind,
            VmErrorKind::NumericDomain {
                op: "quotient",
                detail: "expected an integer"
            }
        ));
        assert!(matches!(
            div(&mut heap, Value::TRUE, fix(1)).unwrap_err().kind,
            VmErrorKind::WrongType { op: "/", .. }
        ));
    }

    #[test]
    fn fixnum_edges_promote_through_the_fast_paths() {
        let mut heap = Heap::new();
        let min = fix(FIXNUM_MIN);
        for v in [
            quot(&mut heap, min, fix(-1)).unwrap(),
            neg(&mut heap, min).unwrap(),
            sub(&mut heap, fix(0), min).unwrap(),
        ] {
            assert_eq!(
                unbox(&heap, v),
                Some(Num::int(-(FIXNUM_MIN as i128) as i64))
            );
            assert_eq!(heap.tag_of(v), Some(HeapTag::Bignum));
        }
        // Two bignum allocations are never bitwise-equal Values; compare the numbers.
        let d = div(&mut heap, min, fix(-1)).unwrap();
        assert_eq!(
            unbox(&heap, d),
            Some(Num::int(-(FIXNUM_MIN as i128) as i64))
        );
    }

    #[test]
    fn tower_values_flow_through_the_opcode_functions() {
        let mut heap = Heap::new();
        let half = heap.number(Num::rational(1.into(), 2.into()).unwrap());
        assert_eq!(add(&mut heap, half, half).unwrap(), fix(1));
        assert_eq!(
            mul(&mut heap, half, Value::flonum(4.0)).unwrap(),
            Value::flonum(2.0)
        );
        assert!(num_lt(&heap, half, Value::flonum(0.6)).unwrap());
        let i = heap.number(Num::rectangular(&Num::int(0), &Num::int(1)).unwrap());
        assert_eq!(mul(&mut heap, i, i).unwrap(), fix(-1));
        assert!(matches!(
            num_lt(&heap, i, fix(1)).unwrap_err().kind,
            VmErrorKind::NumericDomain {
                op: "<",
                detail: "expected a real number"
            }
        ));
    }
}

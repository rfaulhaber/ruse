//! Arithmetic over the M3 numeric slice: fixnums, bignums, and flonums.
//!
//! This is deliberately a miniature of the tower, not the tower: rationals, complex
//! numbers, exactness conversion and the full comparison contagion rules are M5's, and
//! they replace the *insides* of these functions — the signatures are the seam the
//! dispatch loop and the natives are written against. Two rules already hold for good:
//! fixnum results always come back through [`Heap::integer`]/[`Heap::integer_from_big`]
//! (so the fixnum/bignum boundary has exactly one owner), and mixing an inexact operand
//! into an exact operation makes the result inexact.
//!
//! Comparisons across exactness are *exact*: an exact integer never rounds through
//! `f64` on its way into `=`/`<`/`<=`, so the 2⁵³ boundary cannot misjudge — the
//! conformance suite tests exactly that. (Arithmetic, by contrast, is contagiously
//! inexact, which is what R7RS asks for.)

use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;

use crate::gc::Heap;
use crate::rt::wrong_type;
use crate::value::Value;
use crate::value::layout::HeapTag;
use crate::value::object::Bignum;
use crate::vm::error::VmError;

/// A number unboxed for one operation.
enum Num {
    Exact(Exact),
    Flo(f64),
}

enum Exact {
    Fix(i64),
    Big(BigInt),
}

impl Exact {
    fn into_big(self) -> BigInt {
        match self {
            Exact::Fix(n) => BigInt::from(n),
            Exact::Big(b) => b,
        }
    }
}

impl Num {
    fn to_f64(&self) -> f64 {
        match self {
            Num::Exact(Exact::Fix(n)) => *n as f64,
            Num::Flo(x) => *x,
            // `to_f64` fails only outside f64's finite range, where the IEEE answer —
            // and M5's exact→inexact conversion — is the signed infinity.
            Num::Exact(Exact::Big(b)) => b.to_f64().unwrap_or(if b.sign() == Sign::Minus {
                f64::NEG_INFINITY
            } else {
                f64::INFINITY
            }),
        }
    }
}

fn classify(heap: &Heap, v: Value, op: &'static str) -> Result<Num, VmError> {
    if let Some(n) = v.as_fixnum() {
        return Ok(Num::Exact(Exact::Fix(n)));
    }
    if let Some(x) = v.as_flonum() {
        return Ok(Num::Flo(x));
    }
    if let Some(b) = heap.get::<Bignum>(v) {
        return Ok(Num::Exact(Exact::Big(b.value.clone())));
    }
    Err(wrong_type(heap, op, "a number", v))
}

/// Whether `v` is a number in the M3 slice.
pub fn is_number(heap: &Heap, v: Value) -> bool {
    v.is_fixnum() || v.is_flonum() || heap.tag_of(v) == Some(HeapTag::Bignum)
}

fn binop(
    heap: &mut Heap,
    op: &'static str,
    a: Value,
    b: Value,
    big: fn(BigInt, BigInt) -> BigInt,
    flo: fn(f64, f64) -> f64,
) -> Result<Value, VmError> {
    let na = classify(heap, a, op)?;
    let nb = classify(heap, b, op)?;
    match (na, nb) {
        (Num::Exact(x), Num::Exact(y)) => {
            Ok(heap.integer_from_big(big(x.into_big(), y.into_big())))
        }
        (na, nb) => Ok(Value::flonum(flo(na.to_f64(), nb.to_f64()))),
    }
}

/// `ADD`: `a + b`.
pub fn add(heap: &mut Heap, a: Value, b: Value) -> Result<Value, VmError> {
    if let (Some(x), Some(y)) = (a.as_fixnum(), b.as_fixnum()) {
        // Two 48-bit operands cannot overflow an i64 addition; `integer` promotes any
        // result past the fixnum boundary to a bignum.
        return Ok(heap.integer(x + y));
    }
    binop(heap, "+", a, b, |x, y| x + y, |x, y| x + y)
}

/// `SUB`: `a - b`.
pub fn sub(heap: &mut Heap, a: Value, b: Value) -> Result<Value, VmError> {
    if let (Some(x), Some(y)) = (a.as_fixnum(), b.as_fixnum()) {
        return Ok(heap.integer(x - y));
    }
    binop(heap, "-", a, b, |x, y| x - y, |x, y| x - y)
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
    binop(heap, "*", a, b, |x, y| x * y, |x, y| x * y)
}

/// `NEG`: `-a`.
pub fn neg(heap: &mut Heap, a: Value) -> Result<Value, VmError> {
    if let Some(n) = a.as_fixnum() {
        // `-FIXNUM_MIN` exceeds FIXNUM_MAX but fits an i64; `integer` promotes it.
        return Ok(heap.integer(-n));
    }
    match classify(heap, a, "-")? {
        Num::Exact(x) => Ok(heap.integer_from_big(-x.into_big())),
        Num::Flo(x) => Ok(Value::flonum(-x)),
    }
}

/// `ADDI`: `a + imm`, the loop-counter fast path.
pub fn addi(heap: &mut Heap, a: Value, imm: i8) -> Result<Value, VmError> {
    if let Some(n) = a.as_fixnum() {
        return Ok(heap.integer(n + i64::from(imm)));
    }
    match classify(heap, a, "+")? {
        Num::Exact(x) => Ok(heap.integer_from_big(x.into_big() + imm)),
        Num::Flo(x) => Ok(Value::flonum(x + f64::from(imm))),
    }
}

/// Which comparison is being asked. One enum instead of function pointers because the
/// mixed exact/flonum path needs to know the relation to answer it exactly.
#[derive(Clone, Copy)]
enum Rel {
    Eq,
    Lt,
    Le,
}

fn rel_i64(rel: Rel, x: i64, y: i64) -> bool {
    match rel {
        Rel::Eq => x == y,
        Rel::Lt => x < y,
        Rel::Le => x <= y,
    }
}

fn rel_big(rel: Rel, x: &BigInt, y: &BigInt) -> bool {
    match rel {
        Rel::Eq => x == y,
        Rel::Lt => x < y,
        Rel::Le => x <= y,
    }
}

fn rel_f64(rel: Rel, x: f64, y: f64) -> bool {
    // NaN answers false under all three, which is what the operators already say.
    match rel {
        Rel::Eq => x == y,
        Rel::Lt => x < y,
        Rel::Le => x <= y,
    }
}

/// `exact rel flonum`, answered *exactly*: converting the exact side to `f64` would
/// misjudge integers past 2⁵³ — `(= 9007199254740992.0 9007199254740993)` must be `#f` —
/// so instead the flonum is split into its (exactly representable) integral part and its
/// fraction, and the integral parts compare as bignums.
fn rel_exact_flo(rel: Rel, e: &BigInt, f: f64) -> bool {
    if f.is_nan() {
        return false;
    }
    if f.is_infinite() {
        // Every exact integer is below +inf and above -inf, and equal to neither.
        return match rel {
            Rel::Eq => false,
            Rel::Lt | Rel::Le => f > 0.0,
        };
    }
    // A finite f64's truncation is an integer f64 represents exactly, so `from_f64`
    // cannot fail here; `false` is the safe answer if that reasoning ever breaks.
    let Some(t) = num_traits::FromPrimitive::from_f64(f.trunc()) else {
        return false;
    };
    let t: BigInt = t;
    let fract = f.fract();
    match rel {
        Rel::Eq => fract == 0.0 && *e == t,
        // e < t + fract, with fract strictly inside (-1, 1) and sharing t's sign.
        Rel::Lt => *e < t || (*e == t && fract > 0.0),
        Rel::Le => *e < t || (*e == t && fract >= 0.0),
    }
}

/// `flonum rel exact`, by the same splitting.
fn rel_flo_exact(rel: Rel, f: f64, e: &BigInt) -> bool {
    if f.is_nan() {
        return false;
    }
    if f.is_infinite() {
        return match rel {
            Rel::Eq => false,
            Rel::Lt | Rel::Le => f < 0.0,
        };
    }
    let Some(t) = num_traits::FromPrimitive::from_f64(f.trunc()) else {
        return false;
    };
    let t: BigInt = t;
    let fract = f.fract();
    match rel {
        Rel::Eq => fract == 0.0 && t == *e,
        Rel::Lt => t < *e || (t == *e && fract < 0.0),
        Rel::Le => t < *e || (t == *e && fract <= 0.0),
    }
}

fn compare(heap: &Heap, op: &'static str, rel: Rel, a: Value, b: Value) -> Result<bool, VmError> {
    let na = classify(heap, a, op)?;
    let nb = classify(heap, b, op)?;
    Ok(match (na, nb) {
        (Num::Exact(x), Num::Exact(y)) => rel_big(rel, &x.into_big(), &y.into_big()),
        (Num::Flo(x), Num::Flo(y)) => rel_f64(rel, x, y),
        (Num::Exact(x), Num::Flo(y)) => rel_exact_flo(rel, &x.into_big(), y),
        (Num::Flo(x), Num::Exact(y)) => rel_flo_exact(rel, x, &y.into_big()),
    })
}

/// `NUMEQ`: numeric `=` across representations.
pub fn num_eq(heap: &Heap, a: Value, b: Value) -> Result<bool, VmError> {
    if let (Some(x), Some(y)) = (a.as_fixnum(), b.as_fixnum()) {
        return Ok(rel_i64(Rel::Eq, x, y));
    }
    compare(heap, "=", Rel::Eq, a, b)
}

/// `NUMLT`: numeric `<`.
pub fn num_lt(heap: &Heap, a: Value, b: Value) -> Result<bool, VmError> {
    if let (Some(x), Some(y)) = (a.as_fixnum(), b.as_fixnum()) {
        return Ok(rel_i64(Rel::Lt, x, y));
    }
    compare(heap, "<", Rel::Lt, a, b)
}

/// `NUMLE`: numeric `<=`.
pub fn num_le(heap: &Heap, a: Value, b: Value) -> Result<bool, VmError> {
    if let (Some(x), Some(y)) = (a.as_fixnum(), b.as_fixnum()) {
        return Ok(rel_i64(Rel::Le, x, y));
    }
    compare(heap, "<=", Rel::Le, a, b)
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
    fn is_number_covers_the_m3_slice() {
        let mut heap = Heap::new();
        let big = heap.integer(FIXNUM_MAX + 1);
        assert!(is_number(&heap, fix(0)));
        assert!(is_number(&heap, Value::flonum(f64::NAN)));
        assert!(is_number(&heap, big));
        let s = heap.string("no");
        assert!(!is_number(&heap, s));
        assert!(!is_number(&heap, Value::TRUE));
    }
}

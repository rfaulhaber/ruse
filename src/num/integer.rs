//! The integer-flavoured operations on [`Num`]: integer division, rounding, `gcd`/`lcm`,
//! `numerator`/`denominator`, `rationalize` and the exact integer square root.
//!
//! An inexact integral operand is converted exactly, the arithmetic runs on `BigInt`, and
//! only the result is rounded back to a flonum, so `(remainder -13 -4.0)` is `-1.0`.

use std::cmp::Ordering;

use num_integer::Integer;
use num_traits::{Signed, Zero};

use super::{BigInt, BigRational, Num, NumError, NumResult};

impl Num {
    /// `truncate-quotient`.
    pub fn quotient(&self, o: &Num) -> NumResult {
        self.truncate_div(o).map(|(q, _)| q)
    }

    /// `truncate-remainder`.
    pub fn remainder(&self, o: &Num) -> NumResult {
        self.truncate_div(o).map(|(_, r)| r)
    }

    /// `modulo`, which R7RS defines as `floor-remainder`.
    pub fn modulo(&self, o: &Num) -> NumResult {
        self.floor_remainder(o)
    }

    pub fn floor_quotient(&self, o: &Num) -> NumResult {
        self.floor_div(o).map(|(q, _)| q)
    }

    pub fn floor_remainder(&self, o: &Num) -> NumResult {
        self.floor_div(o).map(|(_, r)| r)
    }

    /// `truncate/`: the quotient and remainder, rounding the quotient toward zero.
    pub fn truncate_div(&self, o: &Num) -> Result<(Num, Num), NumError> {
        let (a, b, inexact) = division_operands(self, o)?;
        let (q, r) = a.div_rem(&b);
        Ok((integer_result(q, inexact), integer_result(r, inexact)))
    }

    /// `floor/`: the quotient and remainder, rounding the quotient toward negative infinity.
    pub fn floor_div(&self, o: &Num) -> Result<(Num, Num), NumError> {
        let (a, b, inexact) = division_operands(self, o)?;
        let (q, r) = a.div_mod_floor(&b);
        Ok((integer_result(q, inexact), integer_result(r, inexact)))
    }

    /// Never negative, and `(gcd 0 0)` is `0`.
    pub fn gcd(&self, o: &Num) -> NumResult {
        let (a, b, inexact) = integer_operands(self, o)?;
        Ok(integer_result(a.gcd(&b), inexact))
    }

    /// Never negative, and zero when either operand is.
    pub fn lcm(&self, o: &Num) -> NumResult {
        let (a, b, inexact) = integer_operands(self, o)?;
        Ok(integer_result(a.lcm(&b), inexact))
    }

    pub fn numerator(&self) -> NumResult {
        let ratio = self.exact_ratio()?;
        Ok(integer_result(ratio.numer().clone(), !self.is_exact()))
    }

    pub fn denominator(&self) -> NumResult {
        let ratio = self.exact_ratio()?;
        Ok(integer_result(ratio.denom().clone(), !self.is_exact()))
    }

    pub fn floor(&self) -> NumResult {
        self.to_integral(|r| r.floor().to_integer(), f64::floor)
    }

    pub fn ceiling(&self) -> NumResult {
        self.to_integral(|r| r.ceil().to_integer(), f64::ceil)
    }

    pub fn truncate(&self) -> NumResult {
        self.to_integral(|r| r.trunc().to_integer(), f64::trunc)
    }

    /// Rounds to the nearest integer, and a tie to the even one.
    pub fn round(&self) -> NumResult {
        self.to_integral(round_half_even, f64::round_ties_even)
    }

    /// The simplest rational within `tolerance` of `self`. The result is inexact when
    /// either argument is; the arithmetic is exact either way.
    pub fn rationalize(&self, tolerance: &Num) -> NumResult {
        if !self.is_real() || !tolerance.is_real() {
            return Err(NumError::NotReal);
        }
        if self.is_nan() || tolerance.is_nan() {
            return Ok(Num::flo(f64::NAN));
        }
        if self.is_infinite() {
            return Ok(self.clone());
        }
        // An infinite tolerance is a flonum, so its zero is inexact.
        if tolerance.is_infinite() {
            return Ok(Num::flo(0.0));
        }
        let (x, y) = (self.exact_ratio()?, tolerance.exact_ratio()?);
        let radius = y.abs();
        let found = Num::from_ratio(simplest(&(&x - &radius), &(&x + &radius)));
        if self.is_exact() && tolerance.is_exact() {
            Ok(found)
        } else {
            Ok(found.inexact())
        }
    }

    /// `exact-integer-sqrt`: the floor of the square root and what is left over.
    pub fn exact_integer_sqrt(&self) -> Result<(Num, Num), NumError> {
        let Num::Int(n) = self else {
            return Err(NumError::NotInteger);
        };
        // `BigInt::sqrt` panics on a negative.
        if n.is_negative() {
            return Err(NumError::Domain);
        }
        let root = n.sqrt();
        let rest = n - &root * &root;
        Ok((Num::Int(root), Num::Int(rest)))
    }

    /// The value of an integral number, exact or not, as a `BigInt`.
    fn exact_integer(&self) -> Result<BigInt, NumError> {
        match self.exact() {
            Ok(Num::Int(i)) => Ok(i),
            _ => Err(NumError::NotInteger),
        }
    }

    /// The exact value of a finite real as a ratio.
    fn exact_ratio(&self) -> Result<BigRational, NumError> {
        if !self.is_real() {
            return Err(NumError::NotReal);
        }
        self.exact()?.exact_real().ok_or(NumError::NotReal)
    }

    /// Applies a rounding function to a real: an integer is its own rounding, a ratio
    /// rounds to an exact integer and a flonum to a flonum.
    fn to_integral(&self, ratio: fn(&BigRational) -> BigInt, flonum: fn(f64) -> f64) -> NumResult {
        match self {
            Num::Int(_) => Ok(self.clone()),
            Num::Rat(r) => Ok(Num::Int(ratio(r))),
            Num::Flo(x) => Ok(Num::flo(flonum(*x))),
            Num::ExactComplex(_) | Num::InexactComplex(_) => Err(NumError::NotReal),
        }
    }
}

/// Both operands as exact integers, and whether the answer is inexact.
fn integer_operands(a: &Num, b: &Num) -> Result<(BigInt, BigInt, bool), NumError> {
    Ok((
        a.exact_integer()?,
        b.exact_integer()?,
        !(a.is_exact() && b.is_exact()),
    ))
}

/// As [`integer_operands`] for a division. A zero divisor, exact or not, is rejected here
/// because `Integer::div_rem` panics on it.
fn division_operands(a: &Num, b: &Num) -> Result<(BigInt, BigInt, bool), NumError> {
    let (a, b, inexact) = integer_operands(a, b)?;
    if b.is_zero() {
        Err(NumError::DivisionByZero)
    } else {
        Ok((a, b, inexact))
    }
}

fn integer_result(i: BigInt, inexact: bool) -> Num {
    let n = Num::Int(i);
    if inexact { n.inexact() } else { n }
}

/// `Ratio::round` ties away from zero, which R7RS does not allow.
fn round_half_even(r: &BigRational) -> BigInt {
    let floor = r.floor();
    let fraction = r - &floor;
    let floor = floor.to_integer();
    let up = match fraction.cmp(&BigRational::new(1.into(), 2.into())) {
        Ordering::Less => false,
        Ordering::Greater => true,
        Ordering::Equal => floor.is_odd(),
    };
    if up { floor + 1 } else { floor }
}

/// The simplest rational in the closed interval `[lo, hi]`: the one with the smallest
/// denominator, and of those the smallest in magnitude.
///
/// This is the one hand-written algorithm in the numeric tower, an approved exception to
/// the rule that every numeric operation comes from a crate: no `num-*` crate has it. It
/// is the R4RS/MIT Scheme `simplest-rational`: the endpoints' continued-fraction expansions
/// are followed in step until they part.
fn simplest(lo: &BigRational, hi: &BigRational) -> BigRational {
    if lo.is_positive() {
        simplest_positive(lo.clone(), hi.clone())
    } else if hi.is_negative() {
        -simplest_positive(-hi, -lo)
    } else {
        BigRational::zero()
    }
}

/// [`simplest`] for `0 < lo <= hi`. Each step strips the integer part `fl` of `lo` and
/// recurses on the reciprocals of the fractional parts, so the result is
/// `fl + 1 / simplest_positive(1 / (hi - fl), 1 / (lo - fl))`. The integer parts are kept
/// in a vector and folded back afterwards because the expansion of a large ratio is long
/// enough to overflow the stack if recursed on.
fn simplest_positive(mut lo: BigRational, mut hi: BigRational) -> BigRational {
    let mut stripped = Vec::new();
    let mut innermost = loop {
        let fl = lo.floor().to_integer();
        if lo.is_integer() {
            break lo;
        }
        if fl < hi.floor().to_integer() {
            break BigRational::from_integer(fl + 1);
        }
        // `lo` is not an integer and `hi` shares its integer part, so `fl < lo <= hi` and
        // both differences are positive: neither reciprocal is of zero.
        (lo, hi) = ((hi - &fl).recip(), (lo - &fl).recip());
        stripped.push(fl);
    };
    // Every partial result is positive, so no reciprocal here is of zero either.
    while let Some(fl) = stripped.pop() {
        innermost = innermost.recip() + fl;
    }
    innermost
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integer_division_family() {
        assert_eq!(Num::int(-13).quotient(&Num::int(4)), Ok(Num::int(-3)));
        assert_eq!(Num::int(-13).remainder(&Num::int(4)), Ok(Num::int(-1)));
        assert_eq!(Num::int(-13).modulo(&Num::int(4)), Ok(Num::int(3)));
        assert_eq!(Num::int(-13).remainder(&Num::flo(-4.0)), Ok(Num::flo(-1.0)));
        assert_eq!(
            Num::flo(-5.0).truncate_div(&Num::int(-2)),
            Ok((Num::flo(2.0), Num::flo(-1.0)))
        );
        assert_eq!(
            Num::int(5).floor_div(&Num::int(-2)),
            Ok((Num::int(-3), Num::int(-1)))
        );
        assert_eq!(
            Num::int(5).quotient(&Num::int(0)),
            Err(NumError::DivisionByZero)
        );
        assert_eq!(
            Num::flo(5.0).quotient(&Num::flo(0.0)),
            Err(NumError::DivisionByZero)
        );
        assert_eq!(
            Num::flo(5.5).quotient(&Num::int(2)),
            Err(NumError::NotInteger)
        );
        assert_eq!(
            Num::int(-4611686018427387904i64).quotient(&Num::int(-1)),
            Ok(Num::int(4611686018427387904i64))
        );
        assert_eq!(Num::int(32).gcd(&Num::int(-36)), Ok(Num::int(4)));
        assert_eq!(Num::flo(32.0).lcm(&Num::int(-36)), Ok(Num::flo(288.0)));
        assert_eq!(Num::int(0).gcd(&Num::int(0)), Ok(Num::int(0)));
    }

    #[test]
    fn numerator_denominator_and_rounding() {
        let six_fourths = Num::int(6).div(&Num::int(4)).unwrap();
        assert_eq!(six_fourths.inexact().denominator(), Ok(Num::flo(2.0)));
        assert_eq!(Num::flo(5.5).numerator(), Ok(Num::flo(11.0)));
        assert_eq!(Num::flo(5.0).denominator(), Ok(Num::flo(1.0)));
        let r = |n: i64, d: i64| Num::rational(n.into(), d.into()).unwrap();
        assert_eq!(r(-7, 2).round(), Ok(Num::int(-4)));
        assert_eq!(r(7, 2).round(), Ok(Num::int(4)));
        assert_eq!(r(5, 2).round(), Ok(Num::int(2))); // half to even
        assert_eq!(r(-5, 2).round(), Ok(Num::int(-2)));
        assert_eq!(Num::flo(2.5).round(), Ok(Num::flo(2.0)));
        assert_eq!(Num::flo(-4.3).ceiling(), Ok(Num::flo(-4.0)));
        assert_eq!(r(-7, 2).floor(), Ok(Num::int(-4)));
        assert_eq!(r(-7, 2).truncate(), Ok(Num::int(-3)));
        assert_eq!(
            Num::rectangular(&Num::int(1), &Num::int(1))
                .unwrap()
                .floor(),
            Err(NumError::NotReal)
        );
    }

    #[test]
    fn rationalize_and_exact_integer_sqrt() {
        let r = |n: i64, d: i64| Num::rational(n.into(), d.into()).unwrap();
        assert_eq!(r(3, 10).rationalize(&r(1, 10)), Ok(r(1, 3)));
        assert_eq!(
            Num::flo(0.3).rationalize(&r(1, 10)),
            Ok(Num::flo(1.0 / 3.0))
        );
        assert_eq!(r(-3, 10).rationalize(&r(1, 10)), Ok(r(-1, 3)));
        assert_eq!(Num::int(2).rationalize(&Num::int(1)), Ok(Num::int(1))); // [1, 3] → 1
        assert_eq!(r(5, 12).rationalize(&r(1, 12)), Ok(r(1, 2))); // [1/3, 1/2] → 1/2
        assert_eq!(Num::int(0).rationalize(&r(1, 3)), Ok(Num::int(0)));
        assert_eq!(
            Num::flo(f64::INFINITY).rationalize(&Num::int(3)),
            Ok(Num::flo(f64::INFINITY))
        );
        assert_eq!(
            Num::int(3).rationalize(&Num::flo(f64::INFINITY)),
            Ok(Num::flo(0.0))
        );
        assert_eq!(
            Num::int(17).exact_integer_sqrt(),
            Ok((Num::int(4), Num::int(1)))
        );
        assert_eq!(
            Num::int(BigInt::from(10).pow(40) + 1).exact_integer_sqrt(),
            Ok((Num::int(BigInt::from(10).pow(20)), Num::int(1)))
        );
        assert_eq!(Num::int(-1).exact_integer_sqrt(), Err(NumError::Domain));
        assert_eq!(
            Num::flo(4.0).exact_integer_sqrt(),
            Err(NumError::NotInteger)
        );
    }

    #[test]
    fn integer_family_edges() {
        let r = |n: i64, d: i64| Num::rational(n.into(), d.into()).unwrap();
        // a non-integer is reported before a zero divisor
        assert_eq!(
            Num::flo(5.5).quotient(&Num::int(0)),
            Err(NumError::NotInteger)
        );
        assert_eq!(
            Num::flo(f64::NAN).modulo(&Num::int(2)),
            Err(NumError::NotInteger)
        );
        assert_eq!(Num::int(7).floor_quotient(&Num::int(-2)), Ok(Num::int(-4)));
        assert_eq!(Num::int(7).floor_remainder(&Num::int(-2)), Ok(Num::int(-1)));
        assert_eq!(Num::int(0).lcm(&Num::int(5)), Ok(Num::int(0)));
        assert_eq!(r(3, 2).numerator(), Ok(Num::int(3)));
        assert_eq!(r(-3, 2).denominator(), Ok(Num::int(2)));
        assert_eq!(Num::int(7).denominator(), Ok(Num::int(1)));
        assert_eq!(
            Num::rectangular(&Num::int(1), &Num::int(1))
                .unwrap()
                .numerator(),
            Err(NumError::NotReal)
        );
        assert_eq!(
            Num::flo(f64::INFINITY).numerator(),
            Err(NumError::NotRepresentable)
        );
        assert_eq!(Num::int(5).floor(), Ok(Num::int(5)));
        assert_eq!(r(7, 2).ceiling(), Ok(Num::int(4)));
        assert_eq!(r(-7, 2).ceiling(), Ok(Num::int(-3)));
        assert_eq!(Num::flo(-2.5).round(), Ok(Num::flo(-2.0)));
        assert_eq!(Num::flo(3.5).round(), Ok(Num::flo(4.0)));
        assert_eq!(r(-1, 3).round(), Ok(Num::int(0)));
    }

    #[test]
    fn rationalize_edges() {
        let r = |n: i64, d: i64| Num::rational(n.into(), d.into()).unwrap();
        assert!(
            Num::flo(f64::NAN)
                .rationalize(&Num::int(1))
                .unwrap()
                .is_nan()
        );
        assert!(
            Num::int(1)
                .rationalize(&Num::flo(f64::NAN))
                .unwrap()
                .is_nan()
        );
        assert_eq!(
            Num::rectangular(&Num::int(1), &Num::int(1))
                .unwrap()
                .rationalize(&Num::int(1)),
            Err(NumError::NotReal)
        );
        // a zero tolerance leaves only the number itself, however long its continued
        // fraction is
        for x in [r(355, 113), r(-355, 113), r(1346269, 832040), r(7, 3)] {
            assert_eq!(x.rationalize(&Num::int(0)), Ok(x.clone()));
        }
        // the tolerance's sign is irrelevant
        assert_eq!(r(3, 10).rationalize(&r(-1, 10)), Ok(r(1, 3)));
    }
}

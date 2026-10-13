//! Arithmetic and comparison on [`Num`].
//!
//! Operands are promoted to the least representation that holds both exactly: exact stays
//! exact, any inexact operand makes the result inexact, and an exact real mixed with an
//! exact complex is that real with an imaginary part of zero.

use std::cmp::Ordering;

use num_traits::{Signed, Zero};

use super::{BigInt, BigRational, Complex, Num, NumError, NumResult, int_to_f64, ratio_to_f64};

impl Num {
    /// An exact zero is the additive identity, so the other operand comes back unchanged:
    /// promoting the zero to `0.0` first would turn `(+ -0.0 0)` into `0.0`.
    pub fn add(&self, o: &Num) -> Num {
        if self.is_exact_zero() {
            return o.clone();
        }
        if o.is_exact_zero() {
            return self.clone();
        }
        match Pair::of(self, o) {
            Pair::Int(a, b) => Num::Int(a + b),
            Pair::Rat(a, b) => Num::from_ratio(a + b),
            Pair::Flo(a, b) => Num::flo(a + b),
            Pair::Exact(a, b) => Num::from_exact_complex(a + b),
            Pair::ComplexReal(a, b) => Num::inexact_complex(a + b),
            Pair::RealComplex(a, b) => Num::inexact_complex(a + b),
            Pair::Inexact(a, b) => Num::inexact_complex(a + b),
        }
    }

    /// As for [`Num::add`], an exact zero subtrahend leaves the minuend alone, and an exact
    /// zero minuend is the negated subtrahend.
    pub fn sub(&self, o: &Num) -> Num {
        if o.is_exact_zero() {
            return self.clone();
        }
        if self.is_exact_zero() {
            return o.neg();
        }
        match Pair::of(self, o) {
            Pair::Int(a, b) => Num::Int(a - b),
            Pair::Rat(a, b) => Num::from_ratio(a - b),
            Pair::Flo(a, b) => Num::flo(a - b),
            Pair::Exact(a, b) => Num::from_exact_complex(a - b),
            Pair::ComplexReal(a, b) => Num::inexact_complex(a - b),
            Pair::RealComplex(a, b) => Num::inexact_complex(a - b),
            Pair::Inexact(a, b) => Num::inexact_complex(a - b),
        }
    }

    pub fn mul(&self, o: &Num) -> Num {
        match Pair::of(self, o) {
            Pair::Int(a, b) => Num::Int(a * b),
            Pair::Rat(a, b) => Num::from_ratio(a * b),
            Pair::Flo(a, b) => Num::flo(a * b),
            Pair::Exact(a, b) => Num::from_exact_complex(a * b),
            Pair::ComplexReal(a, b) => Num::inexact_complex(a * b),
            Pair::RealComplex(a, b) => Num::inexact_complex(a * b),
            Pair::Inexact(a, b) => Num::inexact_complex(a * b),
        }
    }

    /// An exact zero divisor is an error whatever the dividend; a flonum zero divides the
    /// IEEE way, so `(/ 1.0 0.0)` is `+inf.0`.
    pub fn div(&self, o: &Num) -> NumResult {
        // Past this check no exact divisor is zero, which the `Ratio` and
        // `Complex<BigRational>` divisions would panic on.
        if o.is_exact() && o.is_zero() {
            return Err(NumError::DivisionByZero);
        }
        match Pair::of(self, o) {
            Pair::Int(a, b) => Num::rational(a.clone(), b.clone()),
            Pair::Rat(a, b) => Ok(Num::from_ratio(a / b)),
            Pair::Flo(a, b) => Ok(Num::flo(a / b)),
            Pair::Exact(a, b) => Ok(Num::from_exact_complex(a / b)),
            Pair::ComplexReal(a, b) => Ok(Num::inexact_complex(a / b)),
            Pair::RealComplex(a, b) => Ok(Num::inexact_complex(a / b)),
            Pair::Inexact(a, b) => Ok(Num::inexact_complex(a / b)),
        }
    }

    pub fn neg(&self) -> Num {
        match self {
            Num::Int(i) => Num::Int(-i),
            Num::Rat(r) => Num::Rat(-r),
            Num::Flo(x) => Num::flo(-x),
            Num::ExactComplex(c) => Num::ExactComplex(-c),
            Num::InexactComplex(c) => Num::inexact_complex(-c),
        }
    }

    fn is_exact_zero(&self) -> bool {
        self.is_exact() && self.is_zero()
    }

    pub fn square(&self) -> Num {
        self.mul(self)
    }

    pub fn abs(&self) -> NumResult {
        match self {
            Num::Int(i) => Ok(Num::Int(i.abs())),
            Num::Rat(r) => Ok(Num::Rat(r.abs())),
            Num::Flo(x) => Ok(Num::flo(x.abs())),
            Num::ExactComplex(_) | Num::InexactComplex(_) => Err(NumError::NotReal),
        }
    }

    /// `=`: the only comparison a complex number takes part in. A NaN part is never equal
    /// to anything, itself included.
    pub fn num_eq(&self, o: &Num) -> bool {
        if self.is_real() && o.is_real() {
            return cmp_real(self, o) == Some(Ordering::Equal);
        }
        let ((re, im), (o_re, o_im)) = (self.parts(), o.parts());
        cmp_real(&re, &o_re) == Some(Ordering::Equal)
            && cmp_real(&im, &o_im) == Some(Ordering::Equal)
    }

    /// Orders two reals exactly, whatever their exactness. `None` means a NaN is involved.
    // `Ord` is deliberately not implemented: the ordering is partial and fallible.
    #[allow(clippy::should_implement_trait)]
    pub fn cmp(&self, o: &Num) -> Result<Option<Ordering>, NumError> {
        if !self.is_real() || !o.is_real() {
            return Err(NumError::NotReal);
        }
        Ok(cmp_real(self, o))
    }

    /// `eqv?`: same exactness and equal, with `-0.0` distinct from `0.0` and a NaN `eqv?`
    /// to itself. Structural equality already says exactly that, because the variant
    /// carries the exactness and flonums compare by bits.
    pub fn eqv(&self, o: &Num) -> bool {
        self == o
    }

    /// The larger of two reals; inexact if either operand is, and a NaN operand yields NaN.
    pub fn max(&self, o: &Num) -> NumResult {
        self.extremum(o, Ordering::Greater)
    }

    /// The smaller of two reals; inexact if either operand is, and a NaN operand yields NaN.
    pub fn min(&self, o: &Num) -> NumResult {
        self.extremum(o, Ordering::Less)
    }

    fn extremum(&self, o: &Num, wanted: Ordering) -> NumResult {
        let chosen = match self.cmp(o)? {
            None => return Ok(Num::flo(f64::NAN)),
            Some(ord) if ord == wanted => self,
            Some(_) => o,
        };
        if self.is_exact() && o.is_exact() {
            Ok(chosen.clone())
        } else {
            Ok(chosen.inexact())
        }
    }

    /// The real and imaginary parts, a real number having an exact-zero imaginary part.
    pub(super) fn parts(&self) -> (Num, Num) {
        match self {
            Num::ExactComplex(c) => (Num::from_ratio(c.re.clone()), Num::from_ratio(c.im.clone())),
            Num::InexactComplex(c) => (Num::flo(c.re), Num::flo(c.im)),
            real => (real.clone(), Num::int(0)),
        }
    }

    /// Collapses an exact-zero imaginary part, so `(1+2i)(1-2i)` is the integer `5`.
    fn from_exact_complex(c: Complex<BigRational>) -> Num {
        Num::exact_rectangular(c.re, c.im)
    }

    /// The value as a complex of ratios; `None` for anything inexact.
    fn to_exact_complex(&self) -> Option<Complex<BigRational>> {
        match self {
            Num::ExactComplex(c) => Some(c.clone()),
            _ => self
                .exact_real()
                .map(|re| Complex::new(re, BigRational::zero())),
        }
    }

    pub(super) fn to_inexact_complex(&self) -> Complex<f64> {
        match self {
            Num::Int(i) => Complex::new(int_to_f64(i), 0.0),
            Num::Rat(r) => Complex::new(ratio_to_f64(r), 0.0),
            Num::Flo(x) => Complex::new(*x, 0.0),
            Num::ExactComplex(c) => Complex::new(ratio_to_f64(&c.re), ratio_to_f64(&c.im)),
            Num::InexactComplex(c) => *c,
        }
    }
}

/// Two operands promoted to a common representation.
enum Pair<'a> {
    Int(&'a BigInt, &'a BigInt),
    Rat(BigRational, BigRational),
    Flo(f64, f64),
    Exact(Complex<BigRational>, Complex<BigRational>),
    /// An inexact result from a complex and a real, in operand order. The real stays a
    /// scalar: lifting it to `x+0i` would turn an infinite part into NaN (`inf * 0`) and a
    /// `-0.0` imaginary part into `+0.0`.
    ComplexReal(Complex<f64>, f64),
    RealComplex(f64, Complex<f64>),
    Inexact(Complex<f64>, Complex<f64>),
}

impl<'a> Pair<'a> {
    fn of(a: &'a Num, b: &'a Num) -> Pair<'a> {
        if let (Num::Int(x), Num::Int(y)) = (a, b) {
            return Pair::Int(x, y);
        }
        if let (Some(x), Some(y)) = (a.exact_real(), b.exact_real()) {
            return Pair::Rat(x, y);
        }
        if let (Some(x), Some(y)) = (a.to_f64(), b.to_f64()) {
            return Pair::Flo(x, y);
        }
        if let (Some(x), Some(y)) = (a.to_exact_complex(), b.to_exact_complex()) {
            return Pair::Exact(x, y);
        }
        match (a.to_f64(), b.to_f64()) {
            (None, Some(y)) => Pair::ComplexReal(a.to_inexact_complex(), y),
            (Some(x), None) => Pair::RealComplex(x, b.to_inexact_complex()),
            _ => Pair::Inexact(a.to_inexact_complex(), b.to_inexact_complex()),
        }
    }
}

/// Orders two reals; `None` when a NaN is involved or an operand is not real.
fn cmp_real(a: &Num, b: &Num) -> Option<Ordering> {
    match (a, b) {
        (Num::Int(x), Num::Int(y)) => Some(x.cmp(y)),
        (Num::Flo(x), Num::Flo(y)) => x.partial_cmp(y),
        (Num::Flo(x), exact) => cmp_flo_exact(*x, &exact.exact_real()?),
        (exact, Num::Flo(y)) => cmp_flo_exact(*y, &exact.exact_real()?).map(Ordering::reverse),
        _ => Some(a.exact_real()?.cmp(&b.exact_real()?)),
    }
}

/// Compares a flonum with an exact value without rounding the exact side to `f64`.
fn cmp_flo_exact(x: f64, exact: &BigRational) -> Option<Ordering> {
    if x.is_nan() {
        None
    } else if x.is_infinite() {
        Some(if x > 0.0 {
            Ordering::Greater
        } else {
            Ordering::Less
        })
    } else {
        BigRational::from_float(x).map(|x| x.cmp(exact))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn contagion_and_exact_division() {
        let third = Num::rational(1.into(), 3.into()).unwrap();
        assert_eq!(Num::int(1).div(&Num::int(3)), Ok(third.clone()));
        assert_eq!(Num::int(6).div(&Num::int(3)), Ok(Num::int(2)));
        assert_eq!(Num::int(1).div(&Num::int(0)), Err(NumError::DivisionByZero));
        assert_eq!(
            Num::flo(1.0).div(&Num::int(0)),
            Err(NumError::DivisionByZero)
        );
        assert_eq!(
            Num::flo(1.0).div(&Num::flo(0.0)),
            Ok(Num::flo(f64::INFINITY))
        );
        assert!(Num::int(0).div(&Num::flo(0.0)).unwrap().is_nan());
        assert_eq!(third.add(&Num::flo(0.5)), Num::flo(1.0 / 3.0 + 0.5));
        assert_eq!(Num::int(2).mul(&Num::flo(3.5)), Num::flo(7.0));
        assert!(matches!(Num::flo(-0.0).neg(), Num::Flo(x) if x == 0.0 && x.is_sign_positive()));
        // An exact zero is the additive identity and carries no sign of its own.
        let negative_zero = |n: Num| matches!(n, Num::Flo(x) if x == 0.0 && x.is_sign_negative());
        assert!(negative_zero(Num::flo(-0.0).add(&Num::int(0))));
        assert!(negative_zero(Num::int(0).add(&Num::flo(-0.0))));
        assert!(negative_zero(Num::flo(-0.0).sub(&Num::int(0))));
        assert!(negative_zero(Num::int(0).sub(&Num::flo(0.0))));
        assert_eq!(
            Num::int(0).sub(&third),
            Num::rational((-1).into(), 3.into()).unwrap()
        );
        assert!(matches!(Num::int(-1).mul(&Num::flo(0.0)), Num::Flo(x) if x.is_sign_negative()));
        let big = Num::int(BigInt::from(2).pow(100));
        assert_eq!(big.sub(&big), Num::int(0));
        let z = Num::rectangular(&Num::int(1), &Num::int(2)).unwrap();
        assert_eq!(
            z.mul(&z),
            Num::rectangular(&Num::int(-3), &Num::int(4)).unwrap()
        );
        assert_eq!(
            z.mul(&Num::rectangular(&Num::int(1), &Num::int(-2)).unwrap()),
            Num::int(5)
        ); // collapses to real
        assert_eq!(z.div(&Num::int(0)), Err(NumError::DivisionByZero));
        assert_eq!(z.abs(), Err(NumError::NotReal));
        assert_eq!(Num::flo(-2.5).abs(), Ok(Num::flo(2.5)));
    }

    #[test]
    fn comparisons_are_exact_across_exactness() {
        use std::cmp::Ordering::*;
        let two53 = Num::int(9007199254740993i64);
        assert!(!Num::flo(9007199254740992.0).num_eq(&two53));
        assert_eq!(Num::flo(9007199254740992.0).cmp(&two53), Ok(Some(Less)));
        assert_eq!(
            Num::flo(f64::INFINITY).cmp(&Num::int(BigInt::from(10).pow(400))),
            Ok(Some(Greater))
        );
        assert_eq!(Num::flo(f64::NAN).cmp(&Num::int(0)), Ok(None));
        assert!(!Num::flo(f64::NAN).num_eq(&Num::flo(f64::NAN)));
        assert!(Num::int(1).num_eq(&Num::rectangular(&Num::flo(1.0), &Num::flo(0.0)).unwrap()));
        assert!(!Num::flo(1.0).num_eq(&Num::rectangular(&Num::flo(1.0), &Num::flo(1.0)).unwrap()));
        assert_eq!(
            Num::rectangular(&Num::int(1), &Num::int(1))
                .unwrap()
                .cmp(&Num::int(1)),
            Err(NumError::NotReal)
        );
        assert_eq!(
            Num::rational(1.into(), 3.into())
                .unwrap()
                .cmp(&Num::flo(0.3333)),
            Ok(Some(Greater))
        );
    }

    #[test]
    fn eqv_max_min() {
        assert!(!Num::int(2).eqv(&Num::flo(2.0)));
        assert!(Num::int(BigInt::from(10).pow(20)).eqv(&Num::int(BigInt::from(10).pow(20))));
        assert!(!Num::flo(0.0).eqv(&Num::flo(-0.0)));
        assert!(Num::flo(f64::NAN).eqv(&Num::flo(f64::NAN)));
        assert!(
            Num::rectangular(&Num::int(1), &Num::int(2))
                .unwrap()
                .eqv(&Num::rectangular(&Num::int(1), &Num::int(2)).unwrap())
        );
        assert_eq!(Num::flo(3.9).max(&Num::int(4)), Ok(Num::flo(4.0)));
        assert_eq!(Num::int(3).min(&Num::flo(3.1)), Ok(Num::flo(3.0)));
        assert_eq!(
            Num::int(100).max(&Num::flo(f64::INFINITY)),
            Ok(Num::flo(f64::INFINITY))
        );
        assert_eq!(
            Num::int(1).max(&Num::rectangular(&Num::int(1), &Num::int(1)).unwrap()),
            Err(NumError::NotReal)
        );
    }

    #[test]
    fn complex_division_and_mixed_exactness() {
        let z = Num::rectangular(&Num::int(1), &Num::int(2)).unwrap();
        let w = Num::rectangular(&Num::int(3), &Num::int(4)).unwrap();
        // (1+2i)/(3+4i) = (11+2i)/25
        assert_eq!(
            z.div(&w),
            Num::rectangular(
                &Num::rational(11.into(), 25.into()).unwrap(),
                &Num::rational(2.into(), 25.into()).unwrap()
            )
        );
        assert_eq!(w.div(&w), Ok(Num::int(1)));
        assert_eq!(
            z.add(&Num::flo(0.5)),
            Num::rectangular(&Num::flo(1.5), &Num::flo(2.0)).unwrap()
        );
        assert!(Num::flo(f64::NAN).max(&Num::int(1)).unwrap().is_nan());
        assert!(Num::int(1).min(&Num::flo(f64::NAN)).unwrap().is_nan());
    }

    #[test]
    fn a_real_with_an_inexact_complex_is_a_scalar() {
        let flo = Num::flo;
        let c = |re: Num, im: Num| Num::rectangular(&re, &im).unwrap();
        let inf_plus_i = c(flo(f64::INFINITY), flo(1.0));
        assert_eq!(
            Num::int(2).mul(&inf_plus_i),
            c(flo(f64::INFINITY), flo(2.0))
        );
        assert_eq!(
            inf_plus_i.div(&Num::int(2)),
            Ok(c(flo(f64::INFINITY), flo(0.5)))
        );
        assert!(matches!(
            Num::int(1).add(&Num::inexact_complex(Complex::new(0.0, -0.0))),
            Num::InexactComplex(z) if z.re == 1.0 && z.im.is_sign_negative()
        ));
        assert!(matches!(
            Num::flo(2.0).sub(&c(flo(1.0), flo(f64::NAN))),
            Num::InexactComplex(z) if z.re == 1.0 && z.im.is_nan()
        ));
    }
}

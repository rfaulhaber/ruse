//! The numeric tower: the single implementation of every numeric question in ruse.
//!
//! [`Num`] is heap-free — it owns its integers, ratios and floats directly and never
//! touches the GC heap — so the lexer, the printer, `eqv?` and the runtime can all call
//! the same code without a `Heap` in hand. The `num-*` crates assert on inputs they
//! cannot represent (a zero denominator, a negative radix, `0 ** -n`); every such
//! precondition is guarded here and surfaces as a [`NumError`], so nothing in this module
//! can panic on user data.

use std::cmp::Ordering;

pub use num_bigint::BigInt;
pub use num_complex::Complex;
pub use num_rational::BigRational;

use num_bigint::Sign;
use num_traits::{ToPrimitive, Zero};

/// A Scheme number, always held in its simplest variant: constructors collapse a ratio
/// with denominator 1 to `Int` and a complex with exact-zero imaginary part to its real
/// part, so structural equality is representation equality.
#[derive(Clone, Debug)]
pub enum Num {
    Int(BigInt),
    /// Denominator is always greater than 1.
    Rat(BigRational),
    /// A NaN is always `f64::NAN`'s bits; `-0.0` is preserved.
    Flo(f64),
    /// Imaginary part is never exact zero.
    ExactComplex(Complex<BigRational>),
    /// Both parts are inexact; an inexact zero imaginary part is kept. Build it only
    /// through [`Num::inexact_complex`], which canonicalises NaN parts: the bitwise
    /// `PartialEq` would otherwise tell two NaN payloads apart.
    InexactComplex(Complex<f64>),
}

/// Structural equality, for the AST and token tests: floats compare by bits, so NaN equals
/// NaN and `-0.0` differs from `0.0`, the way `eqv?` sees flonums. Numeric equality is
/// `num_eq`.
impl PartialEq for Num {
    fn eq(&self, other: &Num) -> bool {
        match (self, other) {
            (Num::Int(a), Num::Int(b)) => a == b,
            (Num::Rat(a), Num::Rat(b)) => a == b,
            (Num::Flo(a), Num::Flo(b)) => a.to_bits() == b.to_bits(),
            (Num::ExactComplex(a), Num::ExactComplex(b)) => a == b,
            (Num::InexactComplex(a), Num::InexactComplex(b)) => {
                a.re.to_bits() == b.re.to_bits() && a.im.to_bits() == b.im.to_bits()
            }
            _ => false,
        }
    }
}

/// Why a numeric operation has no answer.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumError {
    DivisionByZero,
    NotInteger,
    NotReal,
    NotRepresentable,
    BadRadix,
    ZeroToNegativePower,
    /// Anything else a crate would panic on.
    Domain,
}

pub type NumResult = Result<Num, NumError>;

impl NumError {
    /// The message fragment the VM attaches to a failed numeric primitive.
    pub fn detail(self) -> &'static str {
        match self {
            NumError::DivisionByZero => "division by zero",
            NumError::NotInteger => "expected an integer",
            NumError::NotReal => "expected a real number",
            NumError::NotRepresentable => "no exact representation",
            NumError::BadRadix => "radix must be 2, 8, 10 or 16",
            NumError::ZeroToNegativePower => "zero raised to a negative power",
            NumError::Domain => "argument outside the function's domain",
        }
    }
}

impl Num {
    pub fn int(i: impl Into<BigInt>) -> Num {
        Num::Int(i.into())
    }

    pub fn rational(numer: BigInt, denom: BigInt) -> NumResult {
        if denom.is_zero() {
            return Err(NumError::DivisionByZero);
        }
        Ok(Num::from_ratio(BigRational::new(numer, denom)))
    }

    pub fn from_ratio(r: BigRational) -> Num {
        if r.is_integer() {
            Num::Int(r.to_integer())
        } else {
            Num::Rat(r)
        }
    }

    pub fn flo(x: f64) -> Num {
        Num::Flo(canonical_nan(x))
    }

    pub fn inexact_complex(z: Complex<f64>) -> Num {
        Num::InexactComplex(Complex::new(canonical_nan(z.re), canonical_nan(z.im)))
    }

    /// `make-rectangular`. The result is complex only when the imaginary part is not an
    /// exact zero; mixed exactness makes both parts inexact.
    pub fn rectangular(re: &Num, im: &Num) -> NumResult {
        if !re.is_real() || !im.is_real() {
            return Err(NumError::NotReal);
        }
        // An exact zero means there is no imaginary part, whatever the real part's
        // exactness: `(real? -2.5+0i)` is `#t`.
        if im.is_exact() && im.is_zero() {
            return Ok(re.clone());
        }
        match (re.exact_real(), im.exact_real()) {
            (Some(re), Some(im)) => Ok(Num::exact_rectangular(re, im)),
            _ => match (re.to_f64(), im.to_f64()) {
                (Some(re), Some(im)) => Ok(Num::inexact_complex(Complex::new(re, im))),
                _ => Err(NumError::NotReal),
            },
        }
    }

    pub fn is_exact(&self) -> bool {
        matches!(self, Num::Int(_) | Num::Rat(_) | Num::ExactComplex(_))
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Num::Int(_) => true,
            Num::Flo(x) => x.is_finite() && x.fract() == 0.0,
            _ => false,
        }
    }

    pub fn is_rational(&self) -> bool {
        match self {
            Num::Int(_) | Num::Rat(_) => true,
            Num::Flo(x) => x.is_finite(),
            _ => false,
        }
    }

    /// A complex number is never real here, even with an inexact zero imaginary part:
    /// `(real? 1.0+0.0i)` is `#f`.
    pub fn is_real(&self) -> bool {
        matches!(self, Num::Int(_) | Num::Rat(_) | Num::Flo(_))
    }

    pub fn is_exact_integer(&self) -> bool {
        matches!(self, Num::Int(_))
    }

    pub fn is_finite(&self) -> bool {
        match self {
            Num::Int(_) | Num::Rat(_) | Num::ExactComplex(_) => true,
            Num::Flo(x) => x.is_finite(),
            Num::InexactComplex(c) => c.re.is_finite() && c.im.is_finite(),
        }
    }

    pub fn is_infinite(&self) -> bool {
        match self {
            Num::Flo(x) => x.is_infinite(),
            Num::InexactComplex(c) => c.re.is_infinite() || c.im.is_infinite(),
            _ => false,
        }
    }

    pub fn is_nan(&self) -> bool {
        match self {
            Num::Flo(x) => x.is_nan(),
            Num::InexactComplex(c) => c.re.is_nan() || c.im.is_nan(),
            _ => false,
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Num::Int(i) => i.is_zero(),
            Num::Rat(r) => r.is_zero(),
            Num::Flo(x) => x.is_zero(),
            Num::ExactComplex(c) => c.is_zero(),
            Num::InexactComplex(c) => c.is_zero(),
        }
    }

    /// The sign as an ordering against zero: `None` for NaN, `NotReal` for a complex.
    pub fn sign(&self) -> Result<Option<Ordering>, NumError> {
        match self {
            Num::Int(i) => Ok(Some(i.sign().cmp(&Sign::NoSign))),
            Num::Rat(r) => Ok(Some(r.numer().sign().cmp(&Sign::NoSign))),
            Num::Flo(x) => Ok(x.partial_cmp(&0.0)),
            Num::ExactComplex(_) | Num::InexactComplex(_) => Err(NumError::NotReal),
        }
    }

    /// `exact`. Infinities and NaNs have no exact value, in either part of a complex.
    pub fn exact(&self) -> NumResult {
        match self {
            Num::Int(_) | Num::Rat(_) | Num::ExactComplex(_) => Ok(self.clone()),
            Num::Flo(x) => BigRational::from_float(*x)
                .map(Num::from_ratio)
                .ok_or(NumError::NotRepresentable),
            Num::InexactComplex(c) => {
                match (BigRational::from_float(c.re), BigRational::from_float(c.im)) {
                    (Some(re), Some(im)) => Ok(Num::exact_rectangular(re, im)),
                    _ => Err(NumError::NotRepresentable),
                }
            }
        }
    }

    pub fn inexact(&self) -> Num {
        match self {
            Num::Int(i) => Num::Flo(int_to_f64(i)),
            Num::Rat(r) => Num::Flo(ratio_to_f64(r)),
            Num::Flo(_) | Num::InexactComplex(_) => self.clone(),
            Num::ExactComplex(c) => {
                Num::inexact_complex(Complex::new(ratio_to_f64(&c.re), ratio_to_f64(&c.im)))
            }
        }
    }

    /// The correctly rounded `f64` of a real number; `None` for a complex.
    pub fn to_f64(&self) -> Option<f64> {
        match self {
            Num::Int(i) => Some(int_to_f64(i)),
            Num::Rat(r) => Some(ratio_to_f64(r)),
            Num::Flo(x) => Some(*x),
            Num::ExactComplex(_) | Num::InexactComplex(_) => None,
        }
    }

    /// The value of an exact real as a ratio.
    fn exact_real(&self) -> Option<BigRational> {
        match self {
            Num::Int(i) => Some(BigRational::from_integer(i.clone())),
            Num::Rat(r) => Some(r.clone()),
            _ => None,
        }
    }

    fn exact_rectangular(re: BigRational, im: BigRational) -> Num {
        if im.is_zero() {
            Num::from_ratio(re)
        } else {
            Num::ExactComplex(Complex::new(re, im))
        }
    }
}

/// NaNs carry a payload, and `eqv?` compares floats bitwise.
fn canonical_nan(x: f64) -> f64 {
    if x.is_nan() { f64::NAN } else { x }
}

fn int_to_f64(i: &BigInt) -> f64 {
    // The conversion saturates to infinity on overflow; the fallback only keeps it total.
    i.to_f64().unwrap_or(if i.sign() == Sign::Minus {
        f64::NEG_INFINITY
    } else {
        f64::INFINITY
    })
}

fn ratio_to_f64(r: &BigRational) -> f64 {
    r.to_f64().unwrap_or(f64::NAN)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constructors_normalise() {
        assert_eq!(
            Num::rational(6.into(), 4.into()),
            Ok(Num::Rat(BigRational::new(3.into(), 2.into())))
        );
        assert_eq!(Num::rational(6.into(), 3.into()), Ok(Num::int(2)));
        assert_eq!(
            Num::rational(1.into(), 0.into()),
            Err(NumError::DivisionByZero)
        );
        assert_eq!(
            Num::rectangular(&Num::int(3), &Num::int(0)),
            Ok(Num::int(3))
        );
        assert!(matches!(
            Num::rectangular(&Num::int(3), &Num::flo(0.0)),
            Ok(Num::InexactComplex(_))
        ));
        assert!(matches!(
            Num::rectangular(&Num::int(1), &Num::flo(2.0)),
            Ok(Num::InexactComplex(_))
        ));
        assert_eq!(
            Num::rectangular(
                &Num::int(1),
                &Num::rectangular(&Num::int(1), &Num::int(1)).unwrap()
            ),
            Err(NumError::NotReal)
        );
        // one canonical NaN
        assert_eq!(
            Num::flo(f64::from_bits(0x7FF8_0000_0000_0001)),
            Num::flo(f64::NAN)
        );
        assert!(matches!(Num::flo(-0.0), Num::Flo(x) if x.is_sign_negative()));
        assert_eq!(
            Num::rectangular(&Num::flo(-2.5), &Num::int(0)),
            Ok(Num::flo(-2.5))
        );
        assert_eq!(
            Num::inexact_complex(Complex::new(1.0, f64::from_bits(0xFFF8_0000_0000_0000))),
            Num::rectangular(&Num::flo(1.0), &Num::flo(f64::NAN)).unwrap()
        );
    }

    #[test]
    fn predicates_follow_r7rs_6_2_4() {
        let c_exact = Num::rectangular(&Num::int(1), &Num::int(2)).unwrap();
        let c_inexact_zero_im = Num::rectangular(&Num::flo(-2.5), &Num::flo(0.0)).unwrap();
        assert!(Num::flo(3.0).is_integer() && !Num::flo(3.0).is_exact_integer());
        assert!(Num::flo(1e308).is_rational() && !Num::flo(f64::INFINITY).is_rational());
        assert!(!c_inexact_zero_im.is_real() && Num::int(-2).is_real());
        assert!(!c_exact.is_real() && !c_exact.is_rational());
        assert!(
            !Num::rational(32.into(), 5.into())
                .unwrap()
                .is_exact_integer()
        );
        assert!(
            !Num::rectangular(&Num::flo(3.0), &Num::flo(f64::INFINITY))
                .unwrap()
                .is_finite()
        );
        assert!(!c_exact.is_nan());
        assert!(
            Num::rectangular(&Num::flo(0.0), &Num::flo(0.0))
                .unwrap()
                .is_zero()
        );
        assert_eq!(Num::flo(f64::NAN).sign(), Ok(None));
        assert_eq!(c_exact.sign(), Err(NumError::NotReal));
    }

    #[test]
    fn exactness_conversions() {
        assert_eq!(
            Num::flo(0.5).exact(),
            Ok(Num::Rat(BigRational::new(1.into(), 2.into())))
        );
        assert_eq!(
            Num::flo(0.1).exact(),
            Ok(Num::Rat(BigRational::new(
                3602879701896397u64.into(),
                36028797018963968u64.into()
            )))
        );
        assert_eq!(Num::flo(-0.0).exact(), Ok(Num::int(0)));
        assert_eq!(
            Num::flo(f64::INFINITY).exact(),
            Err(NumError::NotRepresentable)
        );
        assert_eq!(Num::flo(f64::NAN).exact(), Err(NumError::NotRepresentable));
        assert_eq!(
            Num::rational(1.into(), 3.into()).unwrap().inexact(),
            Num::flo(1.0 / 3.0)
        );
        assert_eq!(
            Num::int(BigInt::from(2).pow(1024)).inexact(),
            Num::flo(f64::INFINITY)
        );
        assert_eq!(Num::int(1).inexact(), Num::flo(1.0));
        assert_eq!(
            Num::rational(1.into(), 2.into()).unwrap().to_f64(),
            Some(0.5)
        );
        assert_eq!(
            Num::rectangular(&Num::int(1), &Num::int(2))
                .unwrap()
                .to_f64(),
            None
        );
    }
}

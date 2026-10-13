//! Transcendental functions, roots and powers, and the complex-number accessors on [`Num`].
//!
//! `exp`, `log` and the trigonometric family answer inexactly even for an exact argument:
//! `(exp 0)` is `1.0`. A real argument runs on `f64`. Where no real answer exists
//! (`(log -1)`, `(asin 2)`, `(sqrt -2)`) or the argument is complex, the computation moves
//! to `Complex<f64>`. `sqrt` stays exact for an exact real whose root is a ratio of integers,
//! and `expt` for an exact base under an exact integer exponent; `(expt 4 1/2)` is `2.0`.
//!
//! An exact real beyond the double range is scaled to `m · 2^k` before it is converted, so
//! that an answer which fits survives an argument which does not: `(log (expt 3 1000))` is
//! `1098.61...`, not `+inf.0`. That covers `log`, the inexact `sqrt`, `expt` under a real
//! exponent, the two-argument `atan`, and `angle` and `magnitude` of an exact complex; every
//! other function converts its argument directly. `sin`, `cos` and `tan` of an exact number
//! above 2^53 stay inexact noise however it is converted, because one ulp of the argument
//! exceeds 2π.
//!
//! Where R7RS leaves a result open this module follows chibi: `(log 0)` is `-inf.0`,
//! `(angle +inf.0)` is `0.0`, `(angle -0.0)` is `π` as the signed zero of `atan2` says, and
//! `sqrt` takes the principal branch whatever the sign of a zero imaginary part. The
//! choices below are ruse's own:
//!
//! - An exact complex base under an exact integer exponent stays exact: `(expt 1+2i 2)` is
//!   `-3+4i`, where chibi answers with a flonum complex.
//! - An exact zero base under a positive exponent is a zero of the exponent's exactness,
//!   `(expt 0 1/2)` included: `0`, but `(expt 0 1.0)` is `0.0`.
//! - An exact zero base under a negative exponent is an error, `(expt 0 -1.0)` included
//!   (R7RS §6.2.6), as an exact zero divisor is everywhere. An inexact zero base keeps
//!   IEEE: `(expt 0.0 -1)` is `+inf.0`.
//! - A negative base under a fractional exponent is complex: `(expt -8 1/3)`, where chibi
//!   answers `+nan.0`.
//! - `asin` and `acos` ignore the sign of a zero imaginary part where the real part lies
//!   beyond 1 in magnitude, as `sqrt` does, so `(asin 2.0+0.0i)`, `(asin 2.0-0.0i)` and
//!   `(asin 2.0)` agree, on the side of the cut that is continuous with the fourth quadrant
//!   above 1 and with the second below -1. Within [-1, 1] Kahan's formulas run and keep the
//!   sign of the zero. `atan` follows num-complex, which ignores the sign of a zero *real*
//!   part on its cuts instead: `(atan 0.0-2.0i)` and `(atan -0.0-2.0i)` both answer `-π/2`,
//!   where chibi and C99 answer `+π/2` and `-π/2`. That deviation is recorded in
//!   `tests/chibi_diff.rs`.

use std::cmp::Ordering;
use std::f64::consts::{FRAC_PI_2, LN_2, PI};

use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};

use super::{BigInt, BigRational, Complex, Num, NumError, NumResult, int_to_f64, ratio_to_f64};

impl Num {
    pub fn exp(&self) -> Num {
        self.transcendental(f64::exp, Complex::exp)
    }

    /// The natural logarithm. `(log 0)` is `-inf.0`; a negative real has the complex
    /// logarithm `ln|x| + πi`, where `f64::ln` would give NaN.
    pub fn log(&self) -> NumResult {
        Ok(match self.to_scaled_f64() {
            Some((m, k)) if m < 0.0 => Num::inexact_complex(Complex::new(ln_scaled(-m, k), PI)),
            Some((m, k)) => Num::flo(ln_scaled(m, k)),
            None => Num::inexact_complex(self.to_inexact_complex().ln()),
        })
    }

    /// `(log z base)`, as the quotient of natural logarithms.
    pub fn log_base(&self, base: &Num) -> NumResult {
        self.log()?.div(&base.log()?)
    }

    pub fn sin(&self) -> Num {
        self.transcendental(f64::sin, Complex::sin)
    }

    pub fn cos(&self) -> Num {
        self.transcendental(f64::cos, Complex::cos)
    }

    pub fn tan(&self) -> Num {
        self.transcendental(f64::tan, complex_tan)
    }

    pub fn asin(&self) -> Num {
        self.arc(f64::asin, asin_beyond_unit, complex_asin)
    }

    pub fn acos(&self) -> Num {
        self.arc(f64::acos, acos_beyond_unit, complex_acos)
    }

    pub fn atan(&self) -> Num {
        self.transcendental(f64::atan, Complex::atan)
    }

    /// `(atan y x)`, defined on reals only. The signs of the operands, zeros included, pick
    /// the quadrant: `(atan -0.0 -1.0)` is `-π`.
    pub fn atan2(&self, x: &Num) -> NumResult {
        match (self.to_scaled_f64(), x.to_scaled_f64()) {
            (Some(y), Some(x)) => {
                let (y, x) = common_scale(y, x);
                Ok(Num::flo(y.atan2(x)))
            }
            _ => Err(NumError::NotReal),
        }
    }

    /// The principal square root. It is exact only for an exact real whose magnitude has a
    /// perfect-square numerator and denominator: `(sqrt 1/4)` is `1/2` and `(sqrt -4)` is
    /// `+2i`, but `(sqrt 2)` is a flonum.
    pub fn sqrt(&self) -> Num {
        if let Some(r) = self.exact_real()
            && let Some(root) = exact_ratio_root(&r.abs())
        {
            return if r.is_negative() {
                Num::exact_rectangular(BigRational::zero(), root)
            } else {
                Num::from_ratio(root)
            };
        }
        self.inexact_sqrt()
    }

    /// `sqrt` on `f64`. An exact real is scaled first, so a root that fits survives a
    /// radicand that does not: `(sqrt (expt 3 1001))`.
    fn inexact_sqrt(&self) -> Num {
        match self.to_scaled_f64() {
            Some((m, k)) => {
                // A power of two has a power-of-two root only when the exponent is even.
                let (m, k) = if k % 2 == 0 { (m, k) } else { (m * 2.0, k - 1) };
                if m < 0.0 {
                    Num::inexact_complex(Complex::new(0.0, scale_pow2((-m).sqrt(), k / 2)))
                } else {
                    Num::flo(scale_pow2(m.sqrt(), k / 2))
                }
            }
            None => complex_sqrt(self.to_inexact_complex()),
        }
    }

    /// `expt`. An exact base to an exact integer power is exact, and so is an exact zero to a
    /// positive exact power: `(expt 0 1/2)` is `0`. Anything else is inexact: real `powf`
    /// where it has an answer, complex `powc` where it has none or an operand is complex, so
    /// `(expt -8 1/3)` is complex. An exact zero base takes the exponent's real part as R7RS
    /// §6.2.6 does: a positive one gives zero, a negative one is an error.
    pub fn expt(&self, e: &Num) -> NumResult {
        if self.is_exact()
            && let Num::Int(n) = e
        {
            return self.exact_power(n);
        }
        if self.is_exact() && self.is_zero() {
            match e.real_part().sign() {
                Ok(Some(Ordering::Greater)) => {
                    return Ok(if e.is_exact() {
                        Num::int(0)
                    } else {
                        Num::flo(0.0)
                    });
                }
                Ok(Some(Ordering::Less)) => return Err(NumError::ZeroToNegativePower),
                _ => {}
            }
        }
        // `powf` of a base beyond the double range would saturate, so the power goes through
        // the logarithm of the scaled base instead. A negative base takes the complex path.
        if let (Some((m, k)), Some(power)) = (self.to_scaled_f64(), e.to_f64())
            && k != 0
            && m > 0.0
        {
            return Ok(Num::flo((power * ln_scaled(m, k)).exp()));
        }
        match (self.to_f64(), e.to_f64()) {
            (Some(base), Some(power)) if has_real_power(base, power) => {
                Ok(Num::flo(base.powf(power)))
            }
            _ => Ok(Num::inexact_complex(
                self.to_inexact_complex().powc(e.to_inexact_complex()),
            )),
        }
    }

    pub fn real_part(&self) -> Num {
        self.parts().0
    }

    /// A real number has an exact-zero imaginary part, whatever its own exactness.
    pub fn imag_part(&self) -> Num {
        self.parts().1
    }

    /// The absolute value of a real, or the modulus of a complex. An exact complex has an
    /// exact modulus when its squared norm is a perfect square: `(magnitude 3+4i)` is `5`.
    pub fn magnitude(&self) -> Num {
        match self {
            Num::ExactComplex(z) => match exact_ratio_root(&z.norm_sqr()) {
                Some(root) => Num::from_ratio(root),
                None => Num::from_ratio(z.norm_sqr()).inexact_sqrt(),
            },
            Num::InexactComplex(z) => Num::flo(z.norm()),
            // `abs` accepts every real; the fallback only keeps the match total.
            real => real.abs().unwrap_or_else(|_| real.clone()),
        }
    }

    /// The argument in `(-π, π]`. An exact non-negative real, zero included, has the exact
    /// angle 0; the angle of a negative one, and every angle of an inexact number, is a
    /// flonum.
    pub fn angle(&self) -> Num {
        match self {
            Num::Int(_) | Num::Rat(_) if self.sign() == Ok(Some(Ordering::Less)) => Num::flo(PI),
            Num::Int(_) | Num::Rat(_) => Num::int(0),
            Num::Flo(x) => Num::flo(0.0f64.atan2(*x)),
            Num::ExactComplex(z) => {
                let (im, re) = common_scale(scaled_ratio(&z.im), scaled_ratio(&z.re));
                Num::flo(im.atan2(re))
            }
            Num::InexactComplex(z) => Num::flo(z.arg()),
        }
    }

    /// A real as `(m, k)` with the value `m · 2^k`, for a function whose argument may lie
    /// beyond the double range. A value the format holds converts as [`Num::to_f64`] does,
    /// with `k` zero; an exact value beyond it keeps its leading bits in `m`, far from either
    /// end of the range, and the rest in `k`. `None` for a complex.
    fn to_scaled_f64(&self) -> Option<(f64, i64)> {
        match self {
            Num::Int(i) => Some(scaled_int(i)),
            Num::Rat(r) => Some(scaled_ratio(r)),
            Num::Flo(x) => Some((*x, 0)),
            Num::ExactComplex(_) | Num::InexactComplex(_) => None,
        }
    }

    /// Applies `real` to a real argument and `complex` to a complex one, inexactly.
    fn transcendental(
        &self,
        real: fn(f64) -> f64,
        complex: fn(Complex<f64>) -> Complex<f64>,
    ) -> Num {
        match self.to_f64() {
            Some(x) => Num::flo(real(x)),
            None => Num::inexact_complex(complex(self.to_inexact_complex())),
        }
    }

    /// As [`Num::transcendental`] for `asin` and `acos`, which leave the reals for an
    /// argument outside `[-1, 1]`; `beyond` answers that case.
    fn arc(
        &self,
        real: fn(f64) -> f64,
        beyond: fn(f64) -> Complex<f64>,
        complex: fn(Complex<f64>) -> Complex<f64>,
    ) -> Num {
        // A complex argument with a zero imaginary part, of either sign, is on the real axis.
        let on_real_axis = match self {
            Num::InexactComplex(z) if z.im == 0.0 => Some(z.re),
            _ => self.to_f64(),
        };
        match on_real_axis {
            Some(x) if x.abs() > 1.0 => Num::inexact_complex(beyond(x)),
            _ => self.transcendental(real, complex),
        }
    }

    /// An exact base to an exact integer power. The base must be exact; the inexact arm
    /// only keeps the match total.
    fn exact_power(&self, n: &BigInt) -> NumResult {
        if n.is_zero() {
            return Ok(Num::int(1));
        }
        if self.is_zero() {
            return if n.is_negative() {
                Err(NumError::ZeroToNegativePower)
            } else {
                Ok(self.clone())
            };
        }
        // A unit base has one of two answers for any exponent, however large.
        if let Num::Int(b) = self
            && b.magnitude().is_one()
        {
            return Ok(Num::int(if b.is_negative() && n.is_odd() { -1 } else { 1 }));
        }
        // The crates take the exponent as a `u32`, and a larger power is not computed.
        let power = n.magnitude().to_u32().ok_or(NumError::Domain)?;
        let positive = match self {
            Num::Int(b) => Num::Int(b.pow(power)),
            Num::Rat(b) => Num::rational(b.numer().pow(power), b.denom().pow(power))?,
            Num::ExactComplex(z) => {
                let z = z.powu(power);
                Num::exact_rectangular(z.re, z.im)
            }
            Num::Flo(_) | Num::InexactComplex(_) => return Err(NumError::Domain),
        };
        if n.is_negative() {
            Num::int(1).div(&positive)
        } else {
            Ok(positive)
        }
    }
}

/// The root of a perfect square. `BigInt::sqrt` panics on a negative operand.
fn perfect_square_root(n: &BigInt) -> Option<BigInt> {
    if n.is_negative() {
        return None;
    }
    let root = n.sqrt();
    (&root * &root == *n).then_some(root)
}

/// The exact root of a non-negative ratio whose numerator and denominator are both perfect
/// squares.
fn exact_ratio_root(r: &BigRational) -> Option<BigRational> {
    let numer = perfect_square_root(r.numer())?;
    let denom = perfect_square_root(r.denom())?;
    // A denominator is positive, and so is its root.
    Some(BigRational::new(numer, denom))
}

/// Bits of an exact value kept when it is scaled into the `f64` range: more than a
/// mantissa's 53, so the final conversion's rounding is the one that counts.
const SCALED_BITS: u64 = 60;

/// An integer of fewer bits is below 2^1023, so it converts without saturating.
const FINITE_INT_BITS: u64 = 1024;

fn scaled_int(i: &BigInt) -> (f64, i64) {
    if i.bits() < FINITE_INT_BITS {
        (int_to_f64(i), 0)
    } else {
        scale_down(i)
    }
}

fn scaled_ratio(r: &BigRational) -> (f64, i64) {
    let plain = ratio_to_f64(r);
    // A subnormal keeps too few digits to take a logarithm of.
    if plain.is_normal() {
        return (plain, 0);
    }
    let ((n, kn), (d, kd)) = (scale_down(r.numer()), scale_down(r.denom()));
    (n / d, kn - kd)
}

/// An integer as `(m, k)` with the value `m · 2^k`, where `m` holds its leading
/// [`SCALED_BITS`] bits. Zero is `(0.0, 0)`.
fn scale_down(i: &BigInt) -> (f64, i64) {
    let shift = i.bits().saturating_sub(SCALED_BITS);
    // The magnitude is shifted and the sign put back, so that `-i` scales to `-m`: a `BigInt`
    // shift rounds toward negative infinity. The fallback only keeps the conversion total.
    let m = (i.magnitude() >> shift).to_f64().unwrap_or(f64::NAN);
    (if i.is_negative() { -m } else { m }, shift as i64)
}

/// `x · 2^k`. A power of two beyond ±1023 is not a finite double, so the factor is applied in
/// steps. A finite nonzero `x` is infinite or zero within three of them, which bounds the
/// loop however large `k` is.
fn scale_pow2(mut x: f64, mut k: i64) -> f64 {
    while k != 0 && x != 0.0 && x.is_finite() {
        let step = k.clamp(-1000, 1000);
        x *= 2f64.powi(step as i32);
        k -= step;
    }
    x
}

/// `ln(m · 2^k)`.
fn ln_scaled(m: f64, k: i64) -> f64 {
    m.ln() + k as f64 * LN_2
}

/// Two scaled reals as doubles in one scale, for a function of their ratio such as `atan2`.
/// The operand with the smaller exponent may underflow to zero, as its share of the ratio
/// does.
fn common_scale((a, ka): (f64, i64), (b, kb): (f64, i64)) -> (f64, f64) {
    let k = ka.max(kb);
    (scale_pow2(a, ka - k), scale_pow2(b, kb - k))
}

/// `Complex::sqrt` follows C99 Annex G, where a `-0.0` imaginary part selects the lower
/// branch: `csqrt(-1-0i)` is `-i`. The suite and chibi take the principal branch whatever
/// the sign of the zero, so `(sqrt -1.0-0.0i)` is `+i`; the sign is dropped first.
fn complex_sqrt(mut z: Complex<f64>) -> Num {
    if z.im == 0.0 {
        z.im = 0.0;
    }
    Num::inexact_complex(z.sqrt())
}

/// `asin` of a number off the real axis by Kahan's formulas. `Complex::asin` evaluates
/// `-i ln(iz + sqrt(1 - z²))`, whose terms cancel away the digits of the answer once `|z|`
/// passes about `1e5` and make a NaN of it past `1e8`; pairing the square roots of `1 - z` and
/// `1 + z` keeps every digit. The formulas are not used on the real axis, where they would
/// read a zero imaginary part of either sign as `+0.0` in `1 - z` (num-complex subtracts
/// `0.0 - z.im`) and `asin(-z)` would stop being `-asin(z)`; `Num::arc` sends it to
/// [`asin_beyond_unit`] instead.
fn complex_asin(z: Complex<f64>) -> Complex<f64> {
    let (a, b) = ((1.0 - z).sqrt(), (1.0 + z).sqrt());
    Complex::new(z.re.atan2((a * b).re), (a.conj() * b).im.asinh())
}

/// `acos` of a non-real number by Kahan's formulas; see [`complex_asin`].
fn complex_acos(z: Complex<f64>) -> Complex<f64> {
    let (a, b) = ((1.0 - z).sqrt(), (1.0 + z).sqrt());
    Complex::new(2.0 * a.re.atan2(b.re), (b.conj() * a).im.asinh())
}

/// `asin` of a real outside `[-1, 1]` in closed form, with the imaginary part negative for a
/// positive argument as `(asin 2)` has it. It also answers a complex argument whose imaginary
/// part is `+0.0` or `-0.0`, so the sign of that zero cannot pick the side of the cut.
fn asin_beyond_unit(x: f64) -> Complex<f64> {
    Complex::new(FRAC_PI_2.copysign(x), -acosh(x.abs()).copysign(x))
}

/// `acos` of a real outside `[-1, 1]`, as `π/2 - asin`. `Complex::acos` cancels `z` against
/// `i sqrt(1 - z²)` and loses every digit of the imaginary part for `|z|` near `1e8`.
fn acos_beyond_unit(x: f64) -> Complex<f64> {
    let asin = asin_beyond_unit(x);
    Complex::new(FRAC_PI_2 - asin.re, -asin.im)
}

/// `f64::acosh` for `x >= 1`, which overflows summing `x` with itself above `9e307`; from
/// `2^28` up `ln(2x)` is already correctly rounded.
fn acosh(x: f64) -> f64 {
    if x > 268_435_456.0 {
        x.ln() + LN_2
    } else {
        x.acosh()
    }
}

/// `Complex::tan` divides `sinh(2y)` by `cosh(2y)`, which overflow together from `|y|` of
/// about 355 and make a NaN. From `|y|` of 20 the quotient is `±i` to within `e^-40`, and the
/// real part is `2 sin(2x) e^(-2|y|)`.
fn complex_tan(z: Complex<f64>) -> Complex<f64> {
    if z.im.abs() >= 20.0 {
        Complex::new(
            2.0 * (2.0 * z.re).sin() * (-2.0 * z.im.abs()).exp(),
            z.im.signum(),
        )
    } else {
        z.tan()
    }
}

/// Whether `base.powf(power)` is the answer. It is NaN for a negative base and a
/// fractional exponent, where the principal value is complex.
fn has_real_power(base: f64, power: f64) -> bool {
    base >= 0.0 || base.is_nan() || !power.is_finite() || power.fract() == 0.0
}

#[cfg(test)]
mod tests {
    use std::f64::consts::{FRAC_PI_2, PI};

    use super::*;

    fn r(n: i64, d: i64) -> Num {
        Num::rational(n.into(), d.into()).unwrap()
    }

    fn c(re: Num, im: Num) -> Num {
        Num::rectangular(&re, &im).unwrap()
    }

    fn pow(base: i64, exponent: u32) -> Num {
        Num::int(BigInt::from(base).pow(exponent))
    }

    // Miri perturbs each transcendental call by a few ulps, which the tolerance absorbs.
    fn assert_close(actual: Num, expected: f64) {
        match actual {
            Num::Flo(x) => assert!(
                (x - expected).abs() <= 1e-12 * expected.abs(),
                "{x} is not within 1e-12 of {expected}"
            ),
            other => panic!("expected a flonum near {expected}, got {other:?}"),
        }
    }

    // Miri perturbs every transcendental `f64` result by a few ulps, and this test asserts
    // exact floats.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn exact_results_where_the_design_promises_them() {
        assert_eq!(Num::int(4).sqrt(), Num::int(2));
        assert_eq!(r(1, 4).sqrt(), r(1, 2));
        assert_eq!(Num::int(-4).sqrt(), c(Num::int(0), Num::int(2)));
        assert_eq!(Num::int(2).sqrt(), Num::flo(2f64.sqrt()));
        assert_eq!(Num::flo(-1.0).sqrt(), c(Num::flo(0.0), Num::flo(1.0)));
        // The principal branch whatever the zero's sign: the suite and chibi, not C99.
        assert_eq!(
            c(Num::flo(-1.0), Num::flo(-0.0)).sqrt(),
            c(Num::flo(0.0), Num::flo(1.0))
        );
        assert_eq!(
            Num::int(2).expt(&Num::int(100)),
            Ok(Num::int(BigInt::from(2).pow(100)))
        );
        assert_eq!(Num::int(2).expt(&Num::int(-2)), Ok(r(1, 4)));
        assert_eq!(r(1, 2).expt(&Num::int(3)), Ok(r(1, 8)));
        assert_eq!(Num::int(0).expt(&Num::int(0)), Ok(Num::int(1)));
        assert_eq!(Num::int(0).expt(&Num::int(1)), Ok(Num::int(0)));
        assert_eq!(Num::flo(0.0).expt(&Num::int(0)), Ok(Num::flo(1.0)));
        assert_eq!(Num::int(0).expt(&Num::flo(1.0)), Ok(Num::flo(0.0)));
        assert_eq!(
            Num::int(0).expt(&Num::int(-1)),
            Err(NumError::ZeroToNegativePower)
        );
        assert_eq!(
            Num::int(2).expt(&Num::int(4294967296i64)),
            Err(NumError::Domain)
        );
        assert_eq!(Num::flo(2.0).expt(&Num::int(3)), Ok(Num::flo(8.0)));
        assert!(matches!(
            Num::int(-8).expt(&r(1, 3)),
            Ok(Num::InexactComplex(_))
        ));
    }

    // Miri perturbs every transcendental `f64` result by a few ulps, and this test asserts
    // exact floats.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn transcendentals_promote_to_complex_when_the_real_answer_does_not_exist() {
        assert!(matches!(
            Num::int(-1).log(),
            Ok(Num::InexactComplex(z)) if (z.im - PI).abs() < 1e-15 && z.re == 0.0
        ));
        // chibi's answer
        assert_eq!(Num::int(0).log(), Ok(Num::flo(f64::NEG_INFINITY)));
        assert_eq!(Num::int(8).log_base(&Num::int(2)), Ok(Num::flo(3.0)));
        assert!(matches!(Num::int(2).asin(), Num::InexactComplex(_)));
        assert_eq!(Num::flo(-0.0).atan2(&Num::flo(1.0)), Ok(Num::flo(-0.0)));
        assert_eq!(Num::flo(-0.0).atan2(&Num::flo(-1.0)), Ok(Num::flo(-PI)));
        assert_eq!(
            c(Num::int(1), Num::int(1)).atan2(&Num::int(1)),
            Err(NumError::NotReal)
        );
        assert_eq!(Num::int(0).exp(), Num::flo(1.0));
        assert_eq!(Num::flo(0.0).sin(), Num::flo(0.0));
    }

    #[test]
    fn a_unit_base_needs_no_computation_whatever_the_exponent() {
        let huge: BigInt = BigInt::from(1) << 40;
        let odd: BigInt = &huge + 1;
        assert_eq!(Num::int(1).expt(&Num::int(4294967296i64)), Ok(Num::int(1)));
        assert_eq!(Num::int(1).expt(&Num::int(-huge.clone())), Ok(Num::int(1)));
        assert_eq!(Num::int(-1).expt(&Num::int(huge.clone())), Ok(Num::int(1)));
        assert_eq!(Num::int(-1).expt(&Num::int(odd.clone())), Ok(Num::int(-1)));
        assert_eq!(Num::int(-1).expt(&Num::int(-odd)), Ok(Num::int(-1)));
        assert_eq!(Num::int(0).expt(&Num::int(4294967296i64)), Ok(Num::int(0)));
    }

    #[test]
    fn exact_arguments_beyond_the_double_range_scale_before_they_convert() {
        // Scaling 3^1001 shifts by an odd number of bits and 3^1003 by an even number.
        assert_close(pow(3, 1000).log().unwrap(), 1098.6122886681098);
        assert_close(pow(10, 400).log().unwrap(), 400.0 * std::f64::consts::LN_10);
        let tiny = Num::rational(1.into(), BigInt::from(10).pow(400)).unwrap();
        assert_close(tiny.log().unwrap(), -400.0 * std::f64::consts::LN_10);
        assert!(matches!(
            Num::int(-BigInt::from(3).pow(1000)).log(),
            Ok(Num::InexactComplex(z))
                if (z.re - 1098.6122886681098).abs() <= 1e-12 * 1098.6122886681098 && z.im == PI
        ));

        assert_close(pow(3, 1001).sqrt(), 3f64.powf(500.5));
        assert_close(pow(3, 1003).sqrt(), 3f64.powf(501.5));
        let two_over_huge = Num::rational(2.into(), BigInt::from(10).pow(400)).unwrap();
        assert_close(two_over_huge.sqrt(), 2f64.sqrt() * 1e-200);
        assert!(matches!(
            Num::int(-BigInt::from(3).pow(1001)).sqrt(),
            Num::InexactComplex(z)
                if z.re == 0.0 && (z.im - 3f64.powf(500.5)).abs() <= 1e-12 * 3f64.powf(500.5)
        ));

        let quarter = r(1, 4);
        assert_close(pow(10, 400).expt(&quarter).unwrap(), 1e100);
        assert_close(pow(10, 400).expt(&Num::flo(0.25)).unwrap(), 1e100);
        assert_close(tiny.expt(&quarter).unwrap(), 1e-100);

        let (y, x) = (pow(10, 400), Num::int(2).mul(&pow(10, 400)));
        assert_close(y.atan2(&x).unwrap(), 0.4636476090008061);
        assert_close(y.neg().atan2(&x.neg()).unwrap(), -PI + 0.4636476090008061);
        assert_eq!(Num::int(1).atan2(&pow(10, 400)), Ok(Num::flo(0.0)));

        let diagonal = c(pow(10, 400), pow(10, 400));
        assert_close(diagonal.angle(), PI / 4.0);
        assert_close(c(pow(10, 400).neg(), pow(10, 400)).angle(), 3.0 * PI / 4.0);
        // The answer for 10^200 fits a double although its squared norm does not.
        assert_close(
            c(pow(10, 200), pow(10, 200)).magnitude(),
            2f64.sqrt() * 1e200,
        );
        assert_eq!(diagonal.magnitude(), Num::flo(f64::INFINITY));
    }

    #[test]
    fn asin_and_acos_beyond_the_unit_interval_follow_one_branch_at_any_magnitude() {
        for x in [1.5, 2.0, 742.0, 1e3, -1.5, -2.0, -742.0, -1e3] {
            let library = Complex::new(x, 0.0);
            // The library loses digits to cancellation as |x| grows; the branch is what is
            // being compared.
            assert!((asin_beyond_unit(x) - library.asin()).norm() <= 1e-6);
            assert!((acos_beyond_unit(x) - library.acos()).norm() <= 1e-6);
        }
        // `ln` is the one transcendental call, and Miri perturbs it by a few ulps.
        let near = |actual: Num, re: f64, im: f64| match actual {
            Num::InexactComplex(z) => (z.re - re).abs() < 1e-9 && (z.im - im).abs() < 1e-9,
            _ => false,
        };
        let lift = 46.74484904044086;
        assert!(near(Num::flo(1e20).asin(), FRAC_PI_2, -lift));
        assert!(near(Num::flo(-1e20).asin(), -FRAC_PI_2, lift));
        assert!(near(Num::flo(1e20).acos(), 0.0, lift));
        assert!(near(Num::flo(-1e20).acos(), PI, -lift));
        assert_eq!(
            Num::flo(f64::INFINITY).asin(),
            c(Num::flo(FRAC_PI_2), Num::flo(f64::NEG_INFINITY))
        );
        assert_eq!(
            Num::flo(f64::INFINITY).acos(),
            c(Num::flo(0.0), Num::flo(f64::INFINITY))
        );
        assert_eq!(Num::flo(f64::NAN).asin(), Num::flo(f64::NAN));
        assert!(acosh(1e308).is_finite());
    }

    // The references are Python's `cmath.asin` and `cmath.acos`, which use an unrelated
    // algorithm. The tolerance is far below the digits `Complex::asin` loses at 1e5 and
    // above, and loose enough for Miri's few-ulp perturbation of `atan2` and `asinh`. It is
    // absolute for a small part (1e-12 times the larger of 1 and the part), so a part many
    // orders below the other, such as the 2.5e-10 real part of `acos` at `1e10+2.5i`, is only
    // checked that closely: from |z| of about 1e10 the polar-form square root in
    // `Complex::sqrt` costs `acos` digits there, a known limit.
    #[test]
    fn asin_and_acos_of_a_complex_argument_keep_their_digits_where_the_library_loses_them() {
        let cases = [
            (
                (1e10, 2.5),
                (1.5707963265448965, 23.7189981105004),
                (2.5e-10, -23.7189981105004),
            ),
            (
                (-1e10, 2.5),
                (-1.5707963265448965, 23.7189981105004),
                (3.141592653339793, -23.7189981105004),
            ),
            (
                (1e5, 2.5),
                (1.5707713267949006, 12.206072645817674),
                (2.4999999996041667e-5, -12.206072645817674),
            ),
            (
                (1.0, 1.0),
                (0.6662394324925153, 1.0612750619050357),
                (0.9045568943023814, -1.0612750619050357),
            ),
            (
                (3.0, -4.0),
                (0.6339838656391766, -2.305509031243477),
                (0.9368124611557198, 2.305509031243477),
            ),
            (
                (1e200, 1e200),
                (0.7853981633974483, 461.55673936964905),
                (0.7853981633974484, -461.55673936964905),
            ),
        ];
        let close = |actual: Num, (re, im): (f64, f64)| match actual {
            Num::InexactComplex(z) => {
                (z.re - re).abs() <= 1e-12 * re.abs().max(1.0)
                    && (z.im - im).abs() <= 1e-12 * im.abs().max(1.0)
            }
            _ => false,
        };
        for ((re, im), asin, acos) in cases {
            let z = c(Num::flo(re), Num::flo(im));
            assert!(close(z.asin(), asin), "asin of {re}{im:+}i");
            assert!(close(z.acos(), acos), "acos of {re}{im:+}i");
        }
    }

    // Miri perturbs each transcendental call on its own, so two calls on the same argument
    // differ in the last bits and this test's bitwise comparison cannot hold.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn a_zero_imaginary_part_of_either_sign_leaves_asin_and_acos_on_the_real_axis() {
        for re in [2.0, -2.0] {
            let real_asin = Num::flo(re).asin();
            let real_acos = Num::flo(re).acos();
            for im in [0.0, -0.0] {
                let z = c(Num::flo(re), Num::flo(im));
                assert_eq!(z.asin(), real_asin, "asin of {re}{im:+}i");
                assert_eq!(z.acos(), real_acos, "acos of {re}{im:+}i");
            }
        }
        // And the side of the cut is the one the real argument has: below for x > 1, above
        // for x < -1.
        assert!(matches!(
            Num::flo(2.0).asin(),
            Num::InexactComplex(z) if z.im < 0.0
        ));
        assert!(matches!(
            Num::flo(-2.0).asin(),
            Num::InexactComplex(z) if z.im > 0.0
        ));
    }

    #[test]
    fn tan_saturates_to_plus_or_minus_i_instead_of_overflowing_to_nan() {
        for im in [30.0, 718.0, f64::INFINITY] {
            for (re, im, expected) in [(-913.0, -im, -1.0), (1.0, im, 1.0)] {
                assert!(matches!(
                    c(Num::flo(re), Num::flo(im)).tan(),
                    Num::InexactComplex(z) if z.re.abs() < 1e-12 && z.im == expected
                ));
            }
        }
        let moderate = c(Num::flo(1.0), Num::flo(19.0));
        assert!(matches!(moderate.tan(), Num::InexactComplex(z) if (z.im - 1.0).abs() < 1e-12));
    }

    #[test]
    fn complex_parts_polar_and_literals() {
        let z = c(Num::int(3), Num::int(4));
        assert_eq!(z.real_part(), Num::int(3));
        assert_eq!(z.imag_part(), Num::int(4));
        assert_eq!(z.magnitude(), Num::int(5));
        assert_eq!(Num::int(-2).imag_part(), Num::int(0));
        assert_eq!(Num::flo(-2.0).magnitude(), Num::flo(2.0));
        assert_eq!(Num::int(-1).angle(), Num::flo(PI));
        assert_eq!(Num::int(1).angle(), Num::int(0));
        assert_eq!(Num::polar(&Num::int(2), &Num::int(0)), Ok(Num::int(2)));
        assert!(matches!(
            Num::polar(&Num::int(1), &Num::flo(FRAC_PI_2)),
            Ok(Num::InexactComplex(z)) if z.re.abs() < 1e-15 && (z.im - 1.0).abs() < 1e-15
        ));
        assert!(matches!(
            Num::parse("1@2", 10),
            Some(Num::InexactComplex(_))
        ));
        assert_eq!(Num::parse("1@0", 10), Some(Num::int(1)));
        assert_eq!(Num::parse("1@", 10), None);
    }

    // Miri perturbs every transcendental `f64` result by a few ulps, and this test asserts
    // exact floats.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn exact_complex_powers_and_the_remaining_edges() {
        let z = c(Num::int(1), Num::int(2));
        assert_eq!(z.expt(&Num::int(2)), Ok(c(Num::int(-3), Num::int(4))));
        assert_eq!(z.expt(&Num::int(-2)), Ok(c(r(-3, 25), r(-4, 25))));
        assert_eq!(z.expt(&Num::int(0)), Ok(Num::int(1)));
        // An exact zero stays exact under any positive exponent and is an error under any
        // negative one; an inexact zero keeps IEEE.
        assert_eq!(Num::int(0).expt(&r(1, 2)), Ok(Num::int(0)));
        assert_eq!(
            Num::int(0).expt(&r(-1, 2)),
            Err(NumError::ZeroToNegativePower)
        );
        assert_eq!(
            Num::int(0).expt(&Num::flo(-1.0)),
            Err(NumError::ZeroToNegativePower)
        );
        assert_eq!(
            Num::flo(0.0).expt(&Num::int(-1)),
            Ok(Num::flo(f64::INFINITY))
        );
        assert_eq!(
            Num::int(0).expt(&Num::int(i64::MAX)),
            Ok(Num::int(0)),
            "an unreachably large power of zero needs no computation"
        );
        assert_eq!(
            c(Num::int(1), Num::int(1)).magnitude(),
            Num::flo(2f64.sqrt())
        );
        assert_eq!(r(-3, 4).magnitude(), r(3, 4));
        assert_eq!(Num::flo(-0.0).angle(), Num::flo(PI));
        assert_eq!(
            Num::polar(&c(Num::int(1), Num::int(1)), &Num::int(0)),
            Err(NumError::NotReal)
        );
        assert_eq!(Num::parse("@1", 10), None);
        assert_eq!(Num::parse("1@2@3", 10), None);
        assert_eq!(Num::parse("1@2i", 10), None);
    }
}

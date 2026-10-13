//! External representation of [`Num`]: `write`, `display` and `number->string` all print
//! through here.

use std::fmt;

use num_traits::{One, Signed, Zero};

use super::{BigRational, Num, NumError};

impl Num {
    /// The external representation in `radix`. Inexact numbers print only in radix 10:
    /// in radix 16 the exponent marker `e` would be a digit.
    pub fn to_string_radix(&self, radix: u32) -> Result<String, NumError> {
        if !matches!(radix, 2 | 8 | 10 | 16) {
            return Err(NumError::BadRadix);
        }
        if radix != 10 && !self.is_exact() {
            return Err(NumError::NotRepresentable);
        }
        let mut out = String::new();
        self.write_radix(radix, &mut out);
        Ok(out)
    }

    /// `radix` must be 2, 8, 10 or 16, and 10 unless `self` is exact.
    fn write_radix(&self, radix: u32, out: &mut String) {
        match self {
            Num::Int(i) => out.push_str(&i.to_str_radix(radix)),
            Num::Rat(r) => write_ratio(r, radix, out),
            Num::Flo(x) => out.push_str(&flonum_text(*x)),
            Num::ExactComplex(c) => {
                // An exact-zero real part is left out: `+2i`, not `0+2i`.
                if !c.re.is_zero() {
                    write_ratio(&c.re, radix, out);
                }
                out.push(if c.im.is_negative() { '-' } else { '+' });
                let magnitude = c.im.abs();
                if !magnitude.is_one() {
                    write_ratio(&magnitude, radix, out);
                }
                out.push('i');
            }
            Num::InexactComplex(c) => {
                out.push_str(&flonum_text(c.re));
                let im = flonum_text(c.im);
                // The infinities and NaN spell their own sign.
                if !im.starts_with(['+', '-']) {
                    out.push('+');
                }
                out.push_str(&im);
                out.push('i');
            }
        }
    }
}

impl fmt::Display for Num {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        self.write_radix(10, &mut out);
        f.write_str(&out)
    }
}

/// A ratio is a bare integer when its denominator is 1, so a complex part that is a whole
/// number prints without `/1`.
fn write_ratio(r: &BigRational, radix: u32, out: &mut String) {
    out.push_str(&r.numer().to_str_radix(radix));
    if !r.is_integer() {
        out.push('/');
        out.push_str(&r.denom().to_str_radix(radix));
    }
}

/// R7RS external representation of an inexact real: always distinguishable from an exact
/// integer, so `1.0` never prints as `1`.
pub fn flonum_text(x: f64) -> String {
    if x.is_nan() {
        return "+nan.0".to_string();
    }
    if x.is_infinite() {
        return if x > 0.0 { "+inf.0" } else { "-inf.0" }.to_string();
    }
    // Rust's `Display` never uses scientific notation, so 1e300 would print as three
    // hundred digits; switch to it outside a readable magnitude band.
    let s = if x != 0.0 && (x.abs() < 1e-10 || x.abs() >= 1e21) {
        exponent_form(x)
    } else {
        format!("{x}")
    };
    if s.contains(['.', 'e', 'E']) {
        s
    } else {
        format!("{s}.0")
    }
}

/// Shortest round-trip digits in exponent form, with a point in the mantissa and a sign on
/// the exponent (`1.0e+21`, `5.0e-324`). Std omits both, and the conformance suite's
/// precision tests accept `5.0e-324` but not `5e-324`.
fn exponent_form(x: f64) -> String {
    let s = format!("{x:e}");
    let Some((mantissa, exponent)) = s.split_once('e') else {
        return s;
    };
    let point = if mantissa.contains('.') { "" } else { ".0" };
    let sign = if exponent.starts_with('-') { "" } else { "+" };
    format!("{mantissa}{point}e{sign}{exponent}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flonum_spellings() {
        let f = |x: f64| Num::flo(x).to_string();
        assert_eq!(f(1.0), "1.0");
        assert_eq!(f(-0.0), "-0.0");
        assert_eq!(f(100.0), "100.0");
        assert_eq!(f(0.1), "0.1");
        assert_eq!(f(1e21), "1.0e+21");
        assert_eq!(f(1e300), "1.0e+300");
        assert_eq!(f(1e-11), "1.0e-11");
        assert_eq!(f(1.5e21), "1.5e+21");
        assert_eq!(f(1e20), "100000000000000000000.0");
        assert_eq!(f(1e-10), "0.0000000001");
        assert_eq!(f(5e-324), "5.0e-324");
        assert_eq!(f(f64::MAX), "1.7976931348623157e+308");
        assert_eq!(f(f64::NAN), "+nan.0");
        assert_eq!(f(f64::INFINITY), "+inf.0");
        assert_eq!(f(f64::NEG_INFINITY), "-inf.0");
        assert_eq!(f(0.1 + 0.2), "0.30000000000000004");
    }

    #[test]
    fn exact_and_complex_spellings() {
        let r = |n: i64, d: i64| Num::rational(n.into(), d.into()).unwrap();
        assert_eq!(r(1, 3).to_string(), "1/3");
        assert_eq!(r(-3, 2).to_string(), "-3/2");
        assert_eq!(Num::int(255).to_string_radix(16), Ok("ff".into()));
        assert_eq!(Num::int(-5).to_string_radix(2), Ok("-101".into()));
        assert_eq!(r(255, 16).to_string_radix(16), Ok("ff/10".into()));
        assert_eq!(
            Num::flo(1.5).to_string_radix(16),
            Err(NumError::NotRepresentable)
        );
        assert_eq!(Num::int(1).to_string_radix(3), Err(NumError::BadRadix));
        let c = |re: Num, im: Num| Num::rectangular(&re, &im).unwrap();
        assert_eq!(c(Num::int(1), Num::int(2)).to_string(), "1+2i");
        assert_eq!(c(Num::int(1), Num::int(-2)).to_string(), "1-2i");
        assert_eq!(c(Num::int(0), Num::int(1)).to_string(), "+i");
        assert_eq!(c(Num::int(0), Num::int(-1)).to_string(), "-i");
        assert_eq!(c(Num::int(0), Num::int(2)).to_string(), "+2i");
        assert_eq!(c(r(1, 2), r(3, 4)).to_string(), "1/2+3/4i");
        assert_eq!(c(Num::flo(0.0), Num::flo(1.0)).to_string(), "0.0+1.0i");
        assert_eq!(c(Num::flo(-2.5), Num::flo(0.0)).to_string(), "-2.5+0.0i");
        assert_eq!(
            c(Num::flo(f64::NEG_INFINITY), Num::flo(f64::INFINITY)).to_string(),
            "-inf.0+inf.0i"
        );
        assert_eq!(
            c(Num::flo(1.0), Num::flo(f64::NAN)).to_string(),
            "1.0+nan.0i"
        );
        assert_eq!(
            c(Num::int(10), Num::int(11)).to_string_radix(16),
            Ok("a+bi".into())
        );
    }

    /// Every `test-numeric-syntax` string in the suite: the printed form re-reads to an eqv number.
    #[test]
    fn suite_strings_round_trip() {
        const STRINGS: &[&str] = &[
            "1",
            "+1",
            "-1",
            "#i1",
            "#I1",
            "#i-1",
            "1.0",
            "1.",
            ".1",
            "-.1",
            "-.0",
            "-0.",
            "#i1.0",
            "#e1.0",
            "#e-.0",
            "#e-0.",
            "1e2",
            "1E2",
            "1s2",
            "1S2",
            "1f2",
            "1F2",
            "1d2",
            "1D2",
            "1l2",
            "1L2",
            "+nan.0",
            "+NAN.0",
            "+inf.0",
            "+InF.0",
            "-inf.0",
            "-iNF.0",
            "#i+nan.0",
            "#i+inf.0",
            "#i-inf.0",
            "1/2",
            "#e1/2",
            "10/2",
            "-1/2",
            "0/10",
            "#e0/10",
            "#i3/2",
            "1+2i",
            "1+2I",
            "1-2i",
            "-1+2i",
            "-1-2i",
            "+i",
            "0+i",
            "0+1i",
            "-i",
            "0-i",
            "0-1i",
            "+2i",
            "-2i",
            "1.0+2i",
            "1+2.0i",
            "1e2+1.0i",
            "1s2+1.0i",
            "1.0+1e2i",
            "1.0+1s2i",
            "1/2+3/4i",
            "0.5+3/4i",
            "+inf.0+inf.0i",
            "-inf.0+inf.0i",
            "-inf.0-inf.0i",
            "+inf.0-inf.0i",
            "#x11",
            "#X11",
            "#d11",
            "#D11",
            "#o11",
            "#O11",
            "#b11",
            "#B11",
            "#o7",
            "#xa",
            "#xA",
            "#xf",
            "#x-10",
            "#d-10",
            "#o-10",
            "#b-10",
            "#e#x10",
            "#i#x10",
            "#x#i10",
            "#i#x1/10",
            "#x#i1/10",
            "#d1.",
            "#d.1",
            "#x1e2",
            "#d1e2",
            "#x10/2",
            "#x11/2",
            "#d11/2",
            "#o11/2",
            "#b11/10",
            "#d1.0+1.0i",
            "#d10+11i",
        ];
        for s in STRINGS {
            let n = Num::parse(s, 10).unwrap_or_else(|| panic!("{s}"));
            let back =
                Num::parse(&n.to_string(), 10).unwrap_or_else(|| panic!("{s} printed as {n}"));
            assert!(n.eqv(&back), "{s}: {n} re-read as {back}");
        }
    }

    /// A flonum outside the plain-decimal band prints in exponent form, which must re-read to
    /// the same flonum.
    #[test]
    fn exponent_form_round_trips() {
        for x in [f64::MAX, f64::MIN_POSITIVE, 5e-324, 1e21, -1e300, 1.5e-11] {
            let n = Num::flo(x);
            let back =
                Num::parse(&n.to_string(), 10).unwrap_or_else(|| panic!("{x:e} printed as {n}"));
            assert!(n.eqv(&back), "{x:e} printed as {n}, re-read as {back}");
        }
        let z = Num::parse("1e21-1e-11i", 10).unwrap();
        let back = Num::parse(&z.to_string(), 10).unwrap_or_else(|| panic!("printed as {z}"));
        assert!(z.eqv(&back), "{z} re-read as {back}");
    }

    /// The suite's `test-precision` block: `number->string` must produce one of the accepted spellings.
    #[test]
    fn suite_precision_spellings() {
        let cases: &[(&str, &[&str])] = &[
            ("-1.7976931348623157e+308", &["-inf.0"]),
            (
                "4.940656458412465e-324",
                &["4.94065645841247e-324", "5.0e-324", "0.0"],
            ),
            (
                "9.881312916824931e-324",
                &["9.88131291682493e-324", "1.0e-323", "0.0"],
            ),
            ("1.48219693752374e-323", &["1.5e-323", "0.0"]),
            (
                "1.976262583364986e-323",
                &["1.97626258336499e-323", "2.0e-323", "0.0"],
            ),
            (
                "2.470328229206233e-323",
                &["2.47032822920623e-323", "2.5e-323", "0.0"],
            ),
            (
                "2.420921664622108e-322",
                &["2.42092166462211e-322", "2.4e-322", "0.0"],
            ),
            (
                "2.420921664622108e-320",
                &["2.42092166462211e-320", "2.421e-320", "0.0"],
            ),
            ("1.4489974452386991", &["1.4489975"]),
            (
                "0.14285714285714282",
                &["0.14285714285714288", "0.14285715"],
            ),
            ("1.7976931348623157e+308", &["+inf.0"]),
        ];
        for (s, alts) in cases {
            let printed = Num::parse(s, 10).unwrap().to_string();
            assert!(
                printed == *s || alts.contains(&printed.as_str()),
                "{s} printed as {printed}"
            );
        }
    }
}

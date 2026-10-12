//! The R7RS §7.1.1 number grammar, as a scanner over bytes.
//!
//! The scanner checks the character set itself before any crate sees a digit string:
//! `BigInt::parse_bytes` accepts `_` and `Ratio`'s parser accepts `1/-3`, and neither is a
//! number. Every radix a crate receives is known to be in range and every denominator to
//! be non-zero, so no user text can reach a crate assertion.

use super::{BigInt, Num};

/// The largest decimal exponent `#e` expands: past it `#e1e999999999` would build a number
/// with a billion digits from a dozen characters of source.
pub(crate) const MAX_EXACT_DECIMAL_EXPONENT: i64 = 100_000;

#[derive(Clone, Copy)]
enum Exactness {
    Exact,
    Inexact,
}

impl Num {
    /// Reads all of `text` as one number, or `None` if it is not one. A radix prefix in the
    /// text overrides `default_radix`, which must be 2, 8, 10 or 16: those are the radixes
    /// in which `i`, the imaginary unit, is not a digit.
    pub fn parse(text: &str, default_radix: u32) -> Option<Num> {
        if !matches!(default_radix, 2 | 8 | 10 | 16) {
            return None;
        }
        let mut rest = text.as_bytes();
        let mut radix = None;
        let mut exactness = None;
        while let [b'#', flag, tail @ ..] = rest {
            let first_of_its_kind = match flag.to_ascii_lowercase() {
                b'x' => set(&mut radix, 16),
                b'o' => set(&mut radix, 8),
                b'b' => set(&mut radix, 2),
                b'd' => set(&mut radix, 10),
                b'e' => set(&mut exactness, Exactness::Exact),
                b'i' => set(&mut exactness, Exactness::Inexact),
                _ => false,
            };
            if !first_of_its_kind {
                return None;
            }
            rest = tail;
        }
        let reader = Reader {
            radix: radix.unwrap_or(default_radix),
            exact: matches!(exactness, Some(Exactness::Exact)),
        };
        let value = reader.complex(rest)?;
        match exactness {
            Some(Exactness::Exact) => value.exact().ok(),
            Some(Exactness::Inexact) => Some(value.inexact()),
            None => Some(value),
        }
    }
}

/// Stores `value` unless the slot was already filled.
fn set<T>(slot: &mut Option<T>, value: T) -> bool {
    slot.replace(value).is_none()
}

/// The radix and exactness in force while one number is read. Under `#e` a decimal is read
/// as the rational it spells, because the nearest double has already rounded: `#e0.1` is
/// `1/10`, not `3602879701896397/36028797018963968`.
struct Reader {
    radix: u32,
    exact: bool,
}

impl Reader {
    /// `<real>`, `<real> <imaginary>` or `<imaginary>`, consuming all of `s`.
    fn complex(&self, s: &[u8]) -> Option<Num> {
        if let Some(im) = self.imaginary(s) {
            return Num::rectangular(&Num::int(0), &im).ok();
        }
        let (re, rest) = self.real(s)?;
        if rest.is_empty() {
            return Some(re);
        }
        let im = self.imaginary(rest)?;
        Num::rectangular(&re, &im).ok()
    }

    /// `<sign> <ureal>? i`, consuming all of `s`. The sign is required, and for `inf.0` and
    /// `nan.0` it is part of the real.
    fn imaginary(&self, s: &[u8]) -> Option<Num> {
        let [body @ .., b'i' | b'I'] = s else {
            return None;
        };
        match body {
            [b'+'] => Some(Num::int(1)),
            [b'-'] => Some(Num::int(-1)),
            [b'+' | b'-', ..] => match self.real(body)? {
                (im, []) => Some(im),
                _ => None,
            },
            _ => None,
        }
    }

    /// The longest `<real>` at the start of `s`, and what follows it. Because the scan is
    /// greedy a sign after an exponent marker belongs to the exponent, so `1e2+1.0i` splits
    /// at the second `+`.
    fn real<'a>(&self, s: &'a [u8]) -> Option<(Num, &'a [u8])> {
        let (negative, signed, unsigned) = match s {
            [b'+', rest @ ..] => (false, true, rest),
            [b'-', rest @ ..] => (true, true, rest),
            _ => (false, false, s),
        };
        if signed && let Some(rest) = strip_keyword(unsigned, b"inf.0") {
            let infinity = if negative {
                f64::NEG_INFINITY
            } else {
                f64::INFINITY
            };
            return Some((Num::flo(infinity), rest));
        }
        if signed && let Some(rest) = strip_keyword(unsigned, b"nan.0") {
            return Some((Num::flo(f64::NAN), rest));
        }
        self.unsigned_real(negative, unsigned)
    }

    /// An integer, a rational or a decimal, with the sign already read.
    fn unsigned_real<'a>(&self, negative: bool, s: &'a [u8]) -> Option<(Num, &'a [u8])> {
        let (int_digits, rest) = split_digits(s, self.radix);
        if let [b'/', after @ ..] = rest {
            let (denom_digits, rest) = split_digits(after, self.radix);
            let numer = self.integer(negative, int_digits)?;
            let denom = self.integer(false, denom_digits)?;
            return Some((Num::rational(numer, denom).ok()?, rest));
        }
        // Points and exponents exist only in radix 10, which is also the only radix where
        // the exponent markers are not digits.
        let decimal = self.radix == 10;
        let (fraction, rest) = match rest {
            [b'.', after @ ..] if decimal => {
                let (digits, rest) = split_digits(after, 10);
                (Some(digits), rest)
            }
            _ => (None, rest),
        };
        let fraction_digits = fraction.unwrap_or_default();
        if int_digits.is_empty() && fraction_digits.is_empty() {
            return None;
        }
        let (exponent, rest) = if decimal {
            exponent(rest)
        } else {
            (None, rest)
        };
        let value = if fraction.is_none() && exponent.is_none() {
            Num::int(self.integer(negative, int_digits)?)
        } else if self.exact {
            exact_decimal(negative, int_digits, fraction_digits, exponent)?
        } else {
            inexact_decimal(negative, int_digits, fraction_digits, exponent)?
        };
        Some((value, rest))
    }

    fn integer(&self, negative: bool, digits: &[u8]) -> Option<BigInt> {
        let magnitude = BigInt::parse_bytes(digits, self.radix)?;
        Some(if negative { -magnitude } else { magnitude })
    }
}

/// `<marker> <sign>? <digit>+`, the marker being any of `e s f d l` in either case. A
/// marker without digits is left unread so that the caller rejects the stray text.
fn exponent(s: &[u8]) -> (Option<i64>, &[u8]) {
    let [marker, after @ ..] = s else {
        return (None, s);
    };
    if !matches!(
        marker.to_ascii_lowercase(),
        b'e' | b's' | b'f' | b'd' | b'l'
    ) {
        return (None, s);
    }
    let (negative, after) = match after {
        [b'+', rest @ ..] => (false, rest),
        [b'-', rest @ ..] => (true, rest),
        _ => (false, after),
    };
    let (digits, rest) = split_digits(after, 10);
    if digits.is_empty() {
        return (None, s);
    }
    // Saturating: a magnitude past the cap is refused or overflows to infinity (or
    // underflows to zero) whatever its exact value.
    let magnitude = digits.iter().fold(0i64, |acc, &digit| {
        acc.saturating_mul(10)
            .saturating_add(i64::from(digit - b'0'))
    });
    (Some(if negative { -magnitude } else { magnitude }), rest)
}

/// The exact rational a radix-10 decimal spells: the digits on both sides of the point as
/// one integer, scaled by ten to the exponent less the number of fraction digits.
fn exact_decimal(
    negative: bool,
    int_digits: &[u8],
    fraction_digits: &[u8],
    exponent: Option<i64>,
) -> Option<Num> {
    let exponent = exponent.unwrap_or(0);
    if !(-MAX_EXACT_DECIMAL_EXPONENT..=MAX_EXACT_DECIMAL_EXPONENT).contains(&exponent) {
        return None;
    }
    let digits = [int_digits, fraction_digits].concat();
    let magnitude = BigInt::parse_bytes(&digits, 10)?;
    let mantissa = if negative { -magnitude } else { magnitude };
    let scale = exponent.checked_sub(i64::try_from(fraction_digits.len()).ok()?)?;
    let power = BigInt::from(10).pow(u32::try_from(scale.unsigned_abs()).ok()?);
    if scale >= 0 {
        Some(Num::int(mantissa * power))
    } else {
        Num::rational(mantissa, power).ok()
    }
}

/// The nearest double to a radix-10 decimal. The text is rebuilt from the scanned digits,
/// so only the shape `f64::from_str` documents reaches it, whatever the exponent marker was.
fn inexact_decimal(
    negative: bool,
    int_digits: &[u8],
    fraction_digits: &[u8],
    exponent: Option<i64>,
) -> Option<Num> {
    let text = format!(
        "{}{}.{}e{}",
        if negative { "-" } else { "" },
        String::from_utf8_lossy(int_digits),
        String::from_utf8_lossy(fraction_digits),
        exponent.unwrap_or(0),
    );
    text.parse::<f64>().ok().map(Num::flo)
}

/// Splits `s` after its leading run of digits in `radix`. The radix is always one of the
/// four `Num::parse` admits, so `is_digit` cannot panic.
fn split_digits(s: &[u8], radix: u32) -> (&[u8], &[u8]) {
    let len = s
        .iter()
        .take_while(|&&b| char::from(b).is_digit(radix))
        .count();
    s.split_at(len)
}

/// The rest of `s` after `keyword`, compared without regard to case.
fn strip_keyword<'a>(s: &'a [u8], keyword: &[u8]) -> Option<&'a [u8]> {
    let (head, rest) = s.split_at_checked(keyword.len())?;
    head.eq_ignore_ascii_case(keyword).then_some(rest)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(s: &str) -> Num {
        Num::parse(s, 10).unwrap_or_else(|| panic!("{s} should parse"))
    }
    fn r(n: i64, d: i64) -> Num {
        Num::rational(n.into(), d.into()).unwrap()
    }
    fn c(re: Num, im: Num) -> Num {
        Num::rectangular(&re, &im).unwrap()
    }

    #[test]
    fn integers_rationals_decimals() {
        assert_eq!(p("1"), Num::int(1));
        assert_eq!(p("+1"), Num::int(1));
        assert_eq!(p("-1"), Num::int(-1));
        assert_eq!(p("1/2"), r(1, 2));
        assert_eq!(p("10/2"), Num::int(5));
        assert_eq!(p("-1/2"), r(-1, 2));
        assert_eq!(p("0/10"), Num::int(0));
        assert_eq!(p("1.0"), Num::flo(1.0));
        assert_eq!(p("1."), Num::flo(1.0));
        assert_eq!(p(".1"), Num::flo(0.1));
        assert_eq!(p("-.1"), Num::flo(-0.1));
        assert!(matches!(p("-.0"), Num::Flo(x) if x == 0.0 && x.is_sign_negative()));
        assert!(matches!(p("-0."), Num::Flo(x) if x == 0.0 && x.is_sign_negative()));
        for s in [
            "1e2", "1E2", "1s2", "1S2", "1f2", "1F2", "1d2", "1D2", "1l2", "1L2",
        ] {
            assert_eq!(p(s), Num::flo(100.0), "{s}");
        }
        assert_eq!(p(".5e3"), Num::flo(500.0));
        assert_eq!(p("1.7976931348623157e308"), Num::flo(f64::MAX));
        assert_eq!(p("9007199254740993"), Num::int(9007199254740993i64));
        assert_eq!(
            p("123456789012345678901234567890"),
            Num::int("123456789012345678901234567890".parse::<BigInt>().unwrap())
        );
    }

    #[test]
    fn inf_nan_and_signs() {
        for s in ["+inf.0", "+InF.0"] {
            assert_eq!(p(s), Num::flo(f64::INFINITY), "{s}");
        }
        for s in ["-inf.0", "-iNF.0"] {
            assert_eq!(p(s), Num::flo(f64::NEG_INFINITY), "{s}");
        }
        for s in ["+nan.0", "+NAN.0", "-nan.0"] {
            assert!(p(s).is_nan(), "{s}");
        }
        for s in ["inf.0", "nan.0", "+inf", "+inf.", "+inf.1"] {
            assert_eq!(Num::parse(s, 10), None, "{s}");
        }
    }

    #[test]
    fn prefixes() {
        for s in ["#x11", "#X11"] {
            assert_eq!(p(s), Num::int(17), "{s}");
        }
        for s in ["#d11", "#D11"] {
            assert_eq!(p(s), Num::int(11));
        }
        for s in ["#o11", "#O11"] {
            assert_eq!(p(s), Num::int(9));
        }
        for s in ["#b11", "#B11"] {
            assert_eq!(p(s), Num::int(3));
        }
        assert_eq!(p("#o7"), Num::int(7));
        assert_eq!(p("#xa"), Num::int(10));
        assert_eq!(p("#xA"), Num::int(10));
        assert_eq!(p("#xf"), Num::int(15));
        assert_eq!(p("#x-10"), Num::int(-16));
        assert_eq!(p("#d-10"), Num::int(-10));
        assert_eq!(p("#o-10"), Num::int(-8));
        assert_eq!(p("#b-10"), Num::int(-2));
        for s in ["#i1", "#I1"] {
            assert_eq!(p(s), Num::flo(1.0));
        }
        assert_eq!(p("#i-1"), Num::flo(-1.0));
        assert_eq!(p("#i1.0"), Num::flo(1.0));
        assert_eq!(p("#e1.0"), Num::int(1));
        assert_eq!(p("#e-.0"), Num::int(0));
        assert_eq!(p("#e-0."), Num::int(0));
        assert_eq!(p("#e1.5"), r(3, 2));
        assert_eq!(p("#e0.1"), r(1, 10));
        assert_eq!(p("#e1.2"), r(6, 5));
        assert_eq!(p("#e1e30"), Num::int(BigInt::from(10).pow(30)));
        assert_eq!(p("#e1.5e-3"), r(3, 2000));
        assert!(p("#i+nan.0").is_nan());
        assert_eq!(p("#i+inf.0"), Num::flo(f64::INFINITY));
        assert_eq!(p("#i-inf.0"), Num::flo(f64::NEG_INFINITY));
        for s in ["#e#x10", "#x#e10"] {
            assert_eq!(p(s), Num::int(16));
        }
        for s in ["#i#x10", "#x#i10"] {
            assert_eq!(p(s), Num::flo(16.0));
        }
        for s in ["#i#x1/10", "#x#i1/10"] {
            assert_eq!(p(s), Num::flo(1.0 / 16.0));
        }
        assert_eq!(p("#e1/2"), r(1, 2));
        assert_eq!(p("#e0/10"), Num::int(0));
        assert_eq!(p("#i3/2"), Num::flo(1.5));
        assert_eq!(p("#d1."), Num::flo(1.0));
        assert_eq!(p("#d.1"), Num::flo(0.1));
        assert_eq!(p("#x1e2"), Num::int(0x1e2));
        assert_eq!(p("#d1e2"), Num::flo(100.0));
        assert_eq!(p("#x10/2"), Num::int(8));
        assert_eq!(p("#x11/2"), r(17, 2));
        assert_eq!(p("#d11/2"), r(11, 2));
        assert_eq!(p("#o11/2"), r(9, 2));
        assert_eq!(p("#b11/10"), r(3, 2));
        for s in ["#x1.5", "#e#e1", "#x#b1", "#q1", "#e", "#x", "#"] {
            assert_eq!(Num::parse(s, 10), None, "{s}");
        }
        assert_eq!(Num::parse("10", 16), Some(Num::int(16)));
        // an explicit prefix overrides the default radix
        assert_eq!(Num::parse("#d10", 16), Some(Num::int(10)));
        assert_eq!(Num::parse("1.5", 16), None);
    }

    #[test]
    fn complex_rectangular() {
        assert_eq!(p("1+2i"), c(Num::int(1), Num::int(2)));
        assert_eq!(p("1+2I"), c(Num::int(1), Num::int(2)));
        assert_eq!(p("1-2i"), c(Num::int(1), Num::int(-2)));
        assert_eq!(p("-1+2i"), c(Num::int(-1), Num::int(2)));
        assert_eq!(p("-1-2i"), c(Num::int(-1), Num::int(-2)));
        for s in ["+i", "0+i", "0+1i"] {
            assert_eq!(p(s), c(Num::int(0), Num::int(1)), "{s}");
        }
        for s in ["-i", "0-i", "0-1i"] {
            assert_eq!(p(s), c(Num::int(0), Num::int(-1)), "{s}");
        }
        assert_eq!(p("+2i"), c(Num::int(0), Num::int(2)));
        assert_eq!(p("-2i"), c(Num::int(0), Num::int(-2)));
        assert_eq!(p("1.0+2i"), c(Num::flo(1.0), Num::flo(2.0)));
        assert_eq!(p("1+2.0i"), c(Num::flo(1.0), Num::flo(2.0)));
        assert_eq!(p("1e2+1.0i"), c(Num::flo(100.0), Num::flo(1.0)));
        assert_eq!(p("1s2+1.0i"), c(Num::flo(100.0), Num::flo(1.0)));
        assert_eq!(p("1.0+1e2i"), c(Num::flo(1.0), Num::flo(100.0)));
        assert_eq!(p("1.0+1s2i"), c(Num::flo(1.0), Num::flo(100.0)));
        assert_eq!(p("1/2+3/4i"), c(r(1, 2), r(3, 4)));
        assert_eq!(p("0.5+3/4i"), c(Num::flo(0.5), Num::flo(0.75)));
        assert_eq!(
            p("+inf.0+inf.0i"),
            c(Num::flo(f64::INFINITY), Num::flo(f64::INFINITY))
        );
        assert_eq!(
            p("-inf.0+inf.0i"),
            c(Num::flo(f64::NEG_INFINITY), Num::flo(f64::INFINITY))
        );
        assert_eq!(
            p("-inf.0-inf.0i"),
            c(Num::flo(f64::NEG_INFINITY), Num::flo(f64::NEG_INFINITY))
        );
        assert_eq!(
            p("+inf.0-inf.0i"),
            c(Num::flo(f64::INFINITY), Num::flo(f64::NEG_INFINITY))
        );
        assert_eq!(p("#d1.0+1.0i"), c(Num::flo(1.0), Num::flo(1.0)));
        assert_eq!(p("#d10+11i"), c(Num::int(10), Num::int(11)));
        assert_eq!(p("3+0i"), Num::int(3));
        assert_eq!(p("-2.5+0.0i"), c(Num::flo(-2.5), Num::flo(0.0)));
        assert_eq!(p("3/2+i"), c(r(3, 2), Num::int(1)));
        for s in ["i", "1+i2", "1i", "+-i", "1+2", "1++2i", "1+2ii"] {
            assert_eq!(Num::parse(s, 10), None, "{s}");
        }
    }

    #[test]
    fn grammar_edges() {
        assert_eq!(p("+inf.0i"), c(Num::int(0), Num::flo(f64::INFINITY)));
        assert_eq!(p("1-inf.0i"), c(Num::flo(1.0), Num::flo(f64::NEG_INFINITY)));
        assert!(p("1+nan.0i").is_nan());
        assert_eq!(p("+1e-2i"), c(Num::int(0), Num::flo(0.01)));
        assert_eq!(p("1e+2"), Num::flo(100.0));
        assert_eq!(p("#xff+1i"), c(Num::int(255), Num::int(1)));
        assert_eq!(p("#x1e+2i"), c(Num::int(30), Num::int(2)));
        assert_eq!(p("#e1.5+2i"), c(r(3, 2), Num::int(2)));
        assert_eq!(p("#e0.1+0.1i"), c(r(1, 10), r(1, 10)));
        assert_eq!(p("#i1+2i"), c(Num::flo(1.0), Num::flo(2.0)));
        assert_eq!(p("#e1e-5"), r(1, 100_000));
        for s in [
            "#e+inf.0",
            "#e+nan.0",
            "#e1+inf.0i",
            "#e1e-100001",
            "1/",
            "/2",
            "1/2/3",
            "1/2.5",
            "1.5.5",
            "1e-2i",
            "1.e",
            "#b102",
            "#xg",
            "\u{ff11}",
            "1\u{e9}",
            "",
        ] {
            assert_eq!(Num::parse(s, 10), None, "{s}");
        }
        for radix in [0, 1, 3, 7, 18, 36, 37, u32::MAX] {
            assert_eq!(Num::parse("1", radix), None, "{radix}");
        }
    }

    #[test]
    fn rejects_what_the_crates_would_accept_and_never_hangs() {
        for s in [
            "1_0", "1/-3", "1/0", "#e1/0", "1 2", " 1", "1 ", "0x10", "Infinity", "NaN", "1e",
            "e5", ".", "+", "-", "...", "->x", "+soup+", "-in",
        ] {
            assert_eq!(Num::parse(s, 10), None, "{s}");
        }
        assert_eq!(p("1e999999999"), Num::flo(f64::INFINITY));
        assert_eq!(p("-1e999999999"), Num::flo(f64::NEG_INFINITY));
        assert_eq!(p("1e-999999999"), Num::flo(0.0));
        assert_eq!(Num::parse("#e1e999999999", 10), None);
        assert_eq!(
            Num::parse(&format!("#e1e{}", MAX_EXACT_DECIMAL_EXPONENT + 1), 10),
            None
        );
        assert!(Num::parse(&format!("#e1e{}", MAX_EXACT_DECIMAL_EXPONENT), 10).is_some());
    }
}

//! Differential test of the numeric tower against chibi-scheme.
//!
//! Random arithmetic expressions over the whole literal grammar are evaluated by ruse and
//! by `chibi-scheme`, and the two printed results are compared the way chibi's own
//! `test-equal?` does: exact results must be `eqv`, an inexact real expectation tolerates a
//! relative error below 1e-5, and a complex one is compared part by part. chibi's answer
//! is the expectation, so a ruse exact result against a chibi inexact one passes when the
//! two are numerically close.
//!
//! The test is a bug finder, not a gate. It is `#[ignore]`d, additionally returns early
//! unless `RUSE_CHIBI` is set, and runs in an advisory CI job, because chibi-scheme must be
//! on `PATH` and its answers differ between releases. Run it with
//! `RUSE_CHIBI=1 cargo test --test chibi_diff -- --ignored --nocapture`.
//!
//! chibi 0.12 is a reference with bugs of its own, and it extends several operations past
//! what R7RS defines, so [`DIVERGENCES`] lists the expression shapes where a disagreement is
//! expected, with the reason. A disagreement on an expression that matches an entry is
//! counted and excused; one that matches none is either a ruse bug or a divergence nobody
//! has justified yet.

// Integration tests are separate crates, so the crate-root `cfg_attr(test, allow(...))` in
// the library does not reach them. Panicking with context on a failed run or an unusable
// chibi, and printing the tally, are the point of this driver.
#![allow(clippy::panic)]
#![allow(clippy::print_stdout)]

use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::fmt;
use std::io::{Read, Write};
use std::process::{Command, Stdio};
use std::rc::Rc;
use std::thread;
use std::time::{Duration, Instant};

use proptest::prelude::*;
use proptest::sample::select;
use proptest::test_runner::{Config, TestError, TestRunner};
use ruse::Vm;
use ruse::num::Num;

/// chibi 0.12 segfaults on some inputs and could loop on others; a run that outlives this
/// counts as a crash.
const CHIBI_TIMEOUT: Duration = Duration::from_secs(10);

/// The relative error chibi's own `test` harness tolerates on an inexact expectation.
const EPSILON: f64 = 1e-5;

/// Every literal form the grammar accepts. Denormals are left out: chibi's reader loses
/// digits on them (`1e-320` reads a relative 1.1e-5 away from the correctly rounded value),
/// which would only add noise at the edge of the tolerance.
const LITERALS: &[&str] = &[
    "0",
    "1",
    "-1",
    "2",
    "7",
    "123456789012345678901234567890123456789",
    "-98765432109876543210987654321098765432",
    "1000000000000000000000000000000000000000",
    "1/2",
    "-3/4",
    "1/3",
    "22/7",
    "-5/6",
    "0.5",
    "-0.25",
    "3.14159",
    "1e10",
    "-2.5e-3",
    "6.02e23",
    "1e-10",
    "1e308",
    "1e400",
    "#e1.5",
    "#e1.2",
    "#e-0.75",
    "#e1e3",
    "#i1/3",
    "#i5",
    "#x1F",
    "#xff",
    "#xff/10",
    "#b101",
    "#o17",
    "#x-ff",
    "#e#x10",
    "#i#b101",
    "+inf.0",
    "-inf.0",
    "+nan.0",
    "-0.0",
    "0.0",
    "1+2i",
    "-3/4+5/6i",
    "3-4i",
    "1.5+2.5i",
    "-1.5-2.5i",
    "+i",
    "-i",
    "+2.5i",
    "1+2.0i",
    "1.0+2i",
    "#e1.5+2.5i",
];

/// The exponent of an `expt` is drawn from here rather than generated as a sub-expression,
/// so neither implementation is asked to build a bignum of unbounded size.
const EXPONENTS: &[&str] = &[
    "0", "1", "2", "3", "7", "-1", "-2", "-3", "1/2", "-1/2", "1/3", "3/2", "2/3", "0.5", "2.0",
    "-1.5",
];

const UNARY: &[&str] = &[
    "-",
    "abs",
    "floor",
    "ceiling",
    "round",
    "truncate",
    "sqrt",
    "exp",
    "log",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "exact",
    "inexact",
    "numerator",
    "denominator",
    "square",
    "real-part",
    "imag-part",
    "magnitude",
    "angle",
];

/// `expt` is generated separately, with a restricted exponent.
const BINARY: &[&str] = &[
    "+",
    "-",
    "*",
    "/",
    "quotient",
    "remainder",
    "modulo",
    "gcd",
    "lcm",
    "max",
    "min",
    "atan",
    "make-rectangular",
];

/// A generated expression, kept as a tree so shrinking removes whole sub-expressions and
/// [`DIVERGENCES`] can reason about the operands of a call.
#[derive(Clone, Debug)]
enum Expr {
    Literal(String),
    Call(&'static str, Vec<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(text) => f.write_str(text),
            Expr::Call(op, args) => {
                write!(f, "({op}")?;
                for arg in args {
                    write!(f, " {arg}")?;
                }
                f.write_str(")")
            }
        }
    }
}

impl Expr {
    /// This expression and every expression nested in it.
    fn subexpressions(&self) -> Vec<&Expr> {
        let mut found = vec![self];
        if let Expr::Call(_, args) = self {
            for arg in args {
                found.extend(arg.subexpressions());
            }
        }
        found
    }

    /// Whether some call in this expression satisfies `test`, given its operator and operands.
    fn any_call(&self, test: impl Fn(&str, &[Expr]) -> bool) -> bool {
        self.subexpressions().into_iter().any(|e| match e {
            Expr::Call(op, args) => test(op, args),
            Expr::Literal(_) => false,
        })
    }

    /// This expression with every sub-expression passed through chibi's `chk`.
    fn watched(&self) -> String {
        match self {
            Expr::Literal(text) => format!("(chk {text})"),
            Expr::Call(op, args) => {
                let args: String = args.iter().map(|a| format!(" {}", a.watched())).collect();
                format!("(chk ({op}{args}))")
            }
        }
    }
}

fn literal() -> impl Strategy<Value = String> {
    let small_integer = (-1000i64..=1000).prop_map(|n| n.to_string());
    let rational = (-200i64..=200, 1i64..=50).prop_map(|(n, d)| format!("{n}/{d}"));
    let decimal = (
        any::<bool>(),
        0u32..=1000,
        0u32..=9999,
        proptest::option::of(-8i32..=8),
    )
        .prop_map(|(negative, whole, fraction, exponent)| {
            let sign = if negative { "-" } else { "" };
            let exponent = exponent.map_or_else(String::new, |e| format!("e{e}"));
            format!("{sign}{whole}.{fraction:04}{exponent}")
        });
    prop_oneof![
        4 => select(LITERALS).prop_map(str::to_owned),
        1 => small_integer,
        1 => rational,
        1 => decimal,
    ]
}

fn expression() -> impl Strategy<Value = Expr> {
    literal()
        .prop_map(Expr::Literal)
        .prop_recursive(3, 24, 2, |inner| {
            prop_oneof![
                3 => (select(UNARY), inner.clone())
                    .prop_map(|(op, arg)| Expr::Call(op, vec![arg])),
                3 => (select(BINARY), inner.clone(), inner.clone())
                    .prop_map(|(op, a, b)| Expr::Call(op, vec![a, b])),
                1 => (inner, select(EXPONENTS))
                    .prop_map(|(base, exponent)| {
                        Expr::Call("expt", vec![base, Expr::Literal(exponent.to_owned())])
                    }),
            ]
        })
}

/// What an implementation answered.
#[derive(Debug, PartialEq)]
enum Outcome {
    Value(String),
    Error,
    /// chibi died on a signal or ran past the timeout; there is nothing to compare.
    Crashed,
}

impl fmt::Display for Outcome {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Outcome::Value(text) => f.write_str(text),
            Outcome::Error => f.write_str("error"),
            Outcome::Crashed => f.write_str("crashed"),
        }
    }
}

/// A capture buffer the test can read after handing the VM its writer.
#[derive(Clone, Default)]
struct Sink(Rc<RefCell<Vec<u8>>>);

impl Write for Sink {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.borrow_mut().extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

fn ruse_eval(src: &str) -> Outcome {
    let sink = Sink::default();
    let mut vm = Vm::with_output(Box::new(sink.clone()));
    match vm.eval_str(&format!("(write {src})")) {
        Ok(_) => Outcome::Value(String::from_utf8_lossy(&sink.0.borrow()).into_owned()),
        Err(_) => Outcome::Error,
    }
}

/// chibi prints the value of one expression with `-p`. It starts with only its own core
/// bindings, so the R7RS libraries are imported to make `exact`, `square`, `exp` and
/// `make-rectangular` visible. Its stderr is discarded: it warns about every rebinding
/// the imports cause.
fn chibi_run(program: &str) -> Outcome {
    let mut child = Command::new("chibi-scheme")
        .args([
            "-q",
            "-mscheme.base",
            "-mscheme.inexact",
            "-mscheme.complex",
            "-p",
            program,
        ])
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .unwrap_or_else(|e| panic!("cannot run chibi-scheme (is it on PATH?): {e}"));
    let Some(mut stdout) = child.stdout.take() else {
        panic!("chibi-scheme was spawned without a stdout pipe");
    };
    // Drained on its own thread so a result larger than the pipe buffer cannot stall the
    // child while this thread is polling for its exit.
    let reader = thread::spawn(move || {
        let mut text = String::new();
        let _ = stdout.read_to_string(&mut text);
        text
    });

    let deadline = Instant::now() + CHIBI_TIMEOUT;
    let status = loop {
        match child.try_wait() {
            Ok(Some(status)) => break Some(status),
            Ok(None) if Instant::now() >= deadline => {
                let _ = child.kill();
                let _ = child.wait();
                break None;
            }
            Ok(None) => thread::sleep(Duration::from_millis(5)),
            Err(e) => panic!("waiting for chibi-scheme failed: {e}"),
        }
    };
    let text = reader.join().unwrap_or_default();

    match status {
        // A signal death has no exit code.
        Some(status) if status.code().is_some() => {
            if status.success() && !text.trim().is_empty() {
                Outcome::Value(text.trim().to_owned())
            } else {
                Outcome::Error
            }
        }
        _ => Outcome::Crashed,
    }
}

/// Reads a number the way chibi writes it. chibi spells a NaN imaginary part with a doubled
/// sign: `+nan.0++nan.0i`.
fn parse_answer(text: &str) -> Option<Num> {
    Num::parse(&text.replace("++nan.0i", "+nan.0i"), 10)
}

/// chibi's answer to one expression. An exception is caught and written as `error`, because
/// left to chibi's own handler it takes about 1.7 s to report, ten times the cost of the
/// whole evaluation.
fn chibi_eval(src: &str) -> Outcome {
    match chibi_run(&format!("(guard (e (#t 'error)) {src})")) {
        Outcome::Value(text) if text == "error" => Outcome::Error,
        other => other,
    }
}

/// What ruse answers for an expression, as a number; `None` if it errors.
fn ruse_num(expr: &Expr) -> Option<Num> {
    match ruse_eval(&expr.to_string()) {
        Outcome::Value(text) => parse_answer(&text),
        _ => None,
    }
}

/// Whether ruse evaluates `expr` to a number satisfying `test`.
fn ruse_num_is(expr: &Expr, test: impl FnOnce(&Num) -> bool) -> bool {
    ruse_num(expr).is_some_and(|n| test(&n))
}

fn is_inexact_zero(n: &Num) -> bool {
    matches!(n, Num::Flo(x) if *x == 0.0)
}

/// A known difference between ruse and chibi that the test accepts.
struct Divergence {
    shape: &'static str,
    reason: &'static str,
    /// Whether the expression has the shape. It runs only on an expression the two
    /// implementations disagree about, so it may evaluate sub-expressions.
    applies: fn(&Expr) -> bool,
}

/// Disagreements on a matching expression are excused, so each entry has to say why the
/// difference is acceptable: nearly always a chibi bug or extension, once an accuracy limit
/// ruse shares.
const DIVERGENCES: &[Divergence] = &[
    Divergence {
        shape: "`expt` of a negative real to a non-integer power",
        reason: "R7RS defines it as e^(z log w), which is complex; chibi answers +nan.0",
        applies: negative_base_fractional_power,
    },
    Divergence {
        shape: "`expt` of a non-integer exact rational to a negative integer power",
        reason: "chibi answers 1, where ruse and R7RS give the reciprocal power",
        applies: rational_to_negative_power,
    },
    Divergence {
        shape: "`expt` of an exact zero to a negative or fractional power",
        reason: "ruse makes a negative power an error, as dividing by an exact zero is \
                 (R7RS §6.2.6), and a positive one an exact 0 ((expt 0 1/3) is 0); chibi answers \
                 +inf.0 and 0.0",
        applies: exact_zero_to_awkward_power,
    },
    Divergence {
        shape: "`expt` of a complex number to an integer power",
        reason: "ruse keeps an exact base exact and multiplies an inexact one; chibi computes \
                 in flonums, evidently through the polar form. (expt 1+2i 2) is -3+4i against \
                 -3.0+4.000000000000002i, a part that should vanish is rounding noise of a \
                 different size, and infinite parts turn into different NaNs",
        applies: complex_integer_power,
    },
    Divergence {
        shape: "`magnitude`, `log`, `sqrt`, `asin`, `acos`, `atan` or a complex `/` on a number \
                near the float limit",
        reason: "chibi's modulus is sqrt(re² + im²), and its complex functions overflow to \
                 +inf.0 or NaN in the same way above 1e154; ruse scales the computation",
        applies: operand_near_float_limit,
    },
    Divergence {
        shape: "`tan` of a number with a large imaginary part",
        reason: "the answer is ±i plus a real part of about e^(-2|im|), which chibi gets wrong \
                 in the digits that matter (2e-4 relative at |im| 14.6) and answers NaN from \
                 |im| 355; ruse computes it directly and saturates to ±i",
        applies: tan_of_large_imaginary_part,
    },
    Divergence {
        shape: "`asin` or `acos` of a number of large magnitude",
        reason: "chibi loses digits for a negative real below about -1e5 (a relative error of \
                 1e-7 there, 6e-5 at -2.4e6, which `exp` and `tan` magnify; (asin -1e8) is \
                 0.0+inf.0i), is wrong for an exact integer above 2^31 ((asin 3000000000) is \
                 0.785-22.2i) and for a complex argument above about 1e5; ruse has the closed \
                 form for a real, but `Complex::asin` and `acos` cancel away digits for a \
                 complex argument that large",
        applies: arc_of_large_argument,
    },
    Divergence {
        shape: "`sqrt` of an exact integer below -2^62",
        reason: "chibi answers the positive real root of a negative bignum, and an inexact one \
                 at that ((sqrt -10000000000000000000) is 3162277660.0)",
        applies: sqrt_of_negative_bignum,
    },
    Divergence {
        shape: "`atan` of an exact complex number",
        reason: "chibi is wrong for an exact argument with a non-integer part ((atan 2+3/2i) \
                 is 0.67+0.0i); ruse agrees with chibi's own answer for the inexact argument",
        applies: atan_of_exact_complex,
    },
    Divergence {
        shape: "two-argument `atan` with -0.0 as the second operand",
        reason: "ruse takes the quadrant from the signed zero as atan2 does ((atan +inf.0 -0.0) \
                 is π/2); chibi answers the opposite sign",
        applies: atan_with_negative_zero_abscissa,
    },
    Divergence {
        shape: "`max` or `min` of a NaN, of two zeros, or of operands that differ in exactness",
        reason: "R7RS leaves the first two open; ruse propagates a NaN and answers the second of \
                 two equal operands, so (max -0.0 0.0) is 0.0, where chibi skips the NaN and \
                 answers the first. chibi's comparison of a flonum with an exact integer is also \
                 wrong for some flonums ((< -0.0015093111669858105 288) is #f)",
        applies: awkward_extremum,
    },
    Divergence {
        shape: "`sqrt` of a complex number with a negative real part",
        reason: "chibi's real part comes out as 0.0 when the imaginary part is small against a \
                 negative real part ((sqrt -9999999969.5-2.5i)), where ruse keeps 1.25e-5",
        applies: sqrt_left_of_the_imaginary_axis,
    },
    Divergence {
        shape: "`exact`, `numerator` or `denominator` of a transcendental result",
        reason: "they expose the last bits of a flonum, and chibi's complex formulas are not \
                 correctly rounded ((acos -678.6712) has imaginary part 7.213283406471159 there, \
                 7.213283406527076 in ruse)",
        applies: exact_view_of_transcendental,
    },
    Divergence {
        shape: "`+`, `-` or `*` with an exact zero operand",
        reason: "chibi treats an exact 0 as absorbing in `*` ((* 0 +inf.0) is 0) and as an \
                 identity in `+` and `-` ((+ -0.0 0) is -0.0); ruse promotes it to 0.0 and \
                 follows IEEE",
        applies: arithmetic_with_exact_zero,
    },
    Divergence {
        shape: "`gcd` or `lcm` of operands that differ in exactness",
        reason: "ruse makes the result inexact whenever an operand is; chibi's Euclid loop \
                 answers whichever operand it ends on, so (gcd 4.0 -2) is the exact 2",
        applies: gcd_of_mixed_exactness,
    },
    Divergence {
        shape: "`lcm` of zeros",
        reason: "ruse answers 0, as for any zero operand; chibi divides by the zero gcd and \
                 signals an error",
        applies: lcm_of_zeros,
    },
    Divergence {
        shape: "`quotient`, `remainder` or `modulo` by an exact 1",
        reason: "chibi answers the dividend itself for a quotient by 1, so (quotient -0.0 1) \
                 keeps the sign of the zero, and an exact 0 for a remainder by 1 even from an \
                 inexact dividend; ruse's results are inexact with the dividend",
        applies: division_by_one,
    },
    Divergence {
        shape: "`abs` of -0.0",
        reason: "ruse clears the sign as fabs does; chibi's (abs -0.0) is -0.0, which `angle` \
                 and a division then expose",
        applies: abs_of_negative_zero,
    },
    Divergence {
        shape: "`angle` of an exact non-negative real",
        reason: "ruse answers an exact 0, which makes a later division by it an error; chibi \
                 answers 0.0",
        applies: angle_of_exact_non_negative,
    },
    Divergence {
        shape: "a complex number divided by an inexact zero",
        reason: "ruse divides each part, so (/ 1+i 0.0) is +inf.0+inf.0i; chibi answers NaN",
        applies: complex_over_inexact_zero,
    },
    Divergence {
        shape: "an operation R7RS restricts to reals, integers or rationals given other operands",
        reason: "ruse signals the domain error; chibi extends the operation (`(quotient 1.5 1)` \
                 is 1.5, `(numerator 1+i)` is 1+i)",
        applies: operand_outside_domain,
    },
    Divergence {
        shape: "a literal that chibi reads or converts to a flonum differently",
        reason: "chibi's reader is not correctly rounded (6.02e23 reads as 6.0200000000000006e23, \
                 #e1.2 as 1199999999999999/1000000000000000) and it converts an exact integer \
                 above 2^53 to a flonum an ulp off ((inexact 10^39) is 1.0000000000000001e39); \
                 numerator, denominator and sin of a large argument expose the difference",
        applies: chibi_misreads_a_literal,
    },
    Divergence {
        shape: "a complex number whose parts differ in exactness",
        reason: "ruse coerces both parts to one exactness; chibi keeps them apart, so \
                 `(real-part 1+2.0i)` is exact there and `(square +2.5i)` is real",
        applies: chibi_keeps_parts_apart,
    },
];

/// The first divergence the expression falls under, if any.
fn divergence(expr: &Expr) -> Option<&'static Divergence> {
    DIVERGENCES.iter().find(|d| (d.applies)(expr))
}

/// Whether chibi and ruse give a literal of the expression different values, either as read
/// or after `inexact`. All the probes share one chibi run; a probe chibi cannot evaluate
/// answers `#f`, which is not a number and so never counts.
fn chibi_misreads_a_literal(expr: &Expr) -> bool {
    let probes: Vec<String> = expr
        .subexpressions()
        .into_iter()
        .filter_map(|e| match e {
            Expr::Literal(text) => Some(text),
            Expr::Call(..) => None,
        })
        .flat_map(|text| [text.clone(), format!("(inexact {text})")])
        .collect();
    let guarded: String = probes
        .iter()
        .map(|probe| format!(" (guard (e (#t #f)) {probe})"))
        .collect();
    let Outcome::Value(answers) = chibi_run(&format!("(list{guarded})")) else {
        return false;
    };
    let answers = answers.trim_start_matches('(').trim_end_matches(')');
    probes
        .iter()
        .zip(answers.split_whitespace())
        .any(
            |(probe, chibi)| match (ruse_eval(probe), parse_answer(chibi)) {
                (Outcome::Value(ruse), Some(chibi)) => {
                    parse_answer(&ruse).is_some_and(|ruse| !ruse.eqv(&chibi))
                }
                _ => false,
            },
        )
}

fn negative_base_fractional_power(expr: &Expr) -> bool {
    expr.any_call(|op, args| match (op, args) {
        ("expt", [base, Expr::Literal(exponent)]) => {
            Num::parse(exponent, 10).is_some_and(|n| !n.is_integer())
                && ruse_num_is(base, |n| n.sign() == Ok(Some(Ordering::Less)))
        }
        _ => false,
    })
}

fn rational_to_negative_power(expr: &Expr) -> bool {
    expr.any_call(|op, args| match (op, args) {
        ("expt", [base, Expr::Literal(exponent)]) => {
            Num::parse(exponent, 10)
                .is_some_and(|n| n.is_exact_integer() && n.sign() == Ok(Some(Ordering::Less)))
                && ruse_num_is(base, |n| matches!(n, Num::Rat(_)))
        }
        _ => false,
    })
}

fn exact_zero_to_awkward_power(expr: &Expr) -> bool {
    expr.any_call(|op, args| match (op, args) {
        ("expt", [base, Expr::Literal(exponent)]) => {
            Num::parse(exponent, 10)
                .is_some_and(|n| !n.is_integer() || n.sign() == Ok(Some(Ordering::Less)))
                && ruse_num_is(base, |n| n.is_exact() && n.is_zero())
        }
        _ => false,
    })
}

fn complex_integer_power(expr: &Expr) -> bool {
    expr.any_call(|op, args| match (op, args) {
        ("expt", [base, Expr::Literal(exponent)]) => {
            Num::parse(exponent, 10).is_some_and(|n| n.is_integer())
                && ruse_num_is(base, |n| !n.is_real())
        }
        _ => false,
    })
}

fn operand_near_float_limit(expr: &Expr) -> bool {
    let huge = |arg: &Expr| {
        ruse_num_is(arg, |n| {
            [n.real_part(), n.imag_part()]
                .iter()
                .any(|part| part.to_f64().is_some_and(|x| x.abs() > 1e154))
        })
    };
    expr.any_call(|op, args| match (op, args) {
        ("magnitude" | "log" | "sqrt" | "asin" | "acos" | "atan", [arg]) => huge(arg),
        ("/", _) => args.iter().any(huge) && args.iter().any(|a| ruse_num_is(a, |n| !n.is_real())),
        _ => false,
    })
}

fn tan_of_large_imaginary_part(expr: &Expr) -> bool {
    expr.any_call(|op, args| match (op, args) {
        ("tan", [arg]) => ruse_num_is(arg, |n| {
            n.imag_part().to_f64().is_some_and(|y| y.abs() >= 10.0)
        }),
        _ => false,
    })
}

fn arc_of_large_argument(expr: &Expr) -> bool {
    expr.any_call(|op, args| match (op, args) {
        ("asin" | "acos", [arg]) => ruse_num_is(arg, |n| match n.to_f64() {
            Some(x) => x < -1e5 || (n.is_exact_integer() && x.abs() > 2f64.powi(31)),
            None => [n.real_part(), n.imag_part()]
                .iter()
                .any(|part| part.to_f64().is_some_and(|x| x.abs() > 1e5)),
        }),
        _ => false,
    })
}

fn sqrt_of_negative_bignum(expr: &Expr) -> bool {
    expr.any_call(|op, args| match (op, args) {
        ("sqrt", [arg]) => ruse_num_is(arg, |n| {
            n.is_exact_integer() && n.to_f64().is_some_and(|x| x <= -(2f64.powi(62)))
        }),
        _ => false,
    })
}

fn atan_of_exact_complex(expr: &Expr) -> bool {
    expr.any_call(|op, args| match (op, args) {
        ("atan", [arg]) => ruse_num_is(arg, |n| matches!(n, Num::ExactComplex(_))),
        _ => false,
    })
}

fn atan_with_negative_zero_abscissa(expr: &Expr) -> bool {
    expr.any_call(|op, args| match (op, args) {
        ("atan", [_, x]) => ruse_num_is(
            x,
            |n| matches!(n, Num::Flo(z) if *z == 0.0 && z.is_sign_negative()),
        ),
        _ => false,
    })
}

fn awkward_extremum(expr: &Expr) -> bool {
    expr.any_call(|op, args| {
        let exact = |a: &Expr| ruse_num_is(a, |n| n.is_exact());
        let inexact = |a: &Expr| ruse_num_is(a, |n| !n.is_exact());
        matches!(op, "max" | "min")
            && (args.iter().any(|a| ruse_num_is(a, Num::is_nan))
                || args.iter().all(|a| ruse_num_is(a, Num::is_zero))
                || (args.iter().any(exact) && args.iter().any(inexact)))
    })
}

fn sqrt_left_of_the_imaginary_axis(expr: &Expr) -> bool {
    expr.any_call(|op, args| match (op, args) {
        ("sqrt", [arg]) => ruse_num_is(arg, |n| {
            !n.is_real() && n.real_part().sign() == Ok(Some(Ordering::Less))
        }),
        _ => false,
    })
}

fn exact_view_of_transcendental(expr: &Expr) -> bool {
    const TRANSCENDENTAL: &[&str] = &[
        "exp",
        "log",
        "sin",
        "cos",
        "tan",
        "asin",
        "acos",
        "atan",
        "expt",
        "sqrt",
        "magnitude",
        "angle",
    ];
    expr.any_call(|op, args| {
        matches!(op, "exact" | "numerator" | "denominator")
            && args
                .iter()
                .any(|arg| arg.any_call(|inner, _| TRANSCENDENTAL.contains(&inner)))
    })
}

fn arithmetic_with_exact_zero(expr: &Expr) -> bool {
    expr.any_call(|op, args| {
        matches!(op, "+" | "-" | "*")
            && args
                .iter()
                .any(|a| ruse_num_is(a, |n| n.is_exact() && n.is_zero()))
    })
}

fn gcd_of_mixed_exactness(expr: &Expr) -> bool {
    expr.any_call(|op, args| {
        let exact = |a: &Expr| ruse_num_is(a, |n| n.is_exact());
        let inexact = |a: &Expr| ruse_num_is(a, |n| !n.is_exact());
        matches!(op, "gcd" | "lcm") && args.iter().any(exact) && args.iter().any(inexact)
    })
}

fn lcm_of_zeros(expr: &Expr) -> bool {
    expr.any_call(|op, args| op == "lcm" && args.iter().all(|a| ruse_num_is(a, Num::is_zero)))
}

fn division_by_one(expr: &Expr) -> bool {
    expr.any_call(|op, args| match (op, args) {
        ("quotient" | "remainder" | "modulo", [_, divisor]) => {
            ruse_num_is(divisor, |n| n.eqv(&Num::int(1)))
        }
        _ => false,
    })
}

fn abs_of_negative_zero(expr: &Expr) -> bool {
    expr.any_call(|op, args| match (op, args) {
        ("abs", [arg]) => ruse_num_is(
            arg,
            |n| matches!(n, Num::Flo(z) if *z == 0.0 && z.is_sign_negative()),
        ),
        _ => false,
    })
}

fn angle_of_exact_non_negative(expr: &Expr) -> bool {
    expr.any_call(|op, args| match (op, args) {
        ("angle", [arg]) => ruse_num_is(arg, |n| {
            n.is_exact() && n.is_real() && n.sign() != Ok(Some(Ordering::Less))
        }),
        _ => false,
    })
}

fn complex_over_inexact_zero(expr: &Expr) -> bool {
    expr.any_call(|op, args| match (op, args) {
        ("/", [dividend, divisor]) => {
            ruse_num_is(dividend, |n| !n.is_real()) && ruse_num_is(divisor, is_inexact_zero)
        }
        _ => false,
    })
}

/// What an operation's arguments must be, per R7RS.
enum Domain {
    Real,
    Rational,
    Integer,
}

fn domain_of(op: &str, arity: usize) -> Option<Domain> {
    match (op, arity) {
        ("atan", 2) => Some(Domain::Real),
        (
            "floor" | "ceiling" | "round" | "truncate" | "abs" | "max" | "min" | "make-rectangular",
            _,
        ) => Some(Domain::Real),
        ("numerator" | "denominator", _) => Some(Domain::Rational),
        ("quotient" | "remainder" | "modulo" | "gcd" | "lcm", _) => Some(Domain::Integer),
        _ => None,
    }
}

fn operand_outside_domain(expr: &Expr) -> bool {
    expr.any_call(|op, args| {
        let Some(domain) = domain_of(op, args.len()) else {
            return false;
        };
        args.iter().any(|arg| {
            ruse_num_is(arg, |n| match domain {
                Domain::Real => !n.is_real(),
                Domain::Rational => !n.is_rational(),
                Domain::Integer => !n.is_integer(),
            })
        })
    })
}

/// Evaluates the expression in chibi with every sub-expression checked for a complex number
/// whose real and imaginary parts differ in exactness. An error ends the evaluation but
/// still reports what was seen, since the mixed value may be what caused it.
fn chibi_keeps_parts_apart(expr: &Expr) -> bool {
    let program = format!(
        "(let ((mixed #f)) \
           (define (chk x) \
             (if (and (not (real? x)) \
                      (not (eq? (exact? (real-part x)) (exact? (imag-part x))))) \
                 (set! mixed #t)) \
             x) \
           (guard (e (#t mixed)) {} mixed))",
        expr.watched()
    );
    chibi_run(&program) == Outcome::Value("#t".to_owned())
}

/// chibi's `approx-equal?`: the smaller magnitude is measured against the larger, and
/// against an absolute epsilon when it is zero. A NaN or a pair of opposite signs never
/// qualifies.
fn approx_equal(a: f64, b: f64) -> bool {
    let (small, large) = if a.abs() > b.abs() { (b, a) } else { (a, b) };
    if small == 0.0 {
        large.abs() < EPSILON
    } else {
        ((small - large) / large).abs() < EPSILON
    }
}

/// chibi's `test-equal?` with `expect` as chibi's answer.
fn agrees(expect: &Num, got: &Num) -> bool {
    if expect.eqv(got) {
        return true;
    }
    if expect.is_real() {
        return !expect.is_exact()
            && got.is_real()
            && match (expect.to_f64(), got.to_f64()) {
                (Some(a), Some(b)) => approx_equal(a, b),
                _ => false,
            };
    }
    !got.is_real()
        && agrees(&expect.real_part(), &got.real_part())
        && agrees(&expect.imag_part(), &got.imag_part())
}

/// Why the two answers differ, or `Ok` when they agree.
fn judge(ruse: &Outcome, chibi: &Outcome) -> Result<(), String> {
    match (ruse, chibi) {
        (Outcome::Error, Outcome::Error) => Ok(()),
        (Outcome::Value(_), Outcome::Error) | (Outcome::Error, Outcome::Value(_)) => {
            Err("only one implementation reported an error".to_owned())
        }
        (Outcome::Value(ruse_text), Outcome::Value(chibi_text)) => {
            let Some(expect) = parse_answer(chibi_text) else {
                return Err("chibi's answer is not a number ruse can read".to_owned());
            };
            let Some(got) = parse_answer(ruse_text) else {
                return Err("ruse's answer is not a number ruse can read".to_owned());
            };
            if agrees(&expect, &got) {
                Ok(())
            } else {
                Err("the answers differ".to_owned())
            }
        }
        (Outcome::Crashed, _) | (_, Outcome::Crashed) => Ok(()),
    }
}

#[derive(Default)]
struct Tally {
    cases: usize,
    agreed: usize,
    excused: BTreeMap<&'static str, usize>,
    crashed: Vec<String>,
}

impl Tally {
    fn report(&self) {
        let excused: usize = self.excused.values().sum();
        println!(
            "chibi differential: {} cases, {} agreed, {excused} disagreements excused by known \
             divergences, {} skipped after a chibi crash",
            self.cases,
            self.agreed,
            self.crashed.len()
        );
        for d in DIVERGENCES {
            if let Some(count) = self.excused.get(d.shape) {
                println!("  excused {count:>3}  {}: {}", d.shape, d.reason);
            }
        }
        for expr in &self.crashed {
            println!("  chibi crashed on {expr}");
        }
    }
}

#[test]
#[ignore = "needs chibi-scheme on PATH; set RUSE_CHIBI=1"]
fn tower_agrees_with_chibi() {
    if std::env::var_os("RUSE_CHIBI").is_none() {
        return;
    }

    let tally = RefCell::new(Tally::default());
    // No regression file: the failing input is in the panic below, and a file written next
    // to the sources would be untracked clutter.
    let config = Config {
        cases: 300,
        max_shrink_iters: 200,
        failure_persistence: None,
        ..Config::default()
    };
    let result = TestRunner::new(config).run(&expression(), |expr| {
        let mut tally = tally.borrow_mut();
        tally.cases += 1;
        let src = expr.to_string();
        let ruse = ruse_eval(&src);
        let chibi = chibi_eval(&src);
        if chibi == Outcome::Crashed {
            tally.crashed.push(src);
            return Ok(());
        }
        match judge(&ruse, &chibi) {
            Ok(()) => {
                tally.agreed += 1;
                Ok(())
            }
            Err(why) => match divergence(&expr) {
                Some(known) => {
                    *tally.excused.entry(known.shape).or_default() += 1;
                    Ok(())
                }
                None => Err(TestCaseError::fail(format!(
                    "{why}\n  expression: {src}\n  ruse:  {ruse}\n  chibi: {chibi}"
                ))),
            },
        }
    });

    tally.borrow().report();
    if let Err(e) = result {
        match e {
            TestError::Fail(reason, expr) => panic!("{reason}\n  minimal input: {expr}"),
            TestError::Abort(reason) => panic!("the runner aborted: {reason}"),
        }
    }
}

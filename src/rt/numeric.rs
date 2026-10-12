//! The R7RS §6.2.6 numeric procedures that have no opcode of their own.
//!
//! Each native is a thin shell over [`Num`]: unbox the operands, run the one implementation
//! in [`crate::num`], box the result. [`unary`] and [`binary`] carry that contract for
//! natives that return a number; the predicates and the string conversions read their
//! operands through [`operand`] directly.

use std::cmp::Ordering;

use crate::num::{Num, NumError, NumResult};
use crate::rt;
use crate::rt::arith::{binary, num_err, operand, unary, unbox};
use crate::rt::prims::{NativeCtx, NativeFn, arg};
use crate::rt::wrong_type;
use crate::value::Value;
use crate::value::object::Str;
use crate::vm::error::VmError;

/// The natives this module installs, appended to the table after the literal entries.
/// Order is append-only: later milestones add below, never between.
pub(crate) const DEFS: &[(&str, usize, Option<usize>, NativeFn)] = &[
    ("complex?", 1, Some(1), n_complex_p),
    ("real?", 1, Some(1), n_real_p),
    ("rational?", 1, Some(1), n_rational_p),
    ("exact?", 1, Some(1), n_exact_p),
    ("inexact?", 1, Some(1), n_inexact_p),
    ("exact-integer?", 1, Some(1), n_exact_integer_p),
    ("finite?", 1, Some(1), n_finite_p),
    ("infinite?", 1, Some(1), n_infinite_p),
    ("nan?", 1, Some(1), n_nan_p),
    ("positive?", 1, Some(1), n_positive_p),
    ("negative?", 1, Some(1), n_negative_p),
    ("odd?", 1, Some(1), n_odd_p),
    ("even?", 1, Some(1), n_even_p),
    ("max", 1, None, n_max),
    ("min", 1, None, n_min),
    ("/", 1, None, n_div),
    ("abs", 1, Some(1), n_abs),
    ("quotient", 2, Some(2), n_quotient),
    ("remainder", 2, Some(2), n_remainder),
    ("modulo", 2, Some(2), n_modulo),
    ("floor-quotient", 2, Some(2), n_floor_quotient),
    ("floor-remainder", 2, Some(2), n_floor_remainder),
    ("truncate-quotient", 2, Some(2), n_truncate_quotient),
    ("truncate-remainder", 2, Some(2), n_truncate_remainder),
    ("gcd", 0, None, n_gcd),
    ("lcm", 0, None, n_lcm),
    ("numerator", 1, Some(1), n_numerator),
    ("denominator", 1, Some(1), n_denominator),
    ("floor", 1, Some(1), n_floor),
    ("ceiling", 1, Some(1), n_ceiling),
    ("truncate", 1, Some(1), n_truncate),
    ("round", 1, Some(1), n_round),
    ("rationalize", 2, Some(2), n_rationalize),
    ("square", 1, Some(1), n_square),
    ("exact", 1, Some(1), n_exact),
    ("inexact", 1, Some(1), n_inexact),
    ("exact->inexact", 1, Some(1), n_inexact),
    ("inexact->exact", 1, Some(1), n_exact),
    ("number->string", 1, Some(2), n_number_to_string),
    ("string->number", 1, Some(2), n_string_to_number),
];

// ---------------------------------------------------------------- predicates

/// A type predicate accepts any object: a non-number is simply not one.
fn is_num(ctx: &NativeCtx<'_>, args: &[Value], test: fn(&Num) -> bool) -> Result<Value, VmError> {
    Ok(Value::boolean(
        unbox(ctx.heap, arg(args, 0)).is_some_and(|n| test(&n)),
    ))
}

fn n_complex_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    is_num(ctx, args, |_| true)
}

fn n_real_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    is_num(ctx, args, Num::is_real)
}

fn n_rational_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    is_num(ctx, args, Num::is_rational)
}

fn n_exact_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    is_num(ctx, args, Num::is_exact)
}

fn n_inexact_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    is_num(ctx, args, |n| !n.is_exact())
}

fn n_exact_integer_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    is_num(ctx, args, Num::is_exact_integer)
}

fn n_finite_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    is_num(ctx, args, Num::is_finite)
}

fn n_infinite_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    is_num(ctx, args, Num::is_infinite)
}

fn n_nan_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    is_num(ctx, args, Num::is_nan)
}

/// Whether the real operand lies on `side` of zero; a NaN lies on neither.
fn sign_is(
    ctx: &NativeCtx<'_>,
    op: &'static str,
    args: &[Value],
    side: Ordering,
) -> Result<Value, VmError> {
    let sign = operand(ctx.heap, op, arg(args, 0))?
        .sign()
        .map_err(|e| num_err(op, e))?;
    Ok(Value::boolean(sign == Some(side)))
}

fn n_positive_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    sign_is(ctx, "positive?", args, Ordering::Greater)
}

fn n_negative_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    sign_is(ctx, "negative?", args, Ordering::Less)
}

/// Parity through the remainder by two rather than a bit test, so an inexact integer
/// answers too: `(even? 2.0)` is `#t`.
fn is_even(ctx: &NativeCtx<'_>, op: &'static str, args: &[Value]) -> Result<bool, VmError> {
    let r = operand(ctx.heap, op, arg(args, 0))?
        .remainder(&Num::int(2))
        .map_err(|e| num_err(op, e))?;
    Ok(r.is_zero())
}

fn n_odd_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(Value::boolean(!is_even(ctx, "odd?", args)?))
}

fn n_even_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(Value::boolean(is_even(ctx, "even?", args)?))
}

// ---------------------------------------------------------------- folds

/// Folds `f` over the arguments from the first, in `Num` space so only the final result is
/// boxed. A lone argument is folded with itself — every operation passed here is
/// idempotent — so `f`'s domain check still runs: `(gcd 5.5)` and `(max 1+i)` are errors
/// rather than echoes.
fn fold(
    ctx: &mut NativeCtx<'_>,
    op: &'static str,
    args: &[Value],
    f: fn(&Num, &Num) -> NumResult,
) -> Result<Value, VmError> {
    let mut acc = operand(ctx.heap, op, arg(args, 0))?;
    let rest = if args.len() == 1 { args } else { &args[1..] };
    for &v in rest {
        let y = operand(ctx.heap, op, v)?;
        acc = f(&acc, &y).map_err(|e| num_err(op, e))?;
    }
    Ok(ctx.heap.number(acc))
}

fn n_max(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    fold(ctx, "max", args, Num::max)
}

fn n_min(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    fold(ctx, "min", args, Num::min)
}

fn n_gcd(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    if args.is_empty() {
        return Ok(ctx.heap.integer(0));
    }
    fold(ctx, "gcd", args, Num::gcd)
}

fn n_lcm(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    if args.is_empty() {
        return Ok(ctx.heap.integer(1));
    }
    fold(ctx, "lcm", args, Num::lcm)
}

/// `(/ x)` is the reciprocal; longer calls divide left to right, like `-`.
fn n_div(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    if args.len() == 1 {
        return unary(ctx.heap, "/", arg(args, 0), |x| Num::int(1).div(x));
    }
    let mut acc = arg(args, 0);
    for &x in &args[1..] {
        acc = rt::arith::div(ctx.heap, acc, x)?;
    }
    Ok(acc)
}

// ---------------------------------------------------------------- integer division

fn n_quotient(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    rt::arith::quot(ctx.heap, arg(args, 0), arg(args, 1))
}

fn n_remainder(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    binary(
        ctx.heap,
        "remainder",
        arg(args, 0),
        arg(args, 1),
        Num::remainder,
    )
}

fn n_modulo(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    binary(ctx.heap, "modulo", arg(args, 0), arg(args, 1), Num::modulo)
}

fn n_floor_quotient(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    let (a, b) = (arg(args, 0), arg(args, 1));
    binary(ctx.heap, "floor-quotient", a, b, Num::floor_quotient)
}

fn n_floor_remainder(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    let (a, b) = (arg(args, 0), arg(args, 1));
    binary(ctx.heap, "floor-remainder", a, b, Num::floor_remainder)
}

fn n_truncate_quotient(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    let (a, b) = (arg(args, 0), arg(args, 1));
    binary(ctx.heap, "truncate-quotient", a, b, Num::quotient)
}

fn n_truncate_remainder(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    let (a, b) = (arg(args, 0), arg(args, 1));
    binary(ctx.heap, "truncate-remainder", a, b, Num::remainder)
}

// ---------------------------------------------------------------- one-operand operations

fn n_abs(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    unary(ctx.heap, "abs", arg(args, 0), Num::abs)
}

fn n_numerator(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    unary(ctx.heap, "numerator", arg(args, 0), Num::numerator)
}

fn n_denominator(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    unary(ctx.heap, "denominator", arg(args, 0), Num::denominator)
}

fn n_floor(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    unary(ctx.heap, "floor", arg(args, 0), Num::floor)
}

fn n_ceiling(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    unary(ctx.heap, "ceiling", arg(args, 0), Num::ceiling)
}

fn n_truncate(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    unary(ctx.heap, "truncate", arg(args, 0), Num::truncate)
}

fn n_round(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    unary(ctx.heap, "round", arg(args, 0), Num::round)
}

fn n_rationalize(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    let (x, tolerance) = (arg(args, 0), arg(args, 1));
    binary(ctx.heap, "rationalize", x, tolerance, Num::rationalize)
}

fn n_square(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    unary(ctx.heap, "square", arg(args, 0), |x| Ok(x.square()))
}

/// Also the body of `inexact->exact`, so a failure there names `exact`.
fn n_exact(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    unary(ctx.heap, "exact", arg(args, 0), Num::exact)
}

/// Also the body of `exact->inexact`.
fn n_inexact(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    unary(ctx.heap, "inexact", arg(args, 0), |x| Ok(x.inexact()))
}

// ---------------------------------------------------------------- string conversion

/// The optional radix argument of the string conversions. Anything outside R7RS's four
/// radixes is refused here: `Num::parse` answers "not a number" for them, which would turn
/// a bad radix into a quiet `#f`.
fn radix(args: &[Value], op: &'static str) -> Result<u32, VmError> {
    let Some(&v) = args.get(1) else {
        return Ok(10);
    };
    v.as_fixnum()
        .and_then(|r| u32::try_from(r).ok())
        .filter(|r| matches!(r, 2 | 8 | 10 | 16))
        .ok_or_else(|| num_err(op, NumError::BadRadix))
}

fn n_number_to_string(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    const OP: &str = "number->string";
    let n = operand(ctx.heap, OP, arg(args, 0))?;
    let radix = radix(args, OP)?;
    let text = n.to_string_radix(radix).map_err(|e| num_err(OP, e))?;
    Ok(ctx.heap.string(text))
}

fn n_string_to_number(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    const OP: &str = "string->number";
    let s = arg(args, 0);
    let Some(text) = ctx.heap.get::<Str>(s) else {
        return Err(wrong_type(ctx.heap, OP, "a string", s));
    };
    let parsed = Num::parse(&text.chars, radix(args, OP)?);
    Ok(match parsed {
        Some(n) => ctx.heap.number(n),
        None => Value::FALSE,
    })
}

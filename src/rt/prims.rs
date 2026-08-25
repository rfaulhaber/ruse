//! The native-function table: the `PRIMCALL` bridge, and the M3 primitive set.
//!
//! Decision G's three stdlib layers meet here. A primitive is a plain Rust `fn` in a
//! per-VM table; `PRIMCALL C` indexes the table directly, and every entry is *also*
//! installed as a global bound to a [`NativeProc`](crate::value::object::NativeProc)
//! value, so primitives are first-class: `((if #f + *) 3 4)` works because `+` and `*`
//! are ordinary values that `CALL` knows how to invoke framelessly.
//!
//! Natives run entirely between safepoints — nothing a native does can trigger a
//! collection, so plain `Value` locals inside one are safe without pinning. (The pin
//! stack exists for the day that changes.)
//!
//! The table's index order is not frozen: nothing serializes bytecode yet, so `PRIMCALL`
//! operands never outlive the process that compiled them. Freezing becomes necessary the
//! day compiled prototypes are written to disk — noted in the M3 open questions.

use std::collections::HashMap;
use std::io::Write;

use crate::gc::Heap;
use crate::rt;
use crate::rt::write::Style;
use crate::value::Value;
use crate::value::layout::HeapTag;
use crate::vm::error::{VmError, VmErrorKind};
use crate::vm::globals::Globals;

/// What a native runs against: the heap, the VM's output sink, and the global table.
pub struct NativeCtx<'a> {
    pub heap: &'a mut Heap,
    pub out: &'a mut dyn Write,
    pub globals: &'a mut Globals,
}

/// A native primitive: a plain function pointer, so the table stays `Copy`-cheap and a
/// future JIT can call entries directly.
pub type NativeFn = fn(&mut NativeCtx<'_>, &[Value]) -> Result<Value, VmError>;

/// One table entry.
pub struct PrimDef {
    pub name: String,
    pub min_args: usize,
    /// `None` = variadic.
    pub max_args: Option<usize>,
    pub func: NativeFn,
}

impl PrimDef {
    /// Human text for an arity: "2", "at least 1", "1 to 2".
    pub fn expected_text(min: usize, max: Option<usize>) -> String {
        match max {
            Some(max) if max == min => min.to_string(),
            Some(max) => format!("{min} to {max}"),
            None => format!("at least {min}"),
        }
    }
}

/// The per-VM native-function table.
#[derive(Default)]
pub struct PrimTable {
    defs: Vec<PrimDef>,
    by_name: HashMap<String, u32>,
}

impl PrimTable {
    /// Append a definition; its index is stable for the life of the VM.
    pub fn register(&mut self, def: PrimDef) -> u32 {
        let index = u32::try_from(self.defs.len()).unwrap_or(u32::MAX);
        self.by_name.insert(def.name.clone(), index);
        self.defs.push(def);
        index
    }

    /// The entry at `index`.
    pub fn get(&self, index: u32) -> Option<&PrimDef> {
        self.defs.get(index as usize)
    }

    /// The entry named `name`, with its index — the compiler's `PRIMCALL` lookup.
    pub fn lookup(&self, name: &str) -> Option<(u32, &PrimDef)> {
        let index = *self.by_name.get(name)?;
        Some((index, self.defs.get(index as usize)?))
    }

    /// How many entries exist.
    pub fn len(&self) -> usize {
        self.defs.len()
    }

    /// Whether the table is empty.
    pub fn is_empty(&self) -> bool {
        self.defs.is_empty()
    }
}

/// Install the M3 primitive set: register each native and bind it as a pristine global.
pub fn install(prims: &mut PrimTable, heap: &mut Heap, globals: &mut Globals) {
    let defs: &[(&str, usize, Option<usize>, NativeFn)] = &[
        ("+", 0, None, n_add),
        ("-", 1, None, n_sub),
        ("*", 0, None, n_mul),
        ("=", 2, None, n_num_eq),
        ("<", 2, None, n_lt),
        ("<=", 2, None, n_le),
        (">", 2, None, n_gt),
        (">=", 2, None, n_ge),
        ("cons", 2, Some(2), n_cons),
        ("car", 1, Some(1), n_car),
        ("cdr", 1, Some(1), n_cdr),
        ("set-car!", 2, Some(2), n_set_car),
        ("set-cdr!", 2, Some(2), n_set_cdr),
        ("list", 0, None, n_list),
        ("eq?", 2, Some(2), n_eq),
        ("eqv?", 2, Some(2), n_eqv),
        ("equal?", 2, Some(2), n_equal),
        ("not", 1, Some(1), n_not),
        ("null?", 1, Some(1), n_null_p),
        ("pair?", 1, Some(1), n_pair_p),
        ("boolean?", 1, Some(1), n_boolean_p),
        ("symbol?", 1, Some(1), n_symbol_p),
        ("string?", 1, Some(1), n_string_p),
        ("char?", 1, Some(1), n_char_p),
        ("number?", 1, Some(1), n_number_p),
        ("integer?", 1, Some(1), n_integer_p),
        ("procedure?", 1, Some(1), n_procedure_p),
        ("vector?", 1, Some(1), n_vector_p),
        ("zero?", 1, Some(1), n_zero_p),
        ("vector-ref", 2, Some(2), n_vector_ref),
        ("vector-set!", 3, Some(3), n_vector_set),
        ("make-vector", 1, Some(2), n_make_vector),
        ("display", 1, Some(1), n_display),
        ("write", 1, Some(1), n_write),
        ("newline", 0, Some(0), n_newline),
    ];
    for &(name, min, max, func) in defs {
        let index = prims.register(PrimDef {
            name: name.to_string(),
            min_args: min,
            max_args: max,
            func,
        });
        let proc = heap.native_proc(name, index);
        let sym = heap.symbol(name);
        globals.define_builtin(sym, proc);
    }
}

// ---------------------------------------------------------------- helpers

/// Argument access that cannot panic: arity was validated before dispatch, so a miss is
/// unreachable, and `undefined` errors loudly downstream if it ever is not.
fn arg(args: &[Value], i: usize) -> Value {
    args.get(i).copied().unwrap_or(Value::UNDEFINED)
}

fn chain(
    ctx: &NativeCtx<'_>,
    args: &[Value],
    pred: fn(&Heap, Value, Value) -> Result<bool, VmError>,
) -> Result<Value, VmError> {
    for pair in args.windows(2) {
        if !pred(ctx.heap, arg(pair, 0), arg(pair, 1))? {
            return Ok(Value::FALSE);
        }
    }
    Ok(Value::TRUE)
}

fn emit(ctx: &mut NativeCtx<'_>, text: &str) -> Result<Value, VmError> {
    match ctx.out.write_all(text.as_bytes()) {
        Ok(()) => Ok(Value::UNSPECIFIED),
        Err(e) => VmErrorKind::Io {
            message: e.to_string(),
        }
        .err(),
    }
}

// ---------------------------------------------------------------- arithmetic

/// Folds from the first argument rather than a synthesized identity: seeding with exact
/// 0 would turn `(+ -0.0)` into `0.0` (IEEE: `0 + -0.0` is `+0.0`), disagreeing with the
/// inlined `ADD` path on the same operands. The explicit type check keeps `(+ "a")` an
/// error even though no addition runs.
fn n_add(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    let Some((&first, rest)) = args.split_first() else {
        return Ok(ctx.heap.integer(0));
    };
    if !rt::arith::is_number(ctx.heap, first) {
        return Err(rt::wrong_type(ctx.heap, "+", "a number", first));
    }
    let mut acc = first;
    for &x in rest {
        acc = rt::arith::add(ctx.heap, acc, x)?;
    }
    Ok(acc)
}

fn n_sub(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    if args.len() == 1 {
        return rt::arith::neg(ctx.heap, arg(args, 0));
    }
    let mut acc = arg(args, 0);
    for &x in &args[1..] {
        acc = rt::arith::sub(ctx.heap, acc, x)?;
    }
    Ok(acc)
}

/// Folds from the first argument, like `n_add` and for the same reason.
fn n_mul(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    let Some((&first, rest)) = args.split_first() else {
        return Ok(ctx.heap.integer(1));
    };
    if !rt::arith::is_number(ctx.heap, first) {
        return Err(rt::wrong_type(ctx.heap, "*", "a number", first));
    }
    let mut acc = first;
    for &x in rest {
        acc = rt::arith::mul(ctx.heap, acc, x)?;
    }
    Ok(acc)
}

fn n_num_eq(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    chain(ctx, args, rt::arith::num_eq)
}

fn n_lt(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    chain(ctx, args, rt::arith::num_lt)
}

fn n_le(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    chain(ctx, args, rt::arith::num_le)
}

fn n_gt(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    chain(ctx, args, |heap, a, b| rt::arith::num_lt(heap, b, a))
}

fn n_ge(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    chain(ctx, args, |heap, a, b| rt::arith::num_le(heap, b, a))
}

fn n_zero_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    let zero = ctx.heap.integer(0);
    Ok(Value::boolean(rt::arith::num_eq(
        ctx.heap,
        arg(args, 0),
        zero,
    )?))
}

// ---------------------------------------------------------------- pairs and lists

fn n_cons(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(ctx.heap.cons(arg(args, 0), arg(args, 1)))
}

fn n_car(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    rt::pairs::car(ctx.heap, arg(args, 0))
}

fn n_cdr(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    rt::pairs::cdr(ctx.heap, arg(args, 0))
}

fn n_set_car(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    rt::pairs::set_car(ctx.heap, arg(args, 0), arg(args, 1))?;
    Ok(Value::UNSPECIFIED)
}

fn n_set_cdr(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    rt::pairs::set_cdr(ctx.heap, arg(args, 0), arg(args, 1))?;
    Ok(Value::UNSPECIFIED)
}

fn n_list(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(rt::pairs::list(ctx.heap, args))
}

// ---------------------------------------------------------------- equivalence

fn n_eq(_ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(Value::boolean(arg(args, 0) == arg(args, 1)))
}

fn n_eqv(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(Value::boolean(rt::equal::eqv(
        ctx.heap,
        arg(args, 0),
        arg(args, 1),
    )))
}

fn n_equal(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(Value::boolean(rt::equal::equal(
        ctx.heap,
        arg(args, 0),
        arg(args, 1),
    )))
}

// ---------------------------------------------------------------- predicates

fn n_not(_ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(Value::boolean(!arg(args, 0).truthy()))
}

fn n_null_p(_ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(Value::boolean(arg(args, 0).is_null()))
}

fn tag_is(ctx: &NativeCtx<'_>, v: Value, tag: HeapTag) -> Value {
    Value::boolean(ctx.heap.tag_of(v) == Some(tag))
}

fn n_pair_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(tag_is(ctx, arg(args, 0), HeapTag::Pair))
}

fn n_boolean_p(_ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(Value::boolean(arg(args, 0).is_boolean()))
}

fn n_symbol_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(tag_is(ctx, arg(args, 0), HeapTag::Symbol))
}

fn n_string_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(tag_is(ctx, arg(args, 0), HeapTag::Str))
}

fn n_char_p(_ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(Value::boolean(arg(args, 0).is_char()))
}

fn n_number_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(Value::boolean(rt::arith::is_number(ctx.heap, arg(args, 0))))
}

fn n_integer_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    let v = arg(args, 0);
    let is_int = v.is_fixnum()
        || ctx.heap.tag_of(v) == Some(HeapTag::Bignum)
        // R7RS §6.2.4: (integer? 3.0) is #t — inexact integers are integers.
        || v.as_flonum().is_some_and(|x| x.is_finite() && x.fract() == 0.0);
    Ok(Value::boolean(is_int))
}

fn n_procedure_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    let tag = ctx.heap.tag_of(arg(args, 0));
    Ok(Value::boolean(matches!(
        tag,
        Some(HeapTag::Closure | HeapTag::NativeProc)
    )))
}

fn n_vector_p(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    Ok(tag_is(ctx, arg(args, 0), HeapTag::Vector))
}

// ---------------------------------------------------------------- vectors

fn n_vector_ref(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    rt::vectors::vec_ref(ctx.heap, arg(args, 0), arg(args, 1))
}

fn n_vector_set(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    rt::vectors::vec_set(ctx.heap, arg(args, 0), arg(args, 1), arg(args, 2))?;
    Ok(Value::UNSPECIFIED)
}

fn n_make_vector(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    // R7RS leaves the fill of a one-argument make-vector unspecified; ours says so.
    let fill = if args.len() == 2 {
        arg(args, 1)
    } else {
        Value::UNSPECIFIED
    };
    rt::vectors::make_vector(ctx.heap, arg(args, 0), fill)
}

// ---------------------------------------------------------------- output

fn n_display(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    let text = rt::write::value_to_string(ctx.heap, arg(args, 0), Style::Display);
    emit(ctx, &text)
}

fn n_write(ctx: &mut NativeCtx<'_>, args: &[Value]) -> Result<Value, VmError> {
    let text = rt::write::value_to_string(ctx.heap, arg(args, 0), Style::Write);
    emit(ctx, &text)
}

fn n_newline(ctx: &mut NativeCtx<'_>, _args: &[Value]) -> Result<Value, VmError> {
    emit(ctx, "\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fix(n: i64) -> Value {
        Value::fixnum(n).unwrap()
    }

    fn boot() -> (PrimTable, Heap, Globals) {
        let mut prims = PrimTable::default();
        let mut heap = Heap::new();
        let mut globals = Globals::default();
        install(&mut prims, &mut heap, &mut globals);
        (prims, heap, globals)
    }

    fn run(
        prims: &PrimTable,
        heap: &mut Heap,
        globals: &mut Globals,
        name: &str,
        args: &[Value],
    ) -> Result<Value, VmError> {
        let mut sink = Vec::new();
        let (_, def) = prims.lookup(name).expect("prim registered");
        let mut ctx = NativeCtx {
            heap,
            out: &mut sink,
            globals,
        };
        (def.func)(&mut ctx, args)
    }

    #[test]
    fn every_prim_is_a_pristine_global_procedure() {
        let (prims, mut heap, globals) = boot();
        assert!(prims.len() >= 30);
        for name in ["+", "car", "display", "equal?"] {
            let sym = heap.symbol(name);
            assert!(
                globals.is_pristine_builtin(sym),
                "{name} must boot pristine"
            );
            let v = globals.lookup_value(sym).expect("bound");
            assert_eq!(heap.tag_of(v), Some(HeapTag::NativeProc));
        }
    }

    #[test]
    fn variadic_arithmetic_folds() {
        let (prims, mut heap, mut globals) = boot();
        let mut go = |name: &str, args: &[Value]| {
            run(&prims, &mut heap, &mut globals, name, args).expect(name)
        };
        assert_eq!(go("+", &[]), fix(0));
        assert_eq!(go("*", &[]), fix(1));
        assert_eq!(go("+", &[fix(1), fix(2), fix(3)]), fix(6));
        assert_eq!(go("-", &[fix(10), fix(1), fix(2)]), fix(7));
        assert_eq!(go("-", &[fix(5)]), fix(-5));
        assert_eq!(go("*", &[fix(2), fix(3), fix(4)]), fix(24));
        assert_eq!(go("<", &[fix(1), fix(2), fix(3)]), Value::TRUE);
        assert_eq!(go("<", &[fix(1), fix(3), fix(2)]), Value::FALSE);
        assert_eq!(go(">=", &[fix(3), fix(3), fix(2)]), Value::TRUE);
        assert_eq!(go("=", &[fix(3), fix(3), fix(3)]), Value::TRUE);
        assert_eq!(go("zero?", &[fix(0)]), Value::TRUE);
    }

    #[test]
    fn type_errors_carry_the_scheme_name() {
        let (prims, mut heap, mut globals) = boot();
        let s = heap.string("nope");
        let err = run(&prims, &mut heap, &mut globals, "+", &[s]).unwrap_err();
        assert_eq!(
            err.kind,
            VmErrorKind::WrongType {
                op: "+",
                expected: "a number",
                got: "string"
            }
        );
    }

    #[test]
    fn predicates_answer_over_the_whole_value_space() {
        let (prims, mut heap, mut globals) = boot();
        let s = heap.string("s");
        let sym = heap.symbol("s");
        let pair = heap.cons(Value::NIL, Value::NIL);
        let plus = globals.lookup_value(heap.symbol("+")).expect("+ bound");
        let mut go =
            |name: &str, v: Value| run(&prims, &mut heap, &mut globals, name, &[v]).expect(name);
        assert_eq!(go("null?", Value::NIL), Value::TRUE);
        assert_eq!(go("null?", pair), Value::FALSE);
        assert_eq!(go("pair?", pair), Value::TRUE);
        assert_eq!(go("pair?", Value::NIL), Value::FALSE);
        assert_eq!(go("string?", s), Value::TRUE);
        assert_eq!(go("symbol?", sym), Value::TRUE);
        assert_eq!(go("symbol?", s), Value::FALSE);
        assert_eq!(go("boolean?", Value::FALSE), Value::TRUE);
        assert_eq!(go("char?", Value::char('c')), Value::TRUE);
        assert_eq!(go("number?", Value::flonum(1.5)), Value::TRUE);
        assert_eq!(go("integer?", fix(3)), Value::TRUE);
        assert_eq!(go("integer?", Value::flonum(3.0)), Value::TRUE);
        assert_eq!(go("integer?", Value::flonum(3.5)), Value::FALSE);
        assert_eq!(go("procedure?", plus), Value::TRUE);
        assert_eq!(go("procedure?", s), Value::FALSE);
        assert_eq!(go("not", Value::FALSE), Value::TRUE);
        assert_eq!(go("not", Value::NIL), Value::FALSE);
    }

    #[test]
    fn display_and_write_reach_the_sink() {
        let (prims, mut heap, mut globals) = boot();
        let mut sink = Vec::new();
        let s = heap.string("hi\n");
        {
            let (_, def) = prims.lookup("write").expect("write");
            let mut ctx = NativeCtx {
                heap: &mut heap,
                out: &mut sink,
                globals: &mut globals,
            };
            assert_eq!(
                (def.func)(&mut ctx, &[s]).expect("write"),
                Value::UNSPECIFIED
            );
        }
        assert_eq!(String::from_utf8(sink).expect("utf8"), "\"hi\\n\"");
    }

    #[test]
    fn arity_text_reads_like_english() {
        assert_eq!(PrimDef::expected_text(2, Some(2)), "2");
        assert_eq!(PrimDef::expected_text(1, Some(2)), "1 to 2");
        assert_eq!(PrimDef::expected_text(1, None), "at least 1");
    }
}

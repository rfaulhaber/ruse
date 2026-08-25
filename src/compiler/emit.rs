//! The emitter: destination-register + tail-flag descent over the Core IR, with a
//! Lua-5-style `Func` register allocator (decision C).
//!
//! Every expression is compiled *into* a destination register its context chose;
//! temporaries live above the active locals and are released by resetting `freereg` to a
//! saved mark. Three-address instructions read their operands before writing A, so an
//! operation's destination may alias an operand register — which is why the fold for
//! `(+ a b c)` keeps intermediates in a temporary accumulator and only its final
//! instruction writes the destination.
//!
//! Tail position is threaded as a flag: a call in tail position becomes `TAILCALL`, and
//! [`Compiler::tail_value`] closes every other tail expression with `RETURN1`, so the
//! verifier's you-may-not-fall-off-the-end rule holds by construction.
//!
//! Primitive calls compile three ways, best first, all gated on the name being neither
//! lexically bound nor redefined ([`Globals::is_pristine_builtin`]): a dedicated opcode
//! (`ADD`, `CAR`, …), a `PRIMCALL` into the native table, or — for anything shadowed,
//! redefined, or arity-mismatched — an honest `GETGLOBAL` + `CALL`.

use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::Expr;
use crate::bytecode::{Insn, Op, Proto};
use crate::gc::Heap;
use crate::rt::prims::PrimTable;
use crate::span::Span;
use crate::value::Value;
use crate::value::layout::HeapTag;
use crate::vm::globals::Globals;

use super::ir::{Ir, IrLambda, lower_toplevel};
use super::{CompileError, ss};

/// Compile one top-level form into a verified-shape chunk prototype.
pub(crate) fn compile_expr(
    heap: &mut Heap,
    globals: &Globals,
    prims: &PrimTable,
    expr: &Expr,
) -> Result<Rc<Proto>, CompileError> {
    let ir = lower_toplevel(expr)?;
    let mut c = Compiler {
        heap,
        globals,
        prims,
        funcs: vec![Func::default()],
    };
    let dst = c.alloc(expr.span())?;
    let terminated = c.tail_value(&ir, dst, true)?;
    if !terminated {
        // tail_value always terminates in tail mode; belt and braces for the verifier.
        c.emit(Insn::iabc(Op::Return1, dst, 0, 0), expr.span());
    }
    let Some(f) = c.funcs.pop() else {
        return Err(CompileError::Internal {
            detail: "the chunk function vanished during compilation".to_string(),
        });
    };
    Ok(Rc::new(f.into_proto()))
}

/// Per-function compilation state: Lua's `FuncState`.
#[derive(Default)]
struct Func {
    name: Option<String>,
    nparams: u8,
    code: Vec<Insn>,
    spans: Vec<Span>,
    consts: Vec<Value>,
    /// Dedup for constants compared by bits: immediates and interned symbols.
    const_map: HashMap<u64, u32>,
    protos: Vec<Rc<Proto>>,
    /// Lexical scopes: name → register, innermost scope last.
    scopes: Vec<Vec<(String, u8)>>,
    /// First free register; temporaries are allocated here and released by mark.
    freereg: u8,
    max_window: u8,
}

impl Func {
    fn into_proto(self) -> Proto {
        Proto {
            name: self.name,
            code: self.code,
            consts: self.consts,
            upvals: Vec::new(),
            protos: self.protos,
            nparams: self.nparams,
            has_rest: false,
            // A function that allocated nothing still owns r0 for its result.
            max_window: self.max_window.max(1),
            spans: self.spans,
        }
    }
}

enum Resolved {
    Local(u8),
    Global,
}

struct Compiler<'a> {
    heap: &'a mut Heap,
    globals: &'a Globals,
    prims: &'a PrimTable,
    /// The chain of functions being compiled, innermost last.
    funcs: Vec<Func>,
}

impl Compiler<'_> {
    /// The innermost function. The vector is never empty while compiling; the push keeps
    /// this total without an unwrap if that invariant ever breaks.
    fn func(&mut self) -> &mut Func {
        if self.funcs.is_empty() {
            self.funcs.push(Func::default());
        }
        let last = self.funcs.len() - 1;
        &mut self.funcs[last]
    }

    fn emit(&mut self, insn: Insn, span: Span) {
        let f = self.func();
        f.code.push(insn);
        f.spans.push(span);
    }

    // ------------------------------------------------------------ registers

    fn alloc(&mut self, span: Span) -> Result<u8, CompileError> {
        let f = self.func();
        if f.freereg >= Proto::MAX_WINDOW {
            return Err(CompileError::WindowOverflow {
                needed: usize::from(f.freereg) + 1,
                max: usize::from(Proto::MAX_WINDOW),
                span: ss(span),
            });
        }
        let r = f.freereg;
        f.freereg += 1;
        f.max_window = f.max_window.max(f.freereg);
        Ok(r)
    }

    fn mark(&mut self) -> u8 {
        self.func().freereg
    }

    fn free_to(&mut self, mark: u8) {
        self.func().freereg = mark;
    }

    // ------------------------------------------------------------ names

    fn resolve(&mut self, name: &str, span: Span) -> Result<Resolved, CompileError> {
        if let Some(r) = self.peek_local(name) {
            return Ok(Resolved::Local(r));
        }
        if self.is_enclosing_local(name) {
            return Err(CompileError::CaptureUnsupported {
                name: name.to_string(),
                span: ss(span),
            });
        }
        Ok(Resolved::Global)
    }

    /// A binding in the *current* function, if any.
    fn peek_local(&mut self, name: &str) -> Option<u8> {
        let f = self.func();
        for scope in f.scopes.iter().rev() {
            for (n, r) in scope.iter().rev() {
                if n == name {
                    return Some(*r);
                }
            }
        }
        None
    }

    /// A binding in an *enclosing* function — capturable only from M4 on.
    fn is_enclosing_local(&mut self, name: &str) -> bool {
        let outer = self.funcs.len().saturating_sub(1);
        self.funcs[..outer].iter().any(|f| {
            f.scopes
                .iter()
                .any(|scope| scope.iter().any(|(n, _)| n == name))
        })
    }

    /// Whether `name` may be compiled to its primitive: not a local anywhere, and still
    /// the untouched boot-time global.
    fn primitive_licensed(&mut self, name: &str) -> bool {
        if self.peek_local(name).is_some() || self.is_enclosing_local(name) {
            return false;
        }
        let sym = self.heap.symbol(name);
        self.globals.is_pristine_builtin(sym)
    }

    // ------------------------------------------------------------ constants

    fn konst(&mut self, v: Value) -> Result<u32, CompileError> {
        let dedup = !v.is_heap() || self.heap.tag_of(v) == Some(HeapTag::Symbol);
        let f = self.func();
        if dedup && let Some(&i) = f.const_map.get(&v.to_bits()) {
            return Ok(i);
        }
        let i = u32::try_from(f.consts.len()).unwrap_or(u32::MAX);
        if i >= 1 << 24 {
            return Err(CompileError::TooMany {
                what: "constants",
                count: f.consts.len(),
                span: ss(Span::new(0, 0)),
            });
        }
        f.consts.push(v);
        if dedup {
            f.const_map.insert(v.to_bits(), i);
        }
        Ok(i)
    }

    fn emit_load_const(&mut self, v: Value, dst: u8, span: Span) -> Result<(), CompileError> {
        let k = self.konst(v)?;
        match u16::try_from(k) {
            Ok(bx) => self.emit(Insn::iabx(Op::LoadK, dst, bx), span),
            Err(_) => {
                self.emit(Insn::iabx(Op::LoadKx, dst, 0), span);
                self.emit(Insn::iax(Op::ExtraArg, k), span);
            }
        }
        Ok(())
    }

    /// Load any value by its cheapest encoding: singleton → `LOADIMM`, small fixnum →
    /// `LOADI`, everything else → the pool.
    fn emit_load_value(&mut self, v: Value, dst: u8, span: Span) -> Result<(), CompileError> {
        if let Some(ordinal) = singleton_ordinal(v) {
            self.emit(Insn::iabx(Op::LoadImm, dst, ordinal), span);
            return Ok(());
        }
        if let Some(n) = v.as_fixnum()
            && let Ok(sbx) = i16::try_from(n)
        {
            self.emit(Insn::iasbx(Op::LoadI, dst, sbx), span);
            return Ok(());
        }
        self.emit_load_const(v, dst, span)
    }

    /// Build the runtime value of a quoted datum. Recursion depth here matches the
    /// reader's own nesting depth, which the parser already recursed to build.
    fn datum_value(&mut self, expr: &Expr) -> Result<Value, CompileError> {
        Ok(match expr {
            Expr::Integer(n, _) => self.heap.integer(*n),
            Expr::Number(x, _) => Value::flonum(*x),
            Expr::String(s, _) => self.heap.string(s.clone()),
            Expr::Character(c, _) => Value::char(*c),
            Expr::Boolean(b, _) => Value::boolean(*b),
            Expr::Symbol(s, _) => self.heap.symbol(s),
            Expr::List(elems, _) => {
                let mut acc = Value::NIL;
                for e in elems.iter().rev() {
                    let v = self.datum_value(e)?;
                    acc = self.heap.cons(v, acc);
                }
                acc
            }
            Expr::DottedList(elems, tail, _) => {
                let mut acc = self.datum_value(tail)?;
                for e in elems.iter().rev() {
                    let v = self.datum_value(e)?;
                    acc = self.heap.cons(v, acc);
                }
                acc
            }
            // Inside a datum the quote family is ordinary list structure.
            Expr::Quote(inner, _) => self.sugared_datum("quote", inner)?,
            Expr::Quasiquote(inner, _) => self.sugared_datum("quasiquote", inner)?,
            Expr::Unquote(inner, _) => self.sugared_datum("unquote", inner)?,
            Expr::UnquoteSplicing(inner, _) => self.sugared_datum("unquote-splicing", inner)?,
        })
    }

    fn sugared_datum(&mut self, sym: &str, inner: &Expr) -> Result<Value, CompileError> {
        let head = self.heap.symbol(sym);
        let inner = self.datum_value(inner)?;
        let tail = self.heap.cons(inner, Value::NIL);
        Ok(self.heap.cons(head, tail))
    }

    // ------------------------------------------------------------ globals

    fn global_key(&mut self, name: &str, span: Span) -> Result<u16, CompileError> {
        let sym = self.heap.symbol(name);
        let k = self.konst(sym)?;
        u16::try_from(k).map_err(|_| CompileError::TooMany {
            what: "global-name constants",
            count: k as usize,
            span: ss(span),
        })
    }

    fn emit_get_global(&mut self, name: &str, dst: u8, span: Span) -> Result<(), CompileError> {
        let k = self.global_key(name, span)?;
        self.emit(Insn::iabx(Op::GetGlobal, dst, k), span);
        Ok(())
    }

    fn emit_set_global(&mut self, name: &str, src: u8, span: Span) -> Result<(), CompileError> {
        let k = self.global_key(name, span)?;
        self.emit(Insn::iabx(Op::SetGlobal, src, k), span);
        Ok(())
    }

    // ------------------------------------------------------------ expressions

    /// Compile `ir`, leaving its value in `dst`. Returns whether the emitted code
    /// terminated the function (only possible when `tail` is true).
    fn expr(&mut self, ir: &Ir, dst: u8, tail: bool) -> Result<bool, CompileError> {
        match ir {
            Ir::Datum(e) => {
                let v = self.datum_value(e)?;
                self.emit_load_value(v, dst, e.span())?;
                Ok(false)
            }
            Ir::Var(name, span) => {
                match self.resolve(name, *span)? {
                    Resolved::Local(r) => {
                        if r != dst {
                            self.emit(Insn::iabc(Op::Move, dst, r, 0), *span);
                        }
                    }
                    Resolved::Global => self.emit_get_global(name, dst, *span)?,
                }
                Ok(false)
            }
            Ir::Set { name, value, span } => {
                match self.resolve(name, *span)? {
                    Resolved::Local(r) => {
                        self.expr(value, r, false)?;
                        // Destinations are allocated below locals, so dst == r cannot
                        // happen; the guard keeps a future refactor from clobbering the
                        // freshly set variable with the unspecified result.
                        if dst != r {
                            self.emit_load_value(Value::UNSPECIFIED, dst, *span)?;
                        }
                    }
                    Resolved::Global => {
                        let m = self.mark();
                        let tv = self.alloc(*span)?;
                        self.expr(value, tv, false)?;
                        // R7RS set! on an unbound variable is an error, and SETGLOBAL
                        // define-creates its slot — so probe with GETGLOBAL first, whose
                        // unbound check is exactly the one we need.
                        let probe = self.alloc(*span)?;
                        self.emit_get_global(name, probe, *span)?;
                        self.emit_set_global(name, tv, *span)?;
                        self.free_to(m);
                        self.emit_load_value(Value::UNSPECIFIED, dst, *span)?;
                    }
                }
                Ok(false)
            }
            Ir::Define { name, value, span } => {
                let m = self.mark();
                let tv = self.alloc(*span)?;
                self.expr(value, tv, false)?;
                self.emit_set_global(name, tv, *span)?;
                self.free_to(m);
                self.emit_load_value(Value::UNSPECIFIED, dst, *span)?;
                Ok(false)
            }
            Ir::Begin { body, span } => self.body(body, dst, tail, *span),
            Ir::Let {
                bindings,
                body,
                span,
            } => {
                let m = self.mark();
                let mut scope = Vec::with_capacity(bindings.len());
                for (name, init) in bindings {
                    let r = self.alloc(*span)?;
                    self.expr(init, r, false)?;
                    scope.push((name.clone(), r));
                }
                self.func().scopes.push(scope);
                let terminated = self.body(body, dst, tail, *span);
                self.func().scopes.pop();
                self.free_to(m);
                terminated
            }
            Ir::Lambda(l) => {
                let index = self.lambda(l)?;
                self.emit(Insn::iabx(Op::Closure, dst, index), l.span);
                Ok(false)
            }
            Ir::If {
                cond,
                then,
                els,
                span,
            } => self.if_form(cond, then, els.as_deref(), dst, tail, *span),
            Ir::Call { head, args, span } => self.call(head, args, dst, tail, *span),
        }
    }

    /// A body: every form but the last for effect, the last into `dst`/`tail`.
    fn body(&mut self, body: &[Ir], dst: u8, tail: bool, span: Span) -> Result<bool, CompileError> {
        let Some((last, init)) = body.split_last() else {
            self.emit_load_value(Value::UNSPECIFIED, dst, span)?;
            return Ok(false);
        };
        for stmt in init {
            let m = self.mark();
            let scratch = self.alloc(stmt.span())?;
            self.expr(stmt, scratch, false)?;
            self.free_to(m);
        }
        self.tail_value(last, dst, tail)
    }

    /// Compile a value position that, in tail mode, must terminate the function.
    fn tail_value(&mut self, ir: &Ir, dst: u8, tail: bool) -> Result<bool, CompileError> {
        if tail {
            // A local in tail position returns directly — no MOVE into dst.
            if let Ir::Var(name, span) = ir
                && let Some(r) = self.peek_local(name)
            {
                self.emit(Insn::iabc(Op::Return1, r, 0, 0), *span);
                return Ok(true);
            }
            let terminated = self.expr(ir, dst, true)?;
            if !terminated {
                self.emit(Insn::iabc(Op::Return1, dst, 0, 0), ir.span());
            }
            return Ok(true);
        }
        self.expr(ir, dst, false)
    }

    // ------------------------------------------------------------ if

    fn if_form(
        &mut self,
        cond: &Ir,
        then: &Ir,
        els: Option<&Ir>,
        dst: u8,
        tail: bool,
        span: Span,
    ) -> Result<bool, CompileError> {
        // Fall through into the then-branch when the condition holds; the JMP to the
        // else-branch runs only when it does not (skip-family, k=0: "skip if cond").
        self.branch_when_false(cond)?;
        let jmp_else = self.emit_jump_placeholder(cond.span());

        self.tail_value(then, dst, tail)?;
        let jmp_end = if tail {
            None
        } else {
            Some(self.emit_jump_placeholder(span))
        };

        self.patch_jump(jmp_else, span)?;
        match els {
            Some(els) => {
                self.tail_value(els, dst, tail)?;
            }
            None => {
                self.emit_load_value(Value::UNSPECIFIED, dst, span)?;
                if tail {
                    self.emit(Insn::iabc(Op::Return1, dst, 0, 0), span);
                }
            }
        }
        if let Some(jmp) = jmp_end {
            self.patch_jump(jmp, span)?;
        }
        Ok(tail)
    }

    /// Emit a test for `cond` such that the *next* instruction executes exactly when the
    /// condition is false — the caller puts its else-JMP there. Fuses a licensed two-arg
    /// comparison into one skip instruction; anything else goes through `TEST`.
    fn branch_when_false(&mut self, cond: &Ir) -> Result<(), CompileError> {
        let m = self.mark();
        if let Some((op, x, y)) = self.fused_comparison(cond) {
            let rx = self.operand(x)?;
            let ry = self.operand(y)?;
            self.emit(Insn::iabc(op, rx, ry, 0), cond.span());
        } else {
            let t = self.alloc(cond.span())?;
            self.expr(cond, t, false)?;
            self.emit(Insn::iabc(Op::Test, t, 0, 0), cond.span());
        }
        self.free_to(m);
        Ok(())
    }

    /// A condition of the shape `(cmp x y)` where `cmp` is a licensed comparison
    /// primitive. `>`/`>=` compile by swapping operands into `NUMLT`/`NUMLE`.
    fn fused_comparison<'i>(&mut self, cond: &'i Ir) -> Option<(Op, &'i Ir, &'i Ir)> {
        let Ir::Call { head, args, .. } = cond else {
            return None;
        };
        let Ir::Var(name, _) = head.as_ref() else {
            return None;
        };
        let [x, y] = args.as_slice() else {
            return None;
        };
        if !self.primitive_licensed(name) {
            return None;
        }
        Some(match name.as_str() {
            "=" => (Op::NumEq, x, y),
            "<" => (Op::NumLt, x, y),
            "<=" => (Op::NumLe, x, y),
            ">" => (Op::NumLt, y, x),
            ">=" => (Op::NumLe, y, x),
            "eq?" => (Op::Eq, x, y),
            "eqv?" => (Op::Eqv, x, y),
            _ => return None,
        })
    }

    /// An operand for a three-address instruction: a local's own register when the
    /// expression is just that local, a fresh temporary otherwise.
    ///
    /// Reading a local in place means a `set!` in a *later* operand is visible to it —
    /// `(+ x (begin (set! x 99) 1))` sees 99. That is a legal R7RS outcome (§4.1.3
    /// leaves operand evaluation order unspecified, and this matches evaluating `x`
    /// last), and it is the same trade Lua's compiler makes for the same win.
    fn operand(&mut self, ir: &Ir) -> Result<u8, CompileError> {
        if let Ir::Var(name, _) = ir
            && let Some(r) = self.peek_local(name)
        {
            return Ok(r);
        }
        let r = self.alloc(ir.span())?;
        self.expr(ir, r, false)?;
        Ok(r)
    }

    // ------------------------------------------------------------ jumps

    fn emit_jump_placeholder(&mut self, span: Span) -> usize {
        self.emit(Insn::iasbx(Op::Jmp, 0, 0), span);
        self.func().code.len() - 1
    }

    fn patch_jump(&mut self, at: usize, span: Span) -> Result<(), CompileError> {
        let target = self.func().code.len();
        let distance = target as i64 - (at as i64 + 1);
        let Ok(sbx) = i16::try_from(distance) else {
            return Err(CompileError::JumpTooFar {
                distance,
                span: ss(span),
            });
        };
        if let Some(slot) = self.func().code.get_mut(at) {
            *slot = Insn::iasbx(Op::Jmp, 0, sbx);
        }
        Ok(())
    }

    // ------------------------------------------------------------ lambda

    fn lambda(&mut self, l: &IrLambda) -> Result<u16, CompileError> {
        let Ok(nparams) = u8::try_from(l.params.len()) else {
            return Err(CompileError::WindowOverflow {
                needed: l.params.len(),
                max: usize::from(Proto::MAX_WINDOW),
                span: ss(l.span),
            });
        };
        if nparams >= Proto::MAX_WINDOW {
            return Err(CompileError::WindowOverflow {
                needed: usize::from(nparams) + 1,
                max: usize::from(Proto::MAX_WINDOW),
                span: ss(l.span),
            });
        }
        let scope = l
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| (p.clone(), i as u8))
            .collect();
        self.funcs.push(Func {
            name: l.name.clone(),
            nparams,
            scopes: vec![scope],
            freereg: nparams,
            max_window: nparams,
            ..Func::default()
        });

        let dst = self.alloc(l.span)?;
        self.body(&l.body, dst, true, l.span)?;

        let Some(f) = self.funcs.pop() else {
            return Err(CompileError::Internal {
                detail: "lambda function state vanished".to_string(),
            });
        };
        let proto = Rc::new(f.into_proto());
        let parent = self.func();
        let Ok(index) = u16::try_from(parent.protos.len()) else {
            return Err(CompileError::TooMany {
                what: "child prototypes",
                count: parent.protos.len(),
                span: ss(l.span),
            });
        };
        parent.protos.push(proto);
        Ok(index)
    }

    // ------------------------------------------------------------ calls

    fn call(
        &mut self,
        head: &Ir,
        args: &[Ir],
        dst: u8,
        tail: bool,
        span: Span,
    ) -> Result<bool, CompileError> {
        if args.len() > 254 {
            return Err(CompileError::TooMany {
                what: "arguments",
                count: args.len(),
                span: ss(span),
            });
        }
        if let Ir::Var(name, _) = head {
            let name = name.clone();
            if self.primitive_licensed(&name) {
                if let Some(done) = self.inline_op(&name, args, dst, span)? {
                    return Ok(done);
                }
                if let Some(done) = self.emit_primcall(&name, args, dst, tail, span)? {
                    return Ok(done);
                }
            }
        }

        // The honest general case: callee and arguments in a contiguous block.
        let m = self.mark();
        let rf = self.alloc(span)?;
        self.expr(head, rf, false)?;
        for arg in args {
            let r = self.alloc(arg.span())?;
            self.expr(arg, r, false)?;
        }
        let b = args.len() as u8 + 1;
        if tail {
            self.emit(Insn::iabc(Op::TailCall, rf, b, 0), span);
            self.free_to(m);
            return Ok(true);
        }
        self.emit(Insn::iabc(Op::Call, rf, b, 2), span);
        if dst != rf {
            self.emit(Insn::iabc(Op::Move, dst, rf, 0), span);
        }
        self.free_to(m);
        Ok(false)
    }

    /// Opcode-backed primitives. Returns `None` when the shape doesn't fit (wrong arity
    /// for the opcode), letting the caller fall back to `PRIMCALL` or a real call.
    /// Inline results are plain values, never terminators — the tail machinery wraps
    /// them in `RETURN1` when needed — so this takes no tail flag.
    fn inline_op(
        &mut self,
        name: &str,
        args: &[Ir],
        dst: u8,
        span: Span,
    ) -> Result<Option<bool>, CompileError> {
        match (name, args) {
            ("+", []) => self.emit(Insn::iasbx(Op::LoadI, dst, 0), span),
            ("*", []) => self.emit(Insn::iasbx(Op::LoadI, dst, 1), span),
            // One-argument `+`/`*` are *not* inlined: folding an identity in would turn
            // `(+ -0.0)` into `0.0` (IEEE: adding a zero normalizes the sign), and the
            // argument still needs its number check — the native does both right.
            ("+" | "*", [_]) => return Ok(None),
            ("-", [x]) => {
                let m = self.mark();
                let r = self.operand(x)?;
                self.emit(Insn::iabc(Op::Neg, dst, r, 0), span);
                self.free_to(m);
            }
            // The ADDI peephole: adding or subtracting a byte-sized literal.
            ("+", [x, y]) if addi_immediate(y).is_some() => {
                self.emit_addi(x, addi_immediate(y), dst, span)?;
            }
            ("+", [x, y]) if addi_immediate(x).is_some() => {
                self.emit_addi(y, addi_immediate(x), dst, span)?;
            }
            ("-", [x, y]) if addi_immediate(y).and_then(|k| k.checked_neg()).is_some() => {
                self.emit_addi(
                    x,
                    addi_immediate(y).and_then(|k| k.checked_neg()),
                    dst,
                    span,
                )?;
            }
            ("+", _) => self.fold(Op::Add, args, dst, span)?,
            ("-", _) if args.len() >= 2 => self.fold(Op::Sub, args, dst, span)?,
            ("*", _) => self.fold(Op::Mul, args, dst, span)?,
            ("=" | "<" | "<=" | ">" | ">=" | "eq?" | "eqv?", [x, y]) => {
                let (op, x, y) = match name {
                    "=" => (Op::NumEq, x, y),
                    "<" => (Op::NumLt, x, y),
                    "<=" => (Op::NumLe, x, y),
                    ">" => (Op::NumLt, y, x),
                    ">=" => (Op::NumLe, y, x),
                    "eq?" => (Op::Eq, x, y),
                    _ => (Op::Eqv, x, y),
                };
                // Comparison as a value: the spec §3.2 pattern. k=1 falls through into
                // the JMP when the condition holds, reaching the #t load; the skipped
                // path loads #f and hops over it.
                let m = self.mark();
                let rx = self.operand(x)?;
                let ry = self.operand(y)?;
                self.emit(Insn::iabc(op, rx, ry, 1), span);
                self.emit(Insn::iasbx(Op::Jmp, 0, 2), span);
                self.emit_load_value(Value::FALSE, dst, span)?;
                self.emit(Insn::iasbx(Op::Jmp, 0, 1), span);
                self.emit_load_value(Value::TRUE, dst, span)?;
                self.free_to(m);
            }
            ("car", [x]) => {
                let m = self.mark();
                let r = self.operand(x)?;
                self.emit(Insn::iabc(Op::Car, dst, r, 0), span);
                self.free_to(m);
            }
            ("cdr", [x]) => {
                let m = self.mark();
                let r = self.operand(x)?;
                self.emit(Insn::iabc(Op::Cdr, dst, r, 0), span);
                self.free_to(m);
            }
            ("cons", [x, y]) => {
                let m = self.mark();
                let rx = self.operand(x)?;
                let ry = self.operand(y)?;
                self.emit(Insn::iabc(Op::Cons, dst, rx, ry), span);
                self.free_to(m);
            }
            _ => return Ok(None),
        }
        Ok(Some(false))
    }

    fn emit_addi(
        &mut self,
        x: &Ir,
        imm: Option<i8>,
        dst: u8,
        span: Span,
    ) -> Result<(), CompileError> {
        let Some(imm) = imm else {
            return Err(CompileError::Internal {
                detail: "ADDI peephole chosen without an immediate".to_string(),
            });
        };
        let m = self.mark();
        let r = self.operand(x)?;
        self.emit(Insn::iabc(Op::AddI, dst, r, imm as u8), span);
        self.free_to(m);
        Ok(())
    }

    /// Left fold of a licensed n-ary arithmetic primitive (n ≥ 2). Intermediates
    /// accumulate in a reserved temporary; only the final instruction writes `dst`, so
    /// `dst` may alias an operand register (a local being assigned) without clobbering
    /// it early. The first element is taken in place — a local operand needs no MOVE.
    fn fold(&mut self, op: Op, args: &[Ir], dst: u8, span: Span) -> Result<(), CompileError> {
        let m = self.mark();
        let acc = self.alloc(span)?;
        let mut src = self.operand(&args[0])?;
        for (i, arg) in args.iter().enumerate().skip(1) {
            let inner = self.mark();
            let r = self.operand(arg)?;
            let target = if i == args.len() - 1 { dst } else { acc };
            self.emit(Insn::iabc(op, target, src, r), span);
            self.free_to(inner);
            src = acc;
        }
        self.free_to(m);
        Ok(())
    }

    /// A licensed native without a dedicated opcode, with statically compatible arity
    /// and a byte-sized table index, compiles to `PRIMCALL`.
    fn emit_primcall(
        &mut self,
        name: &str,
        args: &[Ir],
        dst: u8,
        tail: bool,
        span: Span,
    ) -> Result<Option<bool>, CompileError> {
        let Some((index, def)) = self.prims.lookup(name) else {
            return Ok(None);
        };
        let Ok(index) = u8::try_from(index) else {
            return Ok(None);
        };
        let n = args.len();
        if n < def.min_args || def.max_args.is_some_and(|max| n > max) {
            // Compile the honest call instead; the runtime arity error names the prim.
            return Ok(None);
        }
        let m = self.mark();
        let ra = self.alloc(span)?;
        for arg in args {
            let r = self.alloc(arg.span())?;
            self.expr(arg, r, false)?;
        }
        self.emit(Insn::iabc(Op::PrimCall, ra, n as u8 + 1, index), span);
        if tail {
            self.emit(Insn::iabc(Op::Return1, ra, 0, 0), span);
            self.free_to(m);
            return Ok(Some(true));
        }
        if dst != ra {
            self.emit(Insn::iabc(Op::Move, dst, ra, 0), span);
        }
        self.free_to(m);
        Ok(Some(false))
    }
}

/// The `value::layout` singleton ordinal of `v`, if it is a singleton. Each constant is
/// paired with its own layout ordinal — not a position in a local array — so a
/// reordering in `layout.rs` re-maps this automatically instead of drifting into a
/// second table.
fn singleton_ordinal(v: Value) -> Option<u16> {
    use crate::value::layout::{
        SINGLETON_EOF, SINGLETON_FALSE, SINGLETON_NULL, SINGLETON_TRUE, SINGLETON_UNDEFINED,
        SINGLETON_UNSPECIFIED,
    };
    [
        (Value::UNDEFINED, SINGLETON_UNDEFINED),
        (Value::UNSPECIFIED, SINGLETON_UNSPECIFIED),
        (Value::NIL, SINGLETON_NULL),
        (Value::EOF, SINGLETON_EOF),
        (Value::FALSE, SINGLETON_FALSE),
        (Value::TRUE, SINGLETON_TRUE),
    ]
    .iter()
    .find(|&&(s, _)| s == v)
    .map(|&(_, ordinal)| ordinal as u16)
}

/// A literal integer that fits `ADDI`'s signed byte.
fn addi_immediate(ir: &Ir) -> Option<i8> {
    match ir {
        Ir::Datum(Expr::Integer(n, _)) => i8::try_from(*n).ok(),
        _ => None,
    }
}

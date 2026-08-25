//! The Ruse register VM (RBC-1).
//!
//! Inspired by Lua's register-based VM
//! (<https://www.mcours.net/cours/pdf/hasclic3/hasssclic818.pdf>). Each call frame owns a
//! window of up to **250** registers (`R0`–`R249`) within a flat per-fiber register array
//! (`VmState::regs`); a callee's window is rebased so its `r0` aliases the caller's
//! `R[A+1]`, which is what makes argument passing free. 8-bit operand fields address
//! registers 0–255; values 250–255 are reserved for future addressing-mode escapes.
//!
//! The instruction set (50 opcodes) is specified in `ruse-bytecode-spec.md`. The dispatch
//! loop trusts the load-time verifier ([`crate::bytecode::verify()`]) for every *static*
//! operand — [`Vm::execute`] runs it before the first instruction — and the semantics of
//! each opcode live in [`crate::rt`] as standalone functions, per habit 2 of
//! `docs/project_plan.org`: the arms here only decode operands and route.
//!
//! # The M3 slice
//!
//! Everything the M3 compiler can emit executes: data movement, fixnum-fast arithmetic,
//! the skip-family comparisons, jumps, pairs and vectors, capture-free closures, global
//! access, `CALL`/`TAILCALL`/`RETURN`/`RETURN1`, and `PRIMCALL`. `TAILCALL` reuses the
//! current frame unconditionally — constant-space tail recursion is R7RS §3.5, not an
//! optimization. Opcodes belonging to later milestones (upvalues, first-class control,
//! open-ended argument/result counts, `DIV`'s exact rationals) return a typed
//! [`VmErrorKind::Unimplemented`] instead of `unreachable!`, so a hand-assembled
//! prototype degrades into a diagnostic rather than a crash.
//!
//! # Collection happens only at the safepoint
//!
//! The top of the dispatch loop is the one place a collection runs (`roots::safepoint`),
//! where the frame stack plus the live register windows are a complete root set. Windows
//! are cleared to `undefined` at frame entry, so the root walk can report every register
//! of every live window without ever reading a stale word; native primitives run entirely
//! between safepoints and never trigger one.

pub mod error;
pub mod globals;
mod roots;

use std::io::Write;
use std::rc::Rc;

use crate::bytecode::{Insn, Op, Proto, verify};
use crate::gc::Heap;
use crate::rt;
use crate::rt::prims::{NativeCtx, NativeFn, PrimDef, PrimTable};
use crate::span::Span;
use crate::value::Value;
use crate::value::object::{Closure, NativeProc, Symbol};

use error::{VmError, VmErrorKind};
use globals::Globals;

/// Default ceiling on non-tail call depth. Deep recursion in non-tail position hits this
/// as a typed error instead of exhausting memory; tail calls never consume frames, which
/// is what the M3 exit criterion measures.
const DEFAULT_FRAME_LIMIT: usize = 10_000;

/// One activation: which code is running, where its window starts, and where its results
/// go in the caller's window.
pub(crate) struct Frame {
    pub(crate) proto: Rc<Proto>,
    /// The callee closure — a heap value the root set keeps alive — or `undefined` for a
    /// top-level chunk, which has no closure object.
    pub(crate) closure: Value,
    /// Absolute index of this frame's `r0` in the flat register file.
    pub(crate) base: usize,
    /// Index of the *next* instruction to execute.
    pub(crate) pc: usize,
    /// Absolute register index in the caller's window where results land (the caller's
    /// `R[A]` of the `CALL`).
    ret_base: usize,
    /// The `CALL`'s C operand: `1` discards the result, `2` stores one value; `0` (all
    /// results) is M7's.
    nresults: u8,
}

/// Everything the VM owns besides the heap: the register file, frame stack and globals.
///
/// Split from [`Vm`] so a safepoint can borrow the heap mutably and the state immutably
/// at once — the shape `Heap::collect`'s documentation prescribes for root sets.
pub(crate) struct VmState {
    pub(crate) regs: Vec<Value>,
    pub(crate) frames: Vec<Frame>,
    pub(crate) globals: Globals,
    /// The previous top-level result, kept rooted so an embedder-held [`Value`] from the
    /// last execution survives the next one's collections.
    pub(crate) last_result: Value,
    /// The prototype [`Vm::compile_only`] most recently handed out, kept rooted so its
    /// constants survive evaluations that run before the embedder executes it. A
    /// prototype on a frame or captured by a closure is rooted through those instead;
    /// this slot is only for the compiled-but-not-yet-running window.
    pub(crate) compiled: Option<Rc<Proto>>,
}

/// How one dispatched instruction left the loop.
enum Flow {
    Continue,
    /// The outermost frame returned this value.
    Done(Value),
}

/// What kind of callable sits in a register.
enum Callee {
    Closure(Rc<Proto>),
    Native(u32),
    NotCallable,
}

/// The virtual machine: heap, state, native-function table, and output sink.
pub struct Vm {
    pub(crate) heap: Heap,
    pub(crate) state: VmState,
    prims: PrimTable,
    out: Box<dyn Write>,
    frame_limit: usize,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    /// A VM with the standard primitives installed, writing to stdout.
    pub fn new() -> Self {
        Self::with_output(Box::new(std::io::stdout()))
    }

    /// A VM writing `display`/`write`/`newline` output to `out`. Tests hand in a buffer.
    pub fn with_output(out: Box<dyn Write>) -> Self {
        let mut heap = Heap::new();
        let mut globals = Globals::default();
        let mut prims = PrimTable::default();
        rt::prims::install(&mut prims, &mut heap, &mut globals);
        Self {
            heap,
            state: VmState {
                regs: Vec::new(),
                frames: Vec::new(),
                globals,
                last_result: Value::UNSPECIFIED,
                compiled: None,
            },
            prims,
            out,
            frame_limit: DEFAULT_FRAME_LIMIT,
        }
    }

    /// The heap values live in. Compilation allocates constants here.
    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    /// Mutable heap access, for embedders building values.
    ///
    /// A [`Value`] built here is a root of nothing: it stays valid only until the next
    /// evaluation (`eval_str`/`eval_expr`/`execute`), whose collections free anything
    /// the VM cannot reach. To hold one longer, pin it through [`Vm::pins`] or store it
    /// in a global.
    pub fn heap_mut(&mut self) -> &mut Heap {
        &mut self.heap
    }

    /// The pin stack: the supported way for an embedder to keep a [`Value`] alive across
    /// evaluations. Values pinned through a live scope are GC roots.
    ///
    /// ```
    /// # use ruse::Vm;
    /// let mut vm = Vm::with_output(Box::new(Vec::new()));
    /// let pins = vm.pins();
    /// let scope = pins.scope();
    /// let kept = scope.pin(vm.eval_str("(cons 1 2)").unwrap());
    /// vm.eval_str("(define (burn n) (if (= n 0) 0 (burn (- n 1)))) (burn 1000)")
    ///     .unwrap();
    /// assert!(vm.heap().get::<ruse::value::object::Pair>(kept.get()).is_some());
    /// ```
    pub fn pins(&self) -> std::rc::Rc<crate::gc::PinStack> {
        self.heap.pins()
    }

    /// Cap non-tail call depth. The M3 tail-call test runs a million-iteration loop under
    /// a tiny limit: only genuine frame reuse can pass it.
    pub fn set_frame_limit(&mut self, limit: usize) {
        self.frame_limit = limit;
    }

    /// The current value of the global named `name`, if bound.
    ///
    /// The returned [`Value`] is rooted only for as long as the global still holds it:
    /// once Scheme code rebinds the name, the old value is collectable at the next
    /// evaluation's safepoints. Read what you need from it before evaluating more code,
    /// or pin it through [`Vm::pins`].
    pub fn global(&mut self, name: &str) -> Option<Value> {
        let sym = self.heap.symbol(name);
        self.state.globals.lookup_value(sym)
    }

    /// Register a native procedure: it gets a table index (usable by `PRIMCALL` while it
    /// fits in a byte) and a global binding under `name`.
    pub fn register_native(
        &mut self,
        name: &str,
        min_args: usize,
        max_args: Option<usize>,
        func: NativeFn,
    ) -> u32 {
        let index = self.prims.register(PrimDef {
            name: name.to_string(),
            min_args,
            max_args,
            func,
        });
        let proc = self.heap.native_proc(name, index);
        let sym = self.heap.symbol(name);
        self.state.globals.define_builtin(sym, proc);
        index
    }

    // ---------------------------------------------------------------- evaluation

    /// Read → compile → run every form in `src`; the last form's value comes back.
    ///
    /// Each form is compiled against the globals as the previous forms left them, which
    /// is what gives a `define` in one form effect over the next — including the
    /// primitive-inlining licence being revoked by a redefinition.
    pub fn eval_str(&mut self, src: &str) -> Result<Value, error::RuseError> {
        let exprs = crate::parser::Parser::parse_from_str(src)?;
        let mut last = Value::UNSPECIFIED;
        for expr in &exprs {
            last = self.eval_expr(expr)?;
        }
        Ok(last)
    }

    /// Compile → verify → run one form.
    ///
    /// A top-level `(begin …)` is spliced here, before compilation: R7RS §5.1 makes it
    /// a *sequence* of top-level forms, so each subform must compile against the world
    /// its predecessors left behind — `(begin (define + -) (+ 3 4))` is −1, because the
    /// `define` has revoked `+`'s inlining licence by the time the call compiles.
    ///
    /// The returned value stays rooted until the next execution, like
    /// [`Vm::execute`]'s.
    pub fn eval_expr(&mut self, expr: &crate::ast::Expr) -> Result<Value, error::RuseError> {
        // At top level nothing is lexically bound, so a list headed by the symbol
        // `begin` is always the special form.
        if let crate::ast::Expr::List(elems, _) = expr
            && let Some(crate::ast::Expr::Symbol(head, _)) = elems.first()
            && head == "begin"
        {
            let mut last = Value::UNSPECIFIED;
            for sub in &elems[1..] {
                last = self.eval_expr(sub)?;
            }
            return Ok(last);
        }
        // Compile and execute back to back: no collection can run in between, so the
        // fresh prototype's constants need no root yet — execute's frame becomes one.
        let proto =
            crate::compiler::compile(&mut self.heap, &self.state.globals, &self.prims, expr)?;
        Ok(self.execute(proto)?)
    }

    /// Compile one form against the current globals without running it. What the
    /// disassembler and the compiler's snapshot tests look at.
    ///
    /// The returned prototype's heap constants (quoted data; interned symbols are
    /// permanent anyway) stay rooted **until the next `compile_only` call** replaces
    /// them — the VM retains the most recent compilation as a root, so the
    /// compile-then-eventually-execute pattern survives intervening evaluations. Two
    /// outstanding uncompiled prototypes at once are not supported; execute or drop the
    /// first before compiling the second.
    pub fn compile_only(
        &mut self,
        expr: &crate::ast::Expr,
    ) -> Result<Rc<Proto>, crate::compiler::CompileError> {
        let proto =
            crate::compiler::compile(&mut self.heap, &self.state.globals, &self.prims, expr)?;
        self.state.compiled = Some(Rc::clone(&proto));
        Ok(proto)
    }

    // ---------------------------------------------------------------- execution

    /// Verify `proto`, then run it as a top-level chunk and return its value.
    ///
    /// The returned [`Value`] stays rooted (as the VM's last result) until the next
    /// `execute`, so it is safe to hold exactly that long; read anything you need from it
    /// before running more code.
    pub fn execute(&mut self, proto: Rc<Proto>) -> Result<Value, VmError> {
        if !self.state.frames.is_empty() {
            return VmErrorKind::Internal {
                detail: "re-entrant execute".to_string(),
            }
            .err();
        }
        verify(&proto).map_err(|e| {
            VmError::new(VmErrorKind::Rejected {
                message: e.to_string(),
            })
        })?;

        self.state.regs.clear();
        self.state
            .regs
            .resize(usize::from(proto.max_window), Value::UNDEFINED);
        self.state.frames.push(Frame {
            proto,
            closure: Value::UNDEFINED,
            base: 0,
            pc: 0,
            ret_base: 0,
            nresults: 2,
        });

        let result = self.run();
        // Leave nothing behind either way: the next execute starts from a clean frame
        // stack, and no register survives to be misreported by a future root walk.
        self.state.frames.clear();
        self.state.regs.clear();
        if let Ok(v) = result {
            self.state.last_result = v;
        }
        result
    }

    fn run(&mut self) -> Result<Value, VmError> {
        loop {
            if self.heap.should_collect() {
                roots::safepoint(&mut self.heap, &self.state);
            }

            let Some(frame) = self.state.frames.last_mut() else {
                return VmErrorKind::Internal {
                    detail: "dispatch loop with no frame".to_string(),
                }
                .err();
            };
            let pc = frame.pc;
            let Some(&insn) = frame.proto.code.get(pc) else {
                return VmErrorKind::Internal {
                    detail: format!("pc {pc} ran off the end of verified code"),
                }
                .err();
            };
            // Captured before dispatch: an arm may push, replace or pop the frame, and
            // the error should still point at the instruction that raised it.
            let span = frame.proto.spans.get(pc).copied();
            frame.pc = pc + 1;

            match self.step(insn) {
                Ok(Flow::Continue) => {}
                Ok(Flow::Done(v)) => return Ok(v),
                Err(e) => return Err(attach(e, span)),
            }
        }
    }

    /// Execute one instruction. `frame.pc` already points past it.
    fn step(&mut self, insn: Insn) -> Result<Flow, VmError> {
        let Some(op) = insn.opcode() else {
            return VmErrorKind::Internal {
                detail: format!("unverified opcode byte {:#04x}", insn.op()),
            }
            .err();
        };
        let base = match self.state.frames.last() {
            Some(f) => f.base,
            None => 0,
        };
        let (a, b, c) = (
            usize::from(insn.a()),
            usize::from(insn.b()),
            usize::from(insn.c()),
        );

        match op {
            // ------------------------------------------------------ data movement
            Op::Move => {
                let v = self.reg(base + b);
                self.set_reg(base + a, v);
            }
            Op::LoadK => {
                let v = self.konst(usize::from(insn.bx()))?;
                self.set_reg(base + a, v);
            }
            Op::LoadKx => {
                // The verifier guarantees the EXTRAARG is present and never a jump
                // target; consume it here so it is never dispatched.
                let ax = {
                    let Some(f) = self.state.frames.last_mut() else {
                        return internal("LOADKX with no frame");
                    };
                    let Some(&extra) = f.proto.code.get(f.pc) else {
                        return internal("LOADKX without its EXTRAARG");
                    };
                    f.pc += 1;
                    extra.ax()
                };
                let v = self.konst(ax as usize)?;
                self.set_reg(base + a, v);
            }
            Op::LoadImm => {
                let Some(v) = singleton(insn.bx()) else {
                    return internal("unverified LOADIMM ordinal");
                };
                self.set_reg(base + a, v);
            }
            Op::LoadI => {
                let v = self.heap.integer(i64::from(insn.sbx()));
                self.set_reg(base + a, v);
            }

            // ------------------------------------------------------ arithmetic
            Op::Add => {
                let (x, y) = (self.reg(base + b), self.reg(base + c));
                let v = rt::arith::add(&mut self.heap, x, y)?;
                self.set_reg(base + a, v);
            }
            Op::Sub => {
                let (x, y) = (self.reg(base + b), self.reg(base + c));
                let v = rt::arith::sub(&mut self.heap, x, y)?;
                self.set_reg(base + a, v);
            }
            Op::Mul => {
                let (x, y) = (self.reg(base + b), self.reg(base + c));
                let v = rt::arith::mul(&mut self.heap, x, y)?;
                self.set_reg(base + a, v);
            }
            Op::Div => return unimplemented_op("exact division (DIV)", "M5"),
            Op::Quot => return unimplemented_op("integer division (QUOT)", "M5"),
            Op::Neg => {
                let x = self.reg(base + b);
                let v = rt::arith::neg(&mut self.heap, x)?;
                self.set_reg(base + a, v);
            }
            Op::AddI => {
                let x = self.reg(base + b);
                let v = rt::arith::addi(&mut self.heap, x, insn.sc())?;
                self.set_reg(base + a, v);
            }

            // ------------------------------------------------------ comparison (skip-next)
            Op::NumEq => {
                let cond = rt::arith::num_eq(&self.heap, self.reg(base + a), self.reg(base + b))?;
                self.skip_if(cond != (c == 1))?;
            }
            Op::NumLt => {
                let cond = rt::arith::num_lt(&self.heap, self.reg(base + a), self.reg(base + b))?;
                self.skip_if(cond != (c == 1))?;
            }
            Op::NumLe => {
                let cond = rt::arith::num_le(&self.heap, self.reg(base + a), self.reg(base + b))?;
                self.skip_if(cond != (c == 1))?;
            }
            Op::Eq => {
                let cond = self.reg(base + a) == self.reg(base + b);
                self.skip_if(cond != (c == 1))?;
            }
            Op::Eqv => {
                let cond = rt::equal::eqv(&self.heap, self.reg(base + a), self.reg(base + b));
                self.skip_if(cond != (c == 1))?;
            }
            Op::Test => {
                let cond = self.reg(base + a).truthy();
                self.skip_if(cond != (c == 1))?;
            }

            // ------------------------------------------------------ control flow
            Op::Jmp => {
                // `A > 0` also closes open upvalues from register A-1: with no open
                // upvalue list until M4, there is never anything to close, so the close
                // half is a correct no-op rather than an error.
                self.jump(insn.sbx())?;
            }
            Op::ExtraArg => {
                // Sequential flow skips it via LOADKX and the verifier bans transfers
                // onto it, so dispatching one means the loop itself is broken.
                return internal("EXTRAARG reached the dispatch loop");
            }
            Op::JmpIdx => return unimplemented_op("computed jumps (JMPIDX)", "M4"),

            // ------------------------------------------------------ pairs
            Op::Cons => {
                let (x, y) = (self.reg(base + b), self.reg(base + c));
                let v = self.heap.cons(x, y);
                self.set_reg(base + a, v);
            }
            Op::Car => {
                let v = rt::pairs::car(&self.heap, self.reg(base + b))?;
                self.set_reg(base + a, v);
            }
            Op::Cdr => {
                let v = rt::pairs::cdr(&self.heap, self.reg(base + b))?;
                self.set_reg(base + a, v);
            }
            Op::SetCar => {
                let (p, v) = (self.reg(base + a), self.reg(base + b));
                rt::pairs::set_car(&mut self.heap, p, v)?;
            }
            Op::SetCdr => {
                let (p, v) = (self.reg(base + a), self.reg(base + b));
                rt::pairs::set_cdr(&mut self.heap, p, v)?;
            }
            Op::Cadr => {
                return unimplemented_op("fused pair paths (CADR)", "the open-question-1 freeze");
            }
            Op::TypeP => {
                return unimplemented_op("type predicates (TYPEP)", "the open-question-2 freeze");
            }

            // ------------------------------------------------------ calls and returns
            Op::Call => return self.call(base, a, insn.b(), insn.c()),
            Op::TailCall => return self.tail_call(base, a, insn.b()),
            Op::Return => match insn.b() {
                // Zero values delivered to a one-value context: R7RS leaves it
                // unspecified, and the unspecified value is exactly what we have.
                1 => return self.do_return(Value::UNSPECIFIED),
                2 => {
                    let v = self.reg(base + a);
                    return self.do_return(v);
                }
                _ => return unimplemented_op("multiple return values (RETURN)", "M7"),
            },
            Op::Return1 => {
                let v = self.reg(base + a);
                return self.do_return(v);
            }
            Op::Apply => return unimplemented_op("argument spreading (APPLY)", "M9"),

            // ------------------------------------------------------ closures and variables
            Op::Closure => {
                let child = {
                    let Some(f) = self.state.frames.last() else {
                        return internal("CLOSURE with no frame");
                    };
                    match f.proto.protos.get(usize::from(insn.bx())) {
                        Some(child) => Rc::clone(child),
                        None => return internal("unverified CLOSURE child index"),
                    }
                };
                if !child.upvals.is_empty() {
                    return unimplemented_op("upvalue capture (CLOSURE)", "M4");
                }
                let v = self.heap.closure(child, Vec::new());
                self.set_reg(base + a, v);
            }
            Op::GetUpval | Op::SetUpval => {
                return unimplemented_op("upvalues (GETUPVAL/SETUPVAL)", "M4");
            }
            Op::CloseUpvals => return unimplemented_op("upvalue closing (CLOSEUPVALS)", "M4"),
            Op::GetGlobal => {
                let sym = self.global_key(usize::from(insn.bx()))?;
                let bound = self
                    .state
                    .globals
                    .resolve(sym)
                    .map(|slot| self.state.globals.get(slot))
                    .filter(|v| !v.is_undefined());
                match bound {
                    Some(v) => self.set_reg(base + a, v),
                    None => {
                        return VmErrorKind::UnboundVariable {
                            name: self.symbol_name(sym),
                        }
                        .err();
                    }
                }
            }
            Op::SetGlobal => {
                let sym = self.global_key(usize::from(insn.bx()))?;
                let slot = self.state.globals.intern(sym);
                let v = self.reg(base + a);
                self.state.globals.set(slot, v);
            }
            Op::GetLocalN => return unimplemented_op("GETLOCALN", "a future inliner"),

            // ------------------------------------------------------ vectors and the bridge
            Op::VecRef => {
                let (vec, idx) = (self.reg(base + b), self.reg(base + c));
                let v = rt::vectors::vec_ref(&self.heap, vec, idx)?;
                self.set_reg(base + a, v);
            }
            Op::VecSet => {
                let (vec, idx, v) = (self.reg(base + a), self.reg(base + b), self.reg(base + c));
                rt::vectors::vec_set(&mut self.heap, vec, idx, v)?;
            }
            Op::NewVec => {
                let (len, fill) = (self.reg(base + b), self.reg(base + c));
                let v = rt::vectors::make_vector(&mut self.heap, len, fill)?;
                self.set_reg(base + a, v);
            }
            Op::PrimCall => {
                if insn.b() == 0 {
                    return unimplemented_op("open-ended argument counts (B=0)", "M7");
                }
                let v = self.run_native(insn.c().into(), base + a + 1, b - 1)?;
                self.set_reg(base + a, v);
            }

            // ------------------------------------------------------ first-class control
            Op::CaptureCc => return unimplemented_op("continuations (CAPTURECC)", "M7"),
            Op::WindPush | Op::WindPop => {
                return unimplemented_op("dynamic-wind (WINDPUSH/WINDPOP)", "M7");
            }
            Op::HandlerPush | Op::HandlerPop => {
                return unimplemented_op("exception handlers (HANDLERPUSH/HANDLERPOP)", "M7");
            }
            Op::Raise => return unimplemented_op("raise (RAISE)", "M7"),
        }
        Ok(Flow::Continue)
    }

    // ---------------------------------------------------------------- calls

    /// `CALL A B C`: invoke `R[A]` with `B-1` args; per C, discard or store the result.
    fn call(&mut self, base: usize, a: usize, b: u8, c: u8) -> Result<Flow, VmError> {
        if b == 0 {
            return unimplemented_op("open-ended argument counts (B=0)", "M7");
        }
        if c == 0 || c > 2 {
            return unimplemented_op("multiple-value result contexts (C=0 or C>2)", "M7");
        }
        let nargs = usize::from(b) - 1;
        let callee = self.reg(base + a);

        match self.callee_kind(callee) {
            Callee::Closure(proto) => {
                if self.state.frames.len() >= self.frame_limit {
                    return VmErrorKind::StackOverflow {
                        limit: self.frame_limit,
                    }
                    .err();
                }
                self.check_scheme_arity(&proto, nargs)?;
                let new_base = base + a + 1;
                self.ensure_window(new_base, proto.max_window, nargs);
                self.state.frames.push(Frame {
                    proto,
                    closure: callee,
                    base: new_base,
                    pc: 0,
                    ret_base: base + a,
                    nresults: c,
                });
                Ok(Flow::Continue)
            }
            Callee::Native(index) => {
                let v = self.run_native(index, base + a + 1, nargs)?;
                if c == 2 {
                    self.set_reg(base + a, v);
                }
                Ok(Flow::Continue)
            }
            Callee::NotCallable => VmErrorKind::NotCallable {
                got: rt::type_name(&self.heap, callee),
            }
            .err(),
        }
    }

    /// `TAILCALL A B`: replace the current frame. Never grows the frame stack — this is
    /// R7RS §3.5's guarantee, and the reason deep tail recursion runs in constant space.
    fn tail_call(&mut self, base: usize, a: usize, b: u8) -> Result<Flow, VmError> {
        if b == 0 {
            return unimplemented_op("open-ended argument counts (B=0)", "M7");
        }
        let nargs = usize::from(b) - 1;
        let callee = self.reg(base + a);

        match self.callee_kind(callee) {
            Callee::Closure(proto) => {
                self.check_scheme_arity(&proto, nargs)?;
                // Slide the arguments down to the head of the reused window; source
                // starts above destination, so a forward copy cannot clobber its input.
                for i in 0..nargs {
                    let v = self.reg(base + a + 1 + i);
                    self.set_reg(base + i, v);
                }
                self.ensure_window(base, proto.max_window, nargs);
                let Some(f) = self.state.frames.last_mut() else {
                    return internal("TAILCALL with no frame");
                };
                f.proto = proto;
                f.closure = callee;
                f.pc = 0;
                Ok(Flow::Continue)
            }
            // A tail call to a native runs it and returns its value to the caller of the
            // *current* frame, exactly as call-then-return would.
            Callee::Native(index) => {
                let v = self.run_native(index, base + a + 1, nargs)?;
                self.do_return(v)
            }
            Callee::NotCallable => VmErrorKind::NotCallable {
                got: rt::type_name(&self.heap, callee),
            }
            .err(),
        }
    }

    /// Return `v` from the current frame to its caller — or out of [`Vm::execute`].
    fn do_return(&mut self, v: Value) -> Result<Flow, VmError> {
        let Some(frame) = self.state.frames.pop() else {
            return internal("RETURN with no frame");
        };
        if self.state.frames.is_empty() {
            return Ok(Flow::Done(v));
        }
        match frame.nresults {
            1 => {}
            2 => self.set_reg(frame.ret_base, v),
            _ => return internal("frame pushed with an unvalidated result count"),
        }
        Ok(Flow::Continue)
    }

    fn callee_kind(&self, callee: Value) -> Callee {
        if let Some(clos) = self.heap.get::<Closure>(callee) {
            return Callee::Closure(Rc::clone(&clos.proto));
        }
        if let Some(native) = self.heap.get::<NativeProc>(callee) {
            return Callee::Native(native.index);
        }
        Callee::NotCallable
    }

    fn check_scheme_arity(&self, proto: &Proto, nargs: usize) -> Result<(), VmError> {
        if proto.has_rest {
            return unimplemented_res("rest arguments", "M4");
        }
        if nargs != usize::from(proto.nparams) {
            return VmErrorKind::WrongArity {
                name: proto
                    .name
                    .clone()
                    .unwrap_or_else(|| "#<procedure>".to_string()),
                expected: proto.nparams.to_string(),
                got: nargs,
            }
            .err();
        }
        Ok(())
    }

    /// Run native `index` on the `nargs` values starting at absolute register
    /// `args_base`. Runs framelessly: no bytecode frame, no safepoint.
    fn run_native(&mut self, index: u32, args_base: usize, nargs: usize) -> Result<Value, VmError> {
        let (func, min, max, name) = match self.prims.get(index) {
            Some(def) => (def.func, def.min_args, def.max_args, def.name.clone()),
            // Reachable with malformed input, not through a VM bug: PRIMCALL's index is
            // a residual obligation the verifier leaves to the VM, and a NativeProc can
            // be minted against the wrong VM's table through the public heap API.
            None => return VmErrorKind::UnknownNative { index }.err(),
        };
        if nargs < min || max.is_some_and(|max| nargs > max) {
            return VmErrorKind::WrongArity {
                name,
                expected: PrimDef::expected_text(min, max),
                got: nargs,
            }
            .err();
        }
        // Arguments are copied out (Values are one word) so the native can borrow the
        // heap mutably without aliasing the register file.
        let args: Vec<Value> = self
            .state
            .regs
            .get(args_base..args_base + nargs)
            .map(<[Value]>::to_vec)
            .unwrap_or_default();
        let mut ctx = NativeCtx {
            heap: &mut self.heap,
            out: &mut *self.out,
            globals: &mut self.state.globals,
        };
        (func)(&mut ctx, &args)
    }

    // ---------------------------------------------------------------- registers

    /// Grow the register file to cover `[base, base + window)` and clear everything above
    /// the `live` already-written prefix (the arguments). A popped deeper frame may have
    /// left dangling values there, and the root walk reports whole windows.
    fn ensure_window(&mut self, base: usize, window: u8, live: usize) {
        let end = base + usize::from(window);
        if self.state.regs.len() < end {
            self.state.regs.resize(end, Value::UNDEFINED);
        }
        let from = (base + live).min(end);
        for slot in &mut self.state.regs[from..end] {
            *slot = Value::UNDEFINED;
        }
    }

    #[inline]
    fn reg(&self, i: usize) -> Value {
        debug_assert!(i < self.state.regs.len(), "register read out of window");
        self.state.regs.get(i).copied().unwrap_or(Value::UNDEFINED)
    }

    #[inline]
    fn set_reg(&mut self, i: usize, v: Value) {
        debug_assert!(i < self.state.regs.len(), "register write out of window");
        if let Some(slot) = self.state.regs.get_mut(i) {
            *slot = v;
        }
    }

    // ---------------------------------------------------------------- frame helpers

    fn skip_if(&mut self, skip: bool) -> Result<(), VmError> {
        if skip {
            let Some(f) = self.state.frames.last_mut() else {
                return internal_unit("skip with no frame");
            };
            f.pc += 1;
        }
        Ok(())
    }

    fn jump(&mut self, sbx: i16) -> Result<(), VmError> {
        let Some(f) = self.state.frames.last_mut() else {
            return internal_unit("jump with no frame");
        };
        // `f.pc` already points past the JMP; the verifier proved the target in bounds.
        let target = f.pc as i64 + i64::from(sbx);
        match usize::try_from(target) {
            Ok(target) => {
                f.pc = target;
                Ok(())
            }
            Err(_) => internal_unit("verified jump left the code"),
        }
    }

    fn konst(&self, index: usize) -> Result<Value, VmError> {
        let Some(f) = self.state.frames.last() else {
            return internal_res("constant read with no frame");
        };
        match f.proto.consts.get(index) {
            Some(&v) => Ok(v),
            None => internal_res("unverified constant index"),
        }
    }

    /// A `GETGLOBAL`/`SETGLOBAL` key: the constant must be a symbol. The verifier cannot
    /// check that (it has no heap), so this is the "slot linker" obligation the spec
    /// leaves to the VM, discharged on every access. A miss is a malformed prototype —
    /// hand-assembled, since the compiler only emits symbol keys — so it rejects rather
    /// than claiming a ruse bug.
    fn global_key(&self, index: usize) -> Result<Value, VmError> {
        let sym = self.konst(index)?;
        if self.heap.get::<Symbol>(sym).is_none() {
            return VmErrorKind::Rejected {
                message: format!("global key constant k{index} is not a symbol"),
            }
            .err();
        }
        Ok(sym)
    }

    fn symbol_name(&self, sym: Value) -> String {
        self.heap
            .get::<Symbol>(sym)
            .map_or_else(|| "#<symbol>".to_string(), |s| s.name.to_string())
    }
}

// ---------------------------------------------------------------- small helpers

fn attach(e: VmError, span: Option<Span>) -> VmError {
    match span {
        Some(span) => e.with_span(span),
        None => e,
    }
}

/// The `LOADIMM` operand is the `value::layout` singleton ordinal. Keyed off the layout
/// constants — like the verifier and the disassembler — so a reordering there re-maps
/// this automatically instead of drifting.
fn singleton(bx: u16) -> Option<Value> {
    use crate::value::layout::{
        SINGLETON_EOF, SINGLETON_FALSE, SINGLETON_NULL, SINGLETON_TRUE, SINGLETON_UNDEFINED,
        SINGLETON_UNSPECIFIED,
    };
    match u64::from(bx) {
        SINGLETON_UNDEFINED => Some(Value::UNDEFINED),
        SINGLETON_UNSPECIFIED => Some(Value::UNSPECIFIED),
        SINGLETON_NULL => Some(Value::NIL),
        SINGLETON_EOF => Some(Value::EOF),
        SINGLETON_FALSE => Some(Value::FALSE),
        SINGLETON_TRUE => Some(Value::TRUE),
        _ => None,
    }
}

fn unimplemented_op(what: &'static str, milestone: &'static str) -> Result<Flow, VmError> {
    Err(VmError::new(VmErrorKind::Unimplemented { what, milestone }))
}

fn unimplemented_res<T>(what: &'static str, milestone: &'static str) -> Result<T, VmError> {
    Err(VmError::new(VmErrorKind::Unimplemented { what, milestone }))
}

fn internal(detail: &str) -> Result<Flow, VmError> {
    Err(VmError::new(VmErrorKind::Internal {
        detail: detail.to_string(),
    }))
}

fn internal_res<T>(detail: &str) -> Result<T, VmError> {
    Err(VmError::new(VmErrorKind::Internal {
        detail: detail.to_string(),
    }))
}

fn internal_unit(detail: &str) -> Result<(), VmError> {
    Err(VmError::new(VmErrorKind::Internal {
        detail: detail.to_string(),
    }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn eval(vm: &mut Vm, src: &str) -> Value {
        let exprs = Parser::parse_from_str(src).unwrap();
        let mut last = Value::UNSPECIFIED;
        for e in &exprs {
            last = vm.eval_expr(e).unwrap();
        }
        last
    }

    fn quiet_vm() -> Vm {
        Vm::with_output(Box::new(Vec::new()))
    }

    #[test]
    fn a_hand_assembled_call_round_trips() {
        // child: (lambda (x) (+ x 1)); main: (child 41).
        let child = Rc::new(Proto {
            name: Some("inc".to_string()),
            code: vec![
                Insn::iabc(Op::AddI, 1, 0, 1),
                Insn::iabc(Op::Return1, 1, 0, 0),
            ],
            nparams: 1,
            max_window: 2,
            ..Proto::default()
        });
        let main = Rc::new(Proto {
            code: vec![
                Insn::iabx(Op::Closure, 0, 0),
                Insn::iasbx(Op::LoadI, 1, 41),
                Insn::iabc(Op::Call, 0, 2, 2),
                Insn::iabc(Op::Return1, 0, 0, 0),
            ],
            protos: vec![child],
            max_window: 2,
            ..Proto::default()
        });
        let mut vm = quiet_vm();
        let v = vm.execute(main).unwrap();
        assert_eq!(v.as_fixnum(), Some(42));
    }

    #[test]
    fn execute_rejects_a_malformed_prototype_at_the_boundary() {
        let bad = Rc::new(Proto {
            code: vec![Insn::iabc(Op::Return1, 9, 0, 0)],
            max_window: 1,
            ..Proto::default()
        });
        let err = quiet_vm().execute(bad).unwrap_err();
        assert!(matches!(err.kind, VmErrorKind::Rejected { .. }));
    }

    #[test]
    fn future_milestone_opcodes_are_typed_refusals() {
        let proto = Rc::new(Proto {
            code: vec![
                Insn::iabc(Op::CaptureCc, 0, 0, 0),
                Insn::iabc(Op::Return1, 0, 0, 0),
            ],
            max_window: 1,
            ..Proto::default()
        });
        let err = quiet_vm().execute(proto).unwrap_err();
        assert!(matches!(
            err.kind,
            VmErrorKind::Unimplemented {
                milestone: "M7",
                ..
            }
        ));
    }

    #[test]
    fn tail_calls_reuse_the_frame_under_a_tiny_limit() {
        const N: i64 = if cfg!(miri) { 300 } else { 20_000 };
        let mut vm = quiet_vm();
        vm.set_frame_limit(4);
        eval(
            &mut vm,
            "(define (loop n) (if (= n 0) 'done (loop (- n 1))))",
        );
        let done = eval(&mut vm, "'done");
        assert_eq!(eval(&mut vm, &format!("(loop {N})")), done);
    }

    /// The safety-critical path under Miri: collections fire at the safepoint while the
    /// register windows, frames and globals are live, and everything reachable survives.
    /// A hole in `VmState`'s `Trace` impl is a use-after-free this test makes loud.
    #[test]
    fn safepoint_collections_keep_every_live_register() {
        let mut vm = quiet_vm();
        // ~0.96 MB kept alive through a global pushes the heap near the 1 MB collection
        // floor, so the churn below crosses it mid-loop with live frames on the stack.
        eval(&mut vm, "(define keep (make-vector 120000 7))");
        let v = eval(
            &mut vm,
            "(define (churn n acc)
               (if (= n 0) acc (churn (- n 1) (car (cons (+ acc 1) '())))))
             (churn 3000 0)",
        );
        assert_eq!(v.as_fixnum(), Some(3000));
        assert!(
            vm.heap().collections() > 0,
            "the churn must have crossed a safepoint"
        );
        // The globally held vector survived the collections intact.
        assert_eq!(
            eval(&mut vm, "(vector-ref keep 119999)").as_fixnum(),
            Some(7)
        );
    }

    /// The adversarial review's reproduced use-after-free: a prototype compiled but not
    /// yet executed must survive intervening evaluations that collect. The `compiled`
    /// slot in `VmState` is the root that makes this hold.
    #[test]
    fn a_compiled_prototype_survives_intervening_collections() {
        const CHURN: i64 = if cfg!(miri) { 2_000 } else { 200_000 };
        let mut vm = quiet_vm();

        let exprs = Parser::parse_from_str("'(\"alpha\" \"beta\" (1 2 3))").unwrap();
        let proto = vm.compile_only(&exprs[0]).unwrap();

        // Enough garbage to cross the collection threshold several times over.
        eval(&mut vm, "(define keep (make-vector 120000 0))");
        eval(
            &mut vm,
            &format!(
                "(define (mk n acc) (if (= n 0) acc (mk (- n 1) (cons (cons n n) '()))))
                 (mk {CHURN} '())"
            ),
        );
        assert!(vm.heap().collections() > 0, "the churn must collect");

        let v = vm.execute(proto).unwrap();
        let text = crate::rt::write::value_to_string(vm.heap(), v, crate::rt::write::Style::Write);
        assert_eq!(text, "(\"alpha\" \"beta\" (1 2 3))");
    }

    /// R7RS §5.1: a top-level begin is a sequence of top-level forms, so a define inside
    /// it must revoke the inlining licence for the forms after it.
    #[test]
    fn a_top_level_begin_sequences_the_inlining_licence() {
        let mut vm = quiet_vm();
        let v = eval(&mut vm, "(begin (define + -) (+ 10 1))");
        assert_eq!(v.as_fixnum(), Some(9));
        // And the redefinition escapes the begin, like any top-level define.
        assert_eq!(eval(&mut vm, "(+ 5 2)").as_fixnum(), Some(3));
    }

    #[test]
    fn malformed_global_keys_and_native_indices_reject_rather_than_blame_ruse() {
        // GETGLOBAL whose key constant is a fixnum: passes the verifier (which only
        // bounds-checks), must reject at the trust boundary.
        let proto = Rc::new(Proto {
            code: vec![
                Insn::iabx(Op::GetGlobal, 0, 0),
                Insn::iabc(Op::Return1, 0, 0, 0),
            ],
            consts: vec![Value::fixnum(7).unwrap()],
            max_window: 1,
            ..Proto::default()
        });
        let mut vm = quiet_vm();
        let err = vm.execute(proto).unwrap_err();
        assert!(matches!(err.kind, VmErrorKind::Rejected { .. }), "{err}");

        // A NativeProc naming a table entry that does not exist.
        let bogus = vm.heap.native_proc("ghost", 9_999);
        let sym = vm.heap.symbol("ghost");
        vm.state.globals.define_builtin(sym, bogus);
        let exprs = Parser::parse_from_str("(ghost)").unwrap();
        let err = vm.eval_expr(&exprs[0]).unwrap_err();
        let error::RuseError::Vm(err) = err else {
            panic!("expected a runtime error, got {err}");
        };
        assert_eq!(err.kind, VmErrorKind::UnknownNative { index: 9_999 });
    }

    #[test]
    fn natives_work_through_call_tailcall_and_primcall() {
        let mut vm = quiet_vm();
        // PRIMCALL: licensed direct emission. CALL: through a variable. TAILCALL: a
        // native in tail position of a closure.
        assert_eq!(eval(&mut vm, "(car '(7 8))").as_fixnum(), Some(7));
        assert_eq!(
            eval(&mut vm, "(define f car) (f '(9 8))").as_fixnum(),
            Some(9)
        );
        assert_eq!(
            eval(&mut vm, "(define (g p) (car p)) (define h g) (h '(5))").as_fixnum(),
            Some(5)
        );
    }
}

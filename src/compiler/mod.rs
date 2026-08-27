//! The M3 compiler: one top-level [`Expr`] in, one verified-shape [`Proto`] out.
//!
//! Two passes, per decision C: [`ir`] lowers the reader's tree into a typed Core IR
//! (owning syntax — special forms, arities, keyword shadowing), and [`emit`] walks the
//! IR with a destination-register + tail-flag descent over a Lua-style `Func` register
//! allocator. There is no CPS and no ANF; the IR is the only intermediate form.
//!
//! Compilation allocates — quoted data and global-name symbols become heap values in the
//! constant pool — but never collects: the heap only collects at the VM's safepoints, so
//! in-flight constants need no rooting while the compiler runs. Once the prototype is
//! executing, the frame's `Rc<Proto>` (traced by the VM's root set) keeps them alive.

mod emit;
mod ir;

use std::rc::Rc;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::ast::Expr;
use crate::bytecode::Proto;
use crate::gc::Heap;
use crate::rt::prims::PrimTable;
use crate::span::Span;
use crate::vm::globals::Globals;

/// Compile one top-level form against the current global environment.
///
/// `globals` and `prims` steer the primitive-inlining licence: a name compiles to its
/// opcode or `PRIMCALL` only while it is the pristine boot-time binding, so a user
/// redefinition of `+` is honoured by every form compiled after it.
///
/// # The prototype's constants are rooted by nothing yet
///
/// Quoted data in the returned prototype's pool lives on `heap` but is reachable from no
/// GC root until a VM frame or a closure holds the prototype. Running any evaluation
/// before then can collect the constants out from under it. Prefer
/// [`Vm::compile_only`](crate::vm::Vm::compile_only), which retains the most recent
/// compilation as a root exactly to close that window.
pub fn compile(
    heap: &mut Heap,
    globals: &Globals,
    prims: &PrimTable,
    expr: &Expr,
) -> Result<Rc<Proto>, CompileError> {
    emit::compile_expr(heap, globals, prims, expr)
}

/// Why a form did not compile.
#[derive(Debug, Clone, PartialEq, Eq, Error, Diagnostic)]
pub enum CompileError {
    #[error("`{form}` is not supported until {milestone}")]
    #[diagnostic(help("the compiler grows milestone by milestone; see docs/project_plan.org"))]
    Unsupported {
        form: String,
        milestone: &'static str,
        #[label("not yet compilable")]
        span: SourceSpan,
    },
    #[error("bad {form} syntax: {detail}")]
    BadSyntax {
        form: &'static str,
        detail: String,
        #[label("here")]
        span: SourceSpan,
    },
    #[error("`{name}` is referenced before its letrec* initialization completes")]
    #[diagnostic(help(
        "a letrec/letrec* (or internal define) init may not read a binding whose own \
         init has not run yet; wrap the reference in a lambda"
    ))]
    PrematureReference {
        name: String,
        #[label("read before initialization")]
        span: SourceSpan,
    },
    #[error("this function needs {needed} registers; a frame's window is capped at {max}")]
    WindowOverflow {
        needed: usize,
        max: usize,
        #[label("in this function")]
        span: SourceSpan,
    },
    #[error("a branch here spans {distance} instructions, past the 16-bit jump range")]
    JumpTooFar {
        distance: i64,
        #[label("branch here")]
        span: SourceSpan,
    },
    #[error("too many {what} in one function ({count})")]
    TooMany {
        what: &'static str,
        count: usize,
        #[label("in this function")]
        span: SourceSpan,
    },
    #[error("internal compiler error (a ruse bug): {detail}")]
    Internal { detail: String },
}

/// [`Span`] → miette, used by both passes when building diagnostics.
pub(crate) fn ss(span: Span) -> SourceSpan {
    SourceSpan::new(span.start.into(), span.len())
}

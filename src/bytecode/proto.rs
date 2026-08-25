//! The compiled-function prototype: what the M3 compiler emits and the VM executes.

use std::rc::Rc;

use crate::bytecode::insn::Insn;
use crate::gc::Tracer;
use crate::span::Span;
use crate::value::Value;

/// Where one of a closure's captured variables comes from, resolved at compile time.
///
/// The descriptors live in the *child* prototype, in upvalue order; `CLOSURE` reads them
/// from there when it builds the closure. (The spec's draft described Lua-5.1-style
/// pseudo-instructions after `CLOSURE`; the ratified design is this table, which is
/// Lua 5.4's, and the spec has been amended to match.)
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UpvalDesc {
    /// Capture a register of the frame executing `CLOSURE` (an open upvalue until closed).
    ParentLocal(u8),
    /// Share an upvalue the enclosing closure already captured.
    ParentUpval(u8),
}

/// A compiled function: code, constants, capture descriptors, child prototypes, arity and
/// window metadata, and a provisional debug side table.
///
/// A `Proto` is *not* a heap object. It is immutable once built and shared by reference
/// counting: every closure over it holds an `Rc`, as does whatever loaded it. The prototype
/// tree is acyclic by construction — `Rc<Proto>` has no interior mutability, so a cycle
/// cannot be tied — which is what makes `Rc` sound here. One residual consequence of
/// `Rc`: dropping the last owner of a pathologically deep chain (~100k uniquely-owned
/// levels) recurses in `Rc`'s drop glue. Marking and verification are both
/// worklist-driven and depth-independent; real nesting is bounded by the reader's own
/// recursion long before either limit.
///
/// # The constants are the GC's business
///
/// `consts` holds [`Value`]s, which may point into a [`Heap`](crate::gc::Heap). The heap
/// does not know about prototypes, so something that traces — a
/// [`Closure`](crate::value::object::Closure), or in M3 the VM's own root set — must reach
/// every live `Proto` and report its constants via [`Proto::trace_values`]. A `Proto` no
/// root can reach does not keep its constants alive.
#[derive(Default)]
pub struct Proto {
    /// A name for listings and errors — the defined name when the compiler knows it.
    pub name: Option<String>,
    /// The instructions.
    pub code: Vec<Insn>,
    /// The constant pool, indexed by `LOADK`/`LOADKX`; `GETGLOBAL`/`SETGLOBAL` index it for
    /// their slot-name symbols.
    pub consts: Vec<Value>,
    /// Capture descriptors for this prototype's upvalues, in upvalue order.
    pub upvals: Vec<UpvalDesc>,
    /// Child prototypes, indexed by `CLOSURE`.
    pub protos: Vec<Rc<Proto>>,
    /// Fixed parameters, in registers `0..nparams` at entry.
    pub nparams: u8,
    /// Whether extra arguments are collected into a list in register `nparams` at entry.
    pub has_rest: bool,
    /// Registers this function's frame needs, at most [`Proto::MAX_WINDOW`].
    pub max_window: u8,
    /// Source span of each instruction, parallel to `code`; empty when debug info is
    /// stripped. Provisional answer to open question 5 (`docs/project_plan.org`): byte
    /// spans rather than line numbers, because the reader already produces them and a
    /// compressed encoding can be swapped in behind this field later.
    pub spans: Vec<Span>,
}

impl Proto {
    /// The most registers a frame may own. Operand values 250–255 are reserved for future
    /// addressing-mode escapes, so no instruction may ever name them as registers.
    pub const MAX_WINDOW: u8 = 250;

    /// Report every [`Value`] this prototype tree holds — its own constants and,
    /// recursively, its children's.
    ///
    /// Callable from any [`Trace`](crate::gc::Trace) implementation or
    /// [`HeapObject::trace_fields`](crate::value::object::HeapObject::trace_fields) that
    /// owns a `Proto`; `Proto` also implements [`Trace`](crate::gc::Trace) directly.
    /// Worklist-driven, like every other walk over prototype trees and heap objects in
    /// this crate: a hand-assembled chain can be arbitrarily deep, and the middle of a
    /// mark phase is the worst possible place to overflow the stack.
    pub fn trace_values(&self, tracer: &mut Tracer<'_>) {
        let mut work: Vec<&Proto> = vec![self];
        while let Some(proto) = work.pop() {
            for &k in &proto.consts {
                tracer.mark(k);
            }
            for child in &proto.protos {
                work.push(child);
            }
        }
    }
}

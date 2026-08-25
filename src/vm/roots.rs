//! The VM's root set, and the one safepoint that uses it.
//!
//! This file joins the crate's unsafe budget for exactly two items: the `unsafe impl
//! Trace` declaring what the VM keeps alive, and the `unsafe` call into
//! [`Heap::collect`] — the call site the resolved safe-versus-unsafe contract puts the
//! obligation on. Everything else about the VM is ordinary safe Rust.

#![allow(unsafe_code)]

use crate::gc::{Heap, Trace, Tracer};

use super::VmState;

// SAFETY: reports every `Value` the VM owns — each live frame's callee closure, its
// prototype-tree constants, and its full register window, plus the global table, the
// held last result, and the constants of the most recently compiled prototype. Windows
// are complete root sets by construction: every register in `[base, base + max_window)`
// was cleared to `undefined` at frame entry and only ever rewritten with live values.
// Registers above the deepest live window are neither reported nor ever read again, so
// stale words there are harmless.
unsafe impl Trace for VmState {
    fn trace(&self, tracer: &mut Tracer<'_>) {
        for frame in &self.frames {
            tracer.mark(frame.closure);
            frame.proto.trace_values(tracer);
            let end = (frame.base + usize::from(frame.proto.max_window)).min(self.regs.len());
            for &v in self.regs.get(frame.base..end).unwrap_or(&[]) {
                tracer.mark(v);
            }
        }
        self.globals.trace_into(tracer);
        tracer.mark(self.last_result);
        self.compiled.trace(tracer);
    }
}

/// The dispatch loop's safepoint: collect with the VM state as the root set.
pub(super) fn safepoint(heap: &mut Heap, state: &VmState) {
    // SAFETY: called only at an instruction boundary. The `Trace` impl above reports
    // every register of every live window, every frame's closure and prototype
    // constants, the globals, the held result, and the retained compiled prototype —
    // which is every `Value` the VM itself will read again. Native primitives run
    // entirely between safepoints and never trigger one, so no VM-internal Rust local
    // holds an unrooted `Value` here. Values an *embedder* holds are outside this claim:
    // their documented contract (`Vm::global`, `Vm::execute`, `Vm::compile_only`) is
    // validity until the next evaluation, with `Vm::pins` as the way to hold on longer.
    unsafe {
        heap.collect(state);
    }
}

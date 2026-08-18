//! Reachability: the grey worklist and the trait that feeds it.

use crate::gc::{LiveSet, STALE_VALUE};
use crate::value::Value;
use crate::value::layout::{Color, GcHeader};

/// The grey worklist, handed to [`Trace`] implementations during the mark phase.
///
/// Marking is worklist-driven rather than recursive on purpose: a Scheme heap routinely
/// holds lists hundreds of thousands of cells long, and a recursive mark would recurse once
/// per cell and overflow the Rust stack.
pub struct Tracer<'g> {
    gray: &'g mut Vec<*mut GcHeader>,
    live: &'g LiveSet,
}

impl<'g> Tracer<'g> {
    pub(crate) fn new(gray: &'g mut Vec<*mut GcHeader>, live: &'g LiveSet) -> Self {
        Self { gray, live }
    }

    /// Reach `v`. Immediates are ignored; a white heap object turns grey and joins the
    /// worklist. Marking the same object twice is free.
    ///
    /// Passing a [`Value`] that does not belong to the collecting heap, or that has already
    /// been freed, is undefined behaviour — that is the invariant every [`Trace`]
    /// implementation is on the hook for.
    #[inline]
    pub fn mark(&mut self, v: Value) {
        let Some(p) = v.header_ptr() else { return };
        // SAFETY: `p` came from a `Value` that a `Trace` implementation reported as live, so
        // by that trait's contract it points at an object this heap allocated and has not
        // freed.
        unsafe { self.mark_header(p) };
    }

    /// Reach an object the heap already holds a pointer to, without the round trip through
    /// a [`Value`]. Used for the collector's own root sets.
    ///
    /// # Safety
    ///
    /// `p` must point at a live object of the heap being collected.
    #[inline]
    pub(crate) unsafe fn mark_header(&mut self, p: *mut GcHeader) {
        debug_assert!(self.live.is_live(p), "{STALE_VALUE}");
        // SAFETY: the caller guarantees `p` is live; only the header is touched, and every
        // heap object has one at offset 0.
        unsafe {
            if (*p).color == Color::White {
                (*p).color = Color::Gray;
                self.gray.push(p);
            }
        }
    }
}

/// A root: something that can enumerate the values it keeps alive.
///
/// The VM's register file, the global slot vector, the frame stack and the wind and handler
/// stacks will each implement this; [`Heap::collect`](crate::gc::Heap::collect) takes one
/// and treats everything it reports, transitively, as live.
///
/// # Safety
///
/// An implementation must report **every** [`Value`] it owns, and must report only values
/// that belong to the heap being collected and are still live. Omitting one lets the
/// collector free an object that is still referenced; reporting a stale one dereferences
/// freed memory. Both are undefined behaviour, which is why this trait is `unsafe`.
pub unsafe trait Trace {
    /// Report every value this root holds.
    fn trace(&self, tracer: &mut Tracer<'_>);
}

// SAFETY: a value reports itself, which is exactly what it owns.
unsafe impl Trace for Value {
    #[inline]
    fn trace(&self, tracer: &mut Tracer<'_>) {
        tracer.mark(*self);
    }
}

// SAFETY: each of these forwards to the contained `Trace` implementations and holds nothing
// else. Their correctness is the correctness of what they contain.
unsafe impl<T: Trace + ?Sized> Trace for &T {
    #[inline]
    fn trace(&self, tracer: &mut Tracer<'_>) {
        (**self).trace(tracer);
    }
}

unsafe impl<T: Trace> Trace for [T] {
    #[inline]
    fn trace(&self, tracer: &mut Tracer<'_>) {
        for item in self {
            item.trace(tracer);
        }
    }
}

unsafe impl<T: Trace, const N: usize> Trace for [T; N] {
    #[inline]
    fn trace(&self, tracer: &mut Tracer<'_>) {
        self.as_slice().trace(tracer);
    }
}

unsafe impl<T: Trace> Trace for Vec<T> {
    #[inline]
    fn trace(&self, tracer: &mut Tracer<'_>) {
        self.as_slice().trace(tracer);
    }
}

unsafe impl<T: Trace> Trace for Option<T> {
    #[inline]
    fn trace(&self, tracer: &mut Tracer<'_>) {
        if let Some(inner) = self {
            inner.trace(tracer);
        }
    }
}

unsafe impl Trace for () {
    #[inline]
    fn trace(&self, _tracer: &mut Tracer<'_>) {}
}

unsafe impl<A: Trace, B: Trace> Trace for (A, B) {
    #[inline]
    fn trace(&self, tracer: &mut Tracer<'_>) {
        self.0.trace(tracer);
        self.1.trace(tracer);
    }
}

unsafe impl<A: Trace, B: Trace, C: Trace> Trace for (A, B, C) {
    #[inline]
    fn trace(&self, tracer: &mut Tracer<'_>) {
        self.0.trace(tracer);
        self.1.trace(tracer);
        self.2.trace(tracer);
    }
}

unsafe impl<A: Trace, B: Trace, C: Trace, D: Trace> Trace for (A, B, C, D) {
    #[inline]
    fn trace(&self, tracer: &mut Tracer<'_>) {
        self.0.trace(tracer);
        self.1.trace(tracer);
        self.2.trace(tracer);
        self.3.trace(tracer);
    }
}

unsafe impl<A: Trace, B: Trace, C: Trace, D: Trace, E: Trace> Trace for (A, B, C, D, E) {
    #[inline]
    fn trace(&self, tracer: &mut Tracer<'_>) {
        self.0.trace(tracer);
        self.1.trace(tracer);
        self.2.trace(tracer);
        self.3.trace(tracer);
        self.4.trace(tracer);
    }
}

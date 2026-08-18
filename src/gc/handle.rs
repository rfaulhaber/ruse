//! Pinning: how native code keeps a value alive across an allocation.
//!
//! A [`Value`] sitting in a Rust local is invisible to a precise collector. Native
//! procedures (the `PRIMCALL` table in M3) routinely build one object, then allocate a
//! second — and the first is unreachable from any root while that happens. This module is
//! the shadow stack that closes the hole:
//!
//! ```
//! # use ruse::gc::Heap;
//! # use ruse::Value;
//! let mut heap = Heap::new();
//! let pins = heap.pins();
//! let scope = pins.scope();
//!
//! let first = scope.pin(heap.cons(Value::TRUE, Value::NIL));
//! // `heap` is still freely mutable inside the scope: the scope borrows the pin stack,
//! // not the heap.
//! let second = heap.cons(Value::FALSE, first.get());
//! # let _ = second;
//! ```
//!
//! # Why slots, and not a stack discipline
//!
//! The obvious implementation — a `Vec<Value>`, a scope remembering the length it opened
//! at, and `truncate` on drop — is wrong in a way that only shows up under nesting. A pin
//! taken from an *enclosing* scope while an inner scope is open lands above the inner
//! scope's mark, so closing the inner scope silently unroots it; the next pin then reuses
//! that index and the outstanding handle starts reading a stranger's value. That is
//! precisely the pattern nested scopes exist for — a loop that accumulates one rooted
//! result while allocating temporaries per iteration.
//!
//! So a pin is a *slot*, not a position: slots are stable, each scope releases only the
//! slots it allocated, and released slots carry a generation counter so a stale handle is
//! caught rather than silently aliasing. Drop order stops mattering, and a leaked scope
//! over-roots — which wastes memory but is safe — instead of under-rooting, which is not.

// Re-tightens the `allow` its parent module needs. Pinning is ordinary safe Rust — slots,
// indices and a generation counter — and it should stay that way.
#![forbid(unsafe_code)]

use std::cell::RefCell;

use crate::value::Value;

/// One rooted value. Stable for the lifetime of the [`PinStack`]; reused only after the
/// scope that owned it releases it, and then with a bumped generation.
struct Slot {
    value: Value,
    /// Incremented on release, so a handle outstanding from a previous occupant is
    /// distinguishable from the current one.
    generation: u32,
    occupied: bool,
}

/// The shadow stack of pinned values, shared between a [`Heap`](crate::gc::Heap) and the
/// native code allocating against it.
#[derive(Default)]
pub struct PinStack {
    slots: RefCell<Vec<Slot>>,
    /// Indices available for reuse.
    free: RefCell<Vec<u32>>,
}

impl PinStack {
    /// Open a scope. Every value pinned through it is a root until the returned guard drops.
    pub fn scope(&self) -> PinScope<'_> {
        PinScope {
            pins: self,
            owned: RefCell::new(Vec::new()),
        }
    }

    /// How many values are pinned right now.
    pub fn len(&self) -> usize {
        self.slots.borrow().iter().filter(|s| s.occupied).count()
    }

    /// Whether nothing is pinned.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub(crate) fn trace_pins(&self, mut f: impl FnMut(Value)) {
        for slot in self.slots.borrow().iter() {
            if slot.occupied {
                f(slot.value);
            }
        }
    }
}

/// A pinning scope. Dropping it releases every pin made through it, and only those.
///
/// Scopes may be dropped in any order. Leaking one with [`core::mem::forget`] leaks its
/// pins: the values stay rooted forever, which wastes memory but is safe.
pub struct PinScope<'p> {
    pins: &'p PinStack,
    owned: RefCell<Vec<u32>>,
}

impl PinScope<'_> {
    /// Root `v` for the rest of this scope.
    pub fn pin(&self, v: Value) -> Pinned<'_> {
        let index = self.pins.free.borrow_mut().pop();
        let mut slots = self.pins.slots.borrow_mut();

        let (index, generation) = match index {
            Some(index) => {
                let slot = &mut slots[index as usize];
                slot.value = v;
                slot.occupied = true;
                (index, slot.generation)
            }
            None => {
                let index = u32::try_from(slots.len()).unwrap_or(u32::MAX);
                slots.push(Slot {
                    value: v,
                    generation: 0,
                    occupied: true,
                });
                (index, 0)
            }
        };

        self.owned.borrow_mut().push(index);
        Pinned {
            pins: self.pins,
            index,
            generation,
        }
    }
}

impl Drop for PinScope<'_> {
    fn drop(&mut self) {
        let mut slots = self.pins.slots.borrow_mut();
        let mut free = self.pins.free.borrow_mut();
        for &index in self.owned.borrow().iter() {
            let slot = &mut slots[index as usize];
            slot.occupied = false;
            // Not strictly needed — `trace_pins` skips unoccupied slots — but it stops a
            // released slot holding a stale pointer that a debugger or a future incremental
            // collector could trip over.
            slot.value = Value::UNDEFINED;
            slot.generation = slot.generation.wrapping_add(1);
            free.push(index);
        }
    }
}

/// A rooted value. Borrows its [`PinScope`], so it cannot outlive the pin.
///
/// It reads through the shadow stack rather than caching the value, so [`Pinned::set`] can
/// update a slot in place — which is how a native procedure keeps a running accumulator
/// rooted while it allocates.
#[derive(Clone, Copy)]
pub struct Pinned<'s> {
    pins: &'s PinStack,
    index: u32,
    generation: u32,
}

impl Pinned<'_> {
    /// The pinned value.
    pub fn get(self) -> Value {
        let slots = self.pins.slots.borrow();
        let slot = &slots[self.index as usize];
        debug_assert!(
            self.is_current(slot),
            "pin used after its scope released it"
        );
        slot.value
    }

    /// Replace the pinned value, keeping the slot.
    pub fn set(self, v: Value) {
        let mut slots = self.pins.slots.borrow_mut();
        let slot = &mut slots[self.index as usize];
        debug_assert!(
            self.is_current(slot),
            "pin used after its scope released it"
        );
        slot.value = v;
    }

    fn is_current(self, slot: &Slot) -> bool {
        slot.occupied && slot.generation == self.generation
    }
}

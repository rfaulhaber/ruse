//! The heap: allocation, symbol interning, and a precise mark-sweep collector.
//!
//! The collector is **precise** (it knows which words are pointers, because every root
//! declares them through [`Trace`]), **non-moving** (an object's address never changes, so
//! the register windows the VM will hand out in M3 never need fixing up), and
//! **stop-the-world tri-colour mark-sweep** with `Drop` run on sweep, which is what frees
//! the `String`, `Vec` and `BigInt` storage heap objects own on the Rust side.
//!
//! # Collection is explicit
//!
//! Allocation never collects. A precise collector can only free what no root reports, and a
//! collection triggered inside `cons` would reap every temporary the caller is holding in a
//! Rust local. So [`Heap::alloc`] grows the heap and [`Heap::should_collect`] reports when
//! it would like a collection; the VM decides where its safepoints are, at instruction
//! boundaries where the register file is a complete root set. Native code that must allocate
//! twice in a row pins across the gap — see [`handle`].

// The collector is the reason this crate has an `unsafe_code` budget at all: it hands out
// typed references derived from a tag byte, and it reconstructs `Box`es from raw pointers.
// The package denies unsafe; this module and `trace` are two of the four places that opt out.
#![allow(unsafe_code)]

pub mod handle;
mod trace;

pub use handle::{PinScope, PinStack, Pinned};
pub use trace::{Trace, Tracer};

use std::collections::HashMap;
use std::rc::Rc;

use num_bigint::BigInt;
use num_traits::ToPrimitive;

use crate::value::Value;
use crate::value::layout::{Color, GcHeader, HeapTag};
use crate::value::object::{
    Bignum, Bytevector, HeapObject, Pair, Record, RecordType, Str, Symbol, UpvalueCell, Vector,
    set_header,
};

/// Floor for the collection threshold: below this, collecting costs more than the memory it
/// returns.
const MIN_HEAP_BYTES: usize = 1 << 20;

/// How much the heap is allowed to grow past its post-collection size before the next
/// collection is due.
const HEAP_GROWTH_FACTOR: usize = 2;

/// Every address the heap currently owns, in debug builds only.
///
/// A `Value` is `Copy` and carries no lifetime, so one captured before a collection and used
/// after it is a use-after-free that nothing in the type system catches — and the damage is
/// usually silent, because a fresh allocation reuses the block and the stale `Value` starts
/// aliasing a *different* live object. This registry turns that into a `debug_assert!` at
/// the moment of the bad dereference, which is where it can still be traced back to the
/// caller that dropped the root.
///
/// It compiles to nothing in release builds: the field is `#[cfg]`-ed away and every method
/// becomes a no-op.
#[derive(Default)]
pub(crate) struct LiveSet {
    #[cfg(debug_assertions)]
    addrs: std::collections::HashSet<usize>,
}

impl LiveSet {
    pub(crate) fn insert(&mut self, p: *mut GcHeader) {
        #[cfg(debug_assertions)]
        self.addrs.insert(p.addr());
        #[cfg(not(debug_assertions))]
        let _ = p;
    }

    pub(crate) fn remove(&mut self, p: *mut GcHeader) {
        #[cfg(debug_assertions)]
        self.addrs.remove(&p.addr());
        #[cfg(not(debug_assertions))]
        let _ = p;
    }

    /// Whether the heap owns `p`. Unconditionally true in release builds, so callers must
    /// use it only inside `debug_assert!`.
    pub(crate) fn is_live(&self, p: *mut GcHeader) -> bool {
        #[cfg(debug_assertions)]
        {
            self.addrs.contains(&p.addr())
        }
        #[cfg(not(debug_assertions))]
        {
            let _ = p;
            true
        }
    }
}

/// The message every liveness assertion shares.
pub(crate) const STALE_VALUE: &str = "value points at an object this heap does not own: it was freed by      an earlier collection because no root reported it, or it belongs to a different heap";

/// What one collection did.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct GcStats {
    /// Objects on the heap before marking.
    pub live_before: usize,
    /// Objects still on the heap after sweeping.
    pub live_after: usize,
    /// Objects freed.
    pub freed: usize,
    /// Tracked bytes before marking.
    pub bytes_before: usize,
    /// Tracked bytes after sweeping.
    pub bytes_after: usize,
}

/// The Scheme heap.
///
/// Owns every heap object, the symbol interner, and the pin shadow stack. Dropping it frees
/// everything it allocated.
pub struct Heap {
    /// Head of the intrusive all-objects list the sweep walks.
    all: *mut GcHeader,
    live: usize,
    bytes: usize,
    next_gc: usize,
    gray: Vec<*mut GcHeader>,
    /// The interner. Keys share their `Rc<str>` buffer with the symbol's own name.
    symbols: HashMap<Rc<str>, *mut Symbol>,
    pins: Rc<PinStack>,
    collections: usize,
    live_set: LiveSet,
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

impl Heap {
    /// An empty heap.
    pub fn new() -> Self {
        Self {
            all: std::ptr::null_mut(),
            live: 0,
            bytes: 0,
            next_gc: MIN_HEAP_BYTES,
            gray: Vec::new(),
            symbols: HashMap::new(),
            pins: Rc::new(PinStack::default()),
            collections: 0,
            live_set: LiveSet::default(),
        }
    }

    // ---------------------------------------------------------------- allocation

    fn alloc_raw<T: HeapObject>(&mut self, mut obj: T) -> *mut T {
        set_header(&mut obj, GcHeader::linked(T::TAG, self.all));
        self.bytes += size_of::<T>() + obj.extra_bytes();
        let raw = Box::into_raw(Box::new(obj));
        let header = raw.cast::<GcHeader>();
        self.all = header;
        self.live += 1;
        self.live_set.insert(header);
        raw
    }

    fn alloc<T: HeapObject>(&mut self, obj: T) -> Value {
        let raw = self.alloc_raw(obj);
        Value::from_header_ptr(raw.cast::<GcHeader>())
    }

    /// Allocate a pair.
    pub fn cons(&mut self, car: Value, cdr: Value) -> Value {
        self.alloc(Pair::new(car, cdr))
    }

    /// Allocate a string.
    pub fn string(&mut self, chars: impl Into<String>) -> Value {
        self.alloc(Str::new(chars.into()))
    }

    /// Allocate a vector.
    pub fn vector(&mut self, elems: Vec<Value>) -> Value {
        self.alloc(Vector::new(elems))
    }

    /// Allocate a bytevector.
    pub fn bytevector(&mut self, bytes: Vec<u8>) -> Value {
        self.alloc(Bytevector::new(bytes))
    }

    /// Allocate a closed upvalue cell holding `value`.
    pub fn upvalue_cell(&mut self, value: Value) -> Value {
        self.alloc(UpvalueCell::new(value))
    }

    /// Allocate a record instance of `rtype`.
    pub fn record(&mut self, rtype: Value, fields: Vec<Value>) -> Value {
        self.alloc(Record::new(rtype, fields))
    }

    /// Allocate a record type descriptor. `name` and `field_names` are symbols.
    pub fn record_type(&mut self, name: Value, field_names: Vec<Value>) -> Value {
        self.alloc(RecordType::new(name, field_names))
    }

    /// The exact integer `n`, as a fixnum when it fits and a bignum when it does not.
    ///
    /// This and [`Heap::integer_from_big`] are the only two places in ruse that decide
    /// between the two representations. Everything upstream — the reader, the numeric tower,
    /// the arithmetic opcodes' overflow paths — funnels through them, so "when does an
    /// integer stop being immediate" has exactly one answer.
    ///
    /// ```
    /// # use ruse::gc::Heap;
    /// # use ruse::value::FIXNUM_MAX;
    /// let mut heap = Heap::new();
    /// assert!(heap.integer(1).is_fixnum());
    /// assert!(heap.integer(FIXNUM_MAX).is_fixnum());
    /// assert!(heap.integer(FIXNUM_MAX + 1).is_heap());
    /// ```
    pub fn integer(&mut self, n: i64) -> Value {
        match Value::fixnum(n) {
            Some(v) => v,
            None => self.alloc(Bignum::new(BigInt::from(n))),
        }
    }

    /// The exact integer `n`, demoted to a fixnum when it fits.
    ///
    /// Demotion is not cosmetic: `eqv?` and the arithmetic fast paths both key on the
    /// representation, so a bignum `1` left undemoted would silently take the slow path
    /// forever after.
    pub fn integer_from_big(&mut self, n: BigInt) -> Value {
        if let Some(small) = n.to_i64()
            && let Some(v) = Value::fixnum(small)
        {
            return v;
        }
        self.alloc(Bignum::new(n))
    }

    // ---------------------------------------------------------------- symbols

    /// The interned symbol named `name`, allocating it on first sight.
    ///
    /// Interning is what makes `eq?` on symbols a pointer comparison, which the compiler
    /// leans on for every special-form and keyword lookup.
    ///
    /// ```
    /// # use ruse::gc::Heap;
    /// let mut heap = Heap::new();
    /// assert_eq!(heap.symbol("lambda"), heap.symbol("lambda"));
    /// assert_ne!(heap.symbol("lambda"), heap.uninterned_symbol("lambda"));
    /// ```
    pub fn symbol(&mut self, name: &str) -> Value {
        if let Some(&existing) = self.symbols.get(name) {
            return Value::from_header_ptr(existing.cast::<GcHeader>());
        }
        let key: Rc<str> = Rc::from(name);
        let raw = self.alloc_raw(Symbol::new(Rc::clone(&key), true));
        self.symbols.insert(key, raw);
        Value::from_header_ptr(raw.cast::<GcHeader>())
    }

    /// A fresh symbol that is `eq?` only to itself, however it is spelled.
    ///
    /// The hygiene work in M8 needs these; the interner never sees them, so unlike interned
    /// symbols they are ordinary collectable objects.
    pub fn uninterned_symbol(&mut self, name: &str) -> Value {
        self.alloc(Symbol::new(Rc::from(name), false))
    }

    // ---------------------------------------------------------------- access

    /// Borrow the object `v` points at, if it is one and its tag is `T`.
    ///
    /// The borrow of the heap gives the *reference* a correct lifetime: it cannot be held
    /// across an allocation or a collection, because both need `&mut self`.
    ///
    /// # The obligation this does not discharge
    ///
    /// `v` must belong to this heap **and must not have been swept**. A [`Value`] is `Copy`
    /// and carries no lifetime, so one captured before a [`Heap::collect`] that did not
    /// reach it is dangling, and this will dereference it. Worse than a crash is the quiet
    /// case: a later allocation reuses the block and the stale `Value` starts naming a
    /// different live object.
    ///
    /// Nothing in the type system enforces this today; see the ADR noted on
    /// [`Heap::collect`]. What does exist is a `debug_assert!` against a registry of every
    /// address the heap owns, so a stale or cross-heap `Value` fails loudly in debug builds
    /// and under Miri rather than corrupting memory in release.
    pub fn get<T: HeapObject>(&self, v: Value) -> Option<&T> {
        let p = v.header_ptr()?;
        debug_assert!(self.live_set.is_live(p), "{STALE_VALUE}");
        // SAFETY: `p` points at a live object of this heap, which is the caller's obligation
        // above and is checked in debug builds. The tag check then establishes that the
        // object really is a `T`, and `HeapObject`'s contract puts its header at offset 0.
        unsafe {
            if (*p).tag != T::TAG {
                return None;
            }
            Some(&*p.cast::<T>())
        }
    }

    /// Mutably borrow the object `v` points at. See [`Heap::get`], including its obligation.
    pub fn get_mut<T: HeapObject>(&mut self, v: Value) -> Option<&mut T> {
        let p = v.header_ptr()?;
        debug_assert!(self.live_set.is_live(p), "{STALE_VALUE}");
        // SAFETY: as `get`, and the `&mut self` borrow makes this the only live reference
        // into the heap for as long as the result is held.
        unsafe {
            if (*p).tag != T::TAG {
                return None;
            }
            Some(&mut *p.cast::<T>())
        }
    }

    /// The concrete type of the object `v` points at.
    ///
    /// Carries the same liveness obligation as [`Heap::get`].
    pub fn tag_of(&self, v: Value) -> Option<HeapTag> {
        let p = v.header_ptr()?;
        debug_assert!(self.live_set.is_live(p), "{STALE_VALUE}");
        // SAFETY: `p` points at a live object of this heap; only the header is read.
        Some(unsafe { (*p).tag })
    }

    /// Whether `v` points at an object this heap currently owns.
    ///
    /// Walks the all-objects list, so it is O(live). It exists to let tests assert the
    /// invariant the rest of the API assumes.
    pub fn contains(&self, v: Value) -> bool {
        let Some(target) = v.header_ptr() else {
            return false;
        };
        let mut cur = self.all;
        while !cur.is_null() {
            if std::ptr::eq(cur, target) {
                return true;
            }
            // SAFETY: `cur` is a node of this heap's own list, so its header is readable.
            cur = unsafe { (*cur).next };
        }
        false
    }

    /// The value of an exact integer, if it fits in an `i64`.
    ///
    /// Carries [`Heap::get`]'s liveness obligation for bignums.
    pub fn integer_to_i64(&self, v: Value) -> Option<i64> {
        match v.as_fixnum() {
            Some(n) => Some(n),
            None => self.get::<Bignum>(v).and_then(|b| b.value.to_i64()),
        }
    }

    // ---------------------------------------------------------------- barrier

    /// Write barrier: record that `container` now refers to `new`.
    ///
    /// A no-op today, because a stop-the-world collector re-marks from the roots every time
    /// and cannot observe a mutation mid-mark. It exists so that `SETCAR`, `SETCDR`,
    /// `VECSET`, `SETUPVAL` and `SETGLOBAL` are written with the call in place from their
    /// first line: an incremental or generational collector needs a barrier at exactly those
    /// five sites, and retrofitting them is how collectors acquire their subtlest bugs.
    #[inline]
    pub fn wb(&mut self, container: Value, new: Value) {
        let _ = (container, new);
    }

    // ---------------------------------------------------------------- collection

    /// Values pinned through the returned stack are roots. See [`handle`].
    pub fn pins(&self) -> Rc<PinStack> {
        Rc::clone(&self.pins)
    }

    /// Whether the heap has grown enough to be worth collecting.
    pub fn should_collect(&self) -> bool {
        self.bytes >= self.next_gc
    }

    /// Objects currently on the heap.
    pub fn live_objects(&self) -> usize {
        self.live
    }

    /// Tracked bytes: object sizes plus the Rust-side storage they own.
    pub fn bytes_allocated(&self) -> usize {
        self.bytes
    }

    /// How many collections have run.
    pub fn collections(&self) -> usize {
        self.collections
    }

    /// Collect, treating `roots` — plus the interner and the pin stack — as the live set.
    ///
    /// Anything not reachable from those is freed, `Drop` and all.
    ///
    /// # This invalidates values
    ///
    /// Every [`Value`] that `roots` and the pin stack do not reach is dangling afterwards,
    /// in the same way `Vec::clear` invalidates outstanding indices — except that a `Value`
    /// looks exactly as usable as it did before. Callers must treat a collection as a
    /// safepoint: at the moment it runs, the root set must be *complete*. Whether that
    /// obligation should be enforced by making this function `unsafe` rather than the
    /// accessors is an open question recorded in `docs/project_plan.org`; today it is
    /// checked by a `debug_assert!` at the point of use (see [`Heap::get`]).
    ///
    /// # Shaping the root set
    ///
    /// The aggregate passed here must not be the struct that owns the heap: `Heap::collect`
    /// takes `&mut self`, so `self.heap.collect(&*self)` cannot borrow-check for any API of
    /// this shape. Split the owner instead —
    ///
    /// ```ignore
    /// struct Vm { heap: Heap, state: VmState }   // registers, globals, frames, wind stack
    /// unsafe impl Trace for VmState { /* ... */ }
    ///
    /// self.heap.collect(&self.state)             // disjoint field borrows
    /// ```
    ///
    /// so that every safepoint reads the same way and a new kind of root is added in exactly
    /// one place.
    pub fn collect(&mut self, roots: &dyn Trace) -> GcStats {
        let live_before = self.live;
        let bytes_before = self.bytes;

        self.mark(roots);
        let freed = self.sweep();

        self.collections += 1;
        self.next_gc = self
            .bytes
            .saturating_mul(HEAP_GROWTH_FACTOR)
            .max(MIN_HEAP_BYTES);

        GcStats {
            live_before,
            live_after: self.live,
            freed,
            bytes_before,
            bytes_after: self.bytes,
        }
    }

    fn mark(&mut self, roots: &dyn Trace) {
        let Self {
            gray,
            symbols,
            pins,
            live_set,
            ..
        } = self;

        gray.clear();
        let mut tracer = Tracer::new(gray, live_set);
        roots.trace(&mut tracer);

        // Interned symbols are permanent roots. `eq?` on symbols is pointer identity, so a
        // symbol that was collected and later re-interned would come back at a different
        // address and stop being `eq?` to the copies already sitting in constant pools.
        for &sym in symbols.values() {
            // SAFETY: the interner only ever holds symbols this heap allocated, and it is
            // marked here on every collection, so none of them has been freed.
            unsafe { tracer.mark_header(sym.cast::<GcHeader>()) };
        }

        // Native-procedure temporaries, which live in Rust locals no root can see.
        pins.trace_pins(|v| tracer.mark(v));

        // Drain: grey means reached but unscanned, so the worklist is empty exactly when
        // everything reachable is black.
        while let Some(p) = self.gray.pop() {
            // SAFETY: every pointer on the worklist was put there by `Tracer::mark` from a
            // live `Value`, so it points at an object this heap owns.
            unsafe { (*p).color = Color::Black };
            let mut tracer = Tracer::new(&mut self.gray, &self.live_set);
            // SAFETY: as above; the object's tag is the one its allocation wrote.
            unsafe { trace_object(p, &mut tracer) };
        }
    }

    /// Free every white object, rebuilding the all-objects list from the survivors.
    ///
    /// Rebuilding rather than unlinking in place keeps the loop free of pointer-to-pointer
    /// bookkeeping; the list order reverses each collection, which nothing depends on.
    ///
    /// The byte count is recomputed from the survivors rather than decremented per free.
    /// Decrementing reads an object's size at free time but added it at allocation time, so
    /// any object whose owned buffer grew in between would over-subtract — and a counter
    /// that drifts downwards eventually stops [`Heap::should_collect`] firing at all. The
    /// walk is happening anyway; summing during it costs one call per survivor and makes
    /// the number exact after every collection.
    fn sweep(&mut self) -> usize {
        let mut survivors: *mut GcHeader = std::ptr::null_mut();
        let mut cur = self.all;
        let mut freed = 0;
        let mut live_bytes = 0;

        while !cur.is_null() {
            // SAFETY: `cur` is a node of this heap's own list.
            let next = unsafe { (*cur).next };
            // SAFETY: as above.
            if unsafe { (*cur).color } == Color::White {
                // SAFETY: white means unreachable, so no live value points here, and the
                // node has been unlinked from the list being rebuilt.
                unsafe { drop_object(cur) };
                self.live_set.remove(cur);
                self.live -= 1;
                freed += 1;
            } else {
                // SAFETY: as above.
                live_bytes += unsafe { object_bytes(cur) };
                // SAFETY: as above.
                unsafe {
                    (*cur).color = Color::White;
                    (*cur).next = survivors;
                }
                survivors = cur;
            }
            cur = next;
        }

        self.all = survivors;
        self.bytes = live_bytes;
        freed
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        let mut cur = self.all;
        while !cur.is_null() {
            // SAFETY: `cur` is a node of this heap's own list, so its header is readable.
            let next = unsafe { (*cur).next };
            // SAFETY: the heap is going away, so every object on the list is unreachable and
            // nothing can observe it afterwards. The interner's raw pointers are dropped
            // without being dereferenced.
            unsafe { drop_object(cur) };
            self.live_set.remove(cur);
            cur = next;
        }
        self.all = std::ptr::null_mut();
    }
}

/// Grey everything `p` refers to.
///
/// # Safety
///
/// `p` must point at a live object allocated by the heap being collected, whose header tag
/// is the tag its allocation wrote.
unsafe fn trace_object(p: *mut GcHeader, tracer: &mut Tracer<'_>) {
    // SAFETY: the caller guarantees `p` is a live object, so its header is readable.
    let tag = unsafe { (*p).tag };
    // SAFETY: in every arm, the tag identifies the concrete type, and `HeapObject` guarantees
    // that type begins with its header at offset 0, so the cast recovers the original type.
    unsafe {
        match tag {
            HeapTag::Pair => (*p.cast::<Pair>()).trace_fields(tracer),
            HeapTag::Str => (*p.cast::<Str>()).trace_fields(tracer),
            HeapTag::Symbol => (*p.cast::<Symbol>()).trace_fields(tracer),
            HeapTag::Vector => (*p.cast::<Vector>()).trace_fields(tracer),
            HeapTag::Bytevector => (*p.cast::<Bytevector>()).trace_fields(tracer),
            HeapTag::UpvalueCell => (*p.cast::<UpvalueCell>()).trace_fields(tracer),
            HeapTag::Bignum => (*p.cast::<Bignum>()).trace_fields(tracer),
            HeapTag::Record => (*p.cast::<Record>()).trace_fields(tracer),
            HeapTag::RecordType => (*p.cast::<RecordType>()).trace_fields(tracer),
            // No allocator exists for closures until `Proto` lands in M2, so no closure can
            // be on the heap to reach.
            HeapTag::Closure => debug_assert!(false, "closures are not allocatable until M2"),
        }
    }
}

/// Run `Drop` on the object at `p` and release its allocation.
///
/// The `Drop` is the point: it is what frees the `String`, `Vec` and `BigInt` buffers heap
/// objects own on the Rust side. Deallocating the header alone would leak all of them, and
/// nothing in the heap's own accounting would notice — which is what `tests/gc_drop.rs`
/// exists to catch.
///
/// # Safety
///
/// `p` must point at a live object allocated by this heap, whose header tag is the tag its
/// allocation wrote, and it must be unreachable — this frees it.
unsafe fn drop_object(p: *mut GcHeader) {
    // SAFETY: the caller guarantees `p` is a live object, so its header is readable.
    let tag = unsafe { (*p).tag };
    // SAFETY: in every arm, as `trace_object`, plus the caller's guarantee that the object
    // is unreachable, which makes reconstructing and dropping its `Box` sound.
    unsafe {
        match tag {
            HeapTag::Pair => drop_as::<Pair>(p),
            HeapTag::Str => drop_as::<Str>(p),
            HeapTag::Symbol => drop_as::<Symbol>(p),
            HeapTag::Vector => drop_as::<Vector>(p),
            HeapTag::Bytevector => drop_as::<Bytevector>(p),
            HeapTag::UpvalueCell => drop_as::<UpvalueCell>(p),
            HeapTag::Bignum => drop_as::<Bignum>(p),
            HeapTag::Record => drop_as::<Record>(p),
            HeapTag::RecordType => drop_as::<RecordType>(p),
            HeapTag::Closure => debug_assert!(false, "closures are not allocatable until M2"),
        }
    }
}

/// # Safety
///
/// `p` must point at a live, unreachable object of type `T` allocated by this heap.
unsafe fn drop_as<T: HeapObject>(p: *mut GcHeader) {
    // SAFETY: the caller guarantees the object is a `T` produced by `Box::into_raw` in
    // `Heap::alloc_raw`, and that nothing else refers to it.
    drop(unsafe { Box::<T>::from_raw(p.cast::<T>()) });
}

/// The object's own size plus the Rust-side storage it owns, read live rather than
/// remembered from allocation time.
///
/// # Safety
///
/// `p` must point at a live object allocated by this heap, whose header tag is the tag its
/// allocation wrote.
unsafe fn object_bytes(p: *mut GcHeader) -> usize {
    // SAFETY: the caller guarantees `p` is a live object, so its header is readable.
    let tag = unsafe { (*p).tag };
    // SAFETY: in every arm, as `trace_object`.
    unsafe {
        match tag {
            HeapTag::Pair => size_as::<Pair>(p),
            HeapTag::Str => size_as::<Str>(p),
            HeapTag::Symbol => size_as::<Symbol>(p),
            HeapTag::Vector => size_as::<Vector>(p),
            HeapTag::Bytevector => size_as::<Bytevector>(p),
            HeapTag::UpvalueCell => size_as::<UpvalueCell>(p),
            HeapTag::Bignum => size_as::<Bignum>(p),
            HeapTag::Record => size_as::<Record>(p),
            HeapTag::RecordType => size_as::<RecordType>(p),
            HeapTag::Closure => {
                debug_assert!(false, "closures are not allocatable until M2");
                0
            }
        }
    }
}

/// # Safety
///
/// `p` must point at a live object of type `T` allocated by this heap.
unsafe fn size_as<T: HeapObject>(p: *mut GcHeader) -> usize {
    // SAFETY: the caller guarantees the object is a live `T` whose header is at offset 0.
    size_of::<T>() + unsafe { (*p.cast::<T>()).extra_bytes() }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::{FIXNUM_MAX, FIXNUM_MIN};

    /// A root set built from a plain vector of values, which is the shape the VM's register
    /// file will have.
    struct Roots(Vec<Value>);

    // SAFETY: reports every value it holds, and holds nothing else.
    unsafe impl Trace for Roots {
        fn trace(&self, tracer: &mut Tracer<'_>) {
            self.0.trace(tracer);
        }
    }

    #[test]
    fn allocation_round_trips_through_the_tag() {
        let mut heap = Heap::new();
        let p = heap.cons(Value::fixnum(1).unwrap(), Value::NIL);

        assert!(p.is_heap());
        assert!(heap.contains(p));
        assert_eq!(heap.tag_of(p), Some(HeapTag::Pair));
        assert_eq!(heap.live_objects(), 1);

        let pair = heap.get::<Pair>(p).unwrap();
        assert_eq!(pair.car.as_fixnum(), Some(1));
        assert!(pair.cdr.is_null());

        // A tag mismatch is a miss, not a transmute.
        assert!(heap.get::<Str>(p).is_none());
        assert!(heap.get::<Vector>(p).is_none());
        // Immediates point at nothing.
        assert!(heap.get::<Pair>(Value::NIL).is_none());
        assert!(!heap.contains(Value::fixnum(1).unwrap()));
    }

    #[test]
    fn every_object_kind_allocates_and_reads_back() {
        let mut heap = Heap::new();

        let s = heap.string("hello");
        assert_eq!(heap.get::<Str>(s).unwrap().chars, "hello");

        let v = heap.vector(vec![Value::TRUE, Value::FALSE]);
        assert_eq!(heap.get::<Vector>(v).unwrap().elems.len(), 2);

        let bv = heap.bytevector(vec![0, 255]);
        assert_eq!(heap.get::<Bytevector>(bv).unwrap().bytes, vec![0, 255]);

        let cell = heap.upvalue_cell(Value::char('x'));
        assert_eq!(
            heap.get::<UpvalueCell>(cell).unwrap().value.as_char(),
            Some('x')
        );

        let name = heap.symbol("point");
        let fields = vec![heap.symbol("x"), heap.symbol("y")];
        let rt = heap.record_type(name, fields);
        assert_eq!(heap.get::<RecordType>(rt).unwrap().field_names.len(), 2);

        let r = heap.record(
            rt,
            vec![Value::fixnum(3).unwrap(), Value::fixnum(4).unwrap()],
        );
        assert_eq!(heap.get::<Record>(r).unwrap().rtype, rt);

        let big = heap.integer(FIXNUM_MAX + 1);
        assert_eq!(heap.tag_of(big), Some(HeapTag::Bignum));
    }

    #[test]
    fn mutation_goes_through_get_mut() {
        let mut heap = Heap::new();
        let p = heap.cons(Value::NIL, Value::NIL);
        let s = heap.string("payload");

        heap.get_mut::<Pair>(p).unwrap().car = s;
        heap.wb(p, s);

        let car = heap.get::<Pair>(p).unwrap().car;
        assert_eq!(heap.get::<Str>(car).unwrap().chars, "payload");
    }

    #[test]
    fn unreachable_objects_are_freed_and_reachable_ones_survive() {
        let mut heap = Heap::new();

        let kept = heap.cons(Value::fixnum(1).unwrap(), Value::NIL);
        let kept_str = heap.string("kept");
        heap.get_mut::<Pair>(kept).unwrap().cdr = kept_str;

        // Three objects nothing will report.
        heap.string("dropped");
        heap.vector(vec![Value::TRUE]);
        heap.cons(Value::NIL, Value::NIL);

        assert_eq!(heap.live_objects(), 5);

        let stats = heap.collect(&Roots(vec![kept]));

        assert_eq!(stats.freed, 3);
        assert_eq!(stats.live_before, 5);
        assert_eq!(stats.live_after, 2);
        assert_eq!(heap.live_objects(), 2);
        assert_eq!(heap.collections(), 1);

        // The survivor is intact, transitively.
        assert!(heap.contains(kept));
        assert!(heap.contains(kept_str));
        let cdr = heap.get::<Pair>(kept).unwrap().cdr;
        assert_eq!(heap.get::<Str>(cdr).unwrap().chars, "kept");
    }

    /// A reference cycle is exactly what a reference count cannot free, and it is why
    /// decision B rules refcounting out: `call/cc` and mutable pairs make cycles ordinary.
    #[test]
    fn cycles_are_collected() {
        let mut heap = Heap::new();

        let a = heap.cons(Value::NIL, Value::NIL);
        let b = heap.cons(a, Value::NIL);
        heap.get_mut::<Pair>(a).unwrap().cdr = b;

        // A pair that is its own cdr, too.
        let selfish = heap.cons(Value::NIL, Value::NIL);
        heap.get_mut::<Pair>(selfish).unwrap().cdr = selfish;

        assert_eq!(heap.live_objects(), 3);
        let stats = heap.collect(&());
        assert_eq!(stats.freed, 3);
        assert_eq!(heap.live_objects(), 0);
    }

    /// Marking must leave the heap in the state the *next* collection expects: survivors
    /// back to white. A collector that forgets this frees nothing ever again.
    #[test]
    fn consecutive_collections_each_free_their_own_garbage() {
        let mut heap = Heap::new();
        let kept = heap.cons(Value::TRUE, Value::NIL);

        heap.string("first round");
        assert_eq!(heap.collect(&Roots(vec![kept])).freed, 1);

        heap.string("second round");
        heap.string("also second round");
        assert_eq!(heap.collect(&Roots(vec![kept])).freed, 2);

        assert_eq!(heap.live_objects(), 1);
        assert!(heap.contains(kept));
        assert_eq!(heap.collections(), 2);
    }

    #[test]
    fn interned_symbols_are_eq_and_outlive_every_collection() {
        let mut heap = Heap::new();

        let a = heap.symbol("lambda");
        let b = heap.symbol("lambda");
        let c = heap.symbol("define");

        // `eq?` on symbols is pointer identity, which is what the compiler dispatches on.
        assert_eq!(a, b);
        assert_ne!(a, c);
        assert_eq!(heap.live_objects(), 2);

        // Nothing roots them, and they survive anyway: the interner is a root.
        let stats = heap.collect(&());
        assert_eq!(stats.freed, 0);
        assert_eq!(heap.symbol("lambda"), a);
        assert_eq!(heap.get::<Symbol>(a).unwrap().name.as_ref(), "lambda");
        assert!(heap.get::<Symbol>(a).unwrap().interned);
    }

    #[test]
    fn uninterned_symbols_are_distinct_and_collectable() {
        let mut heap = Heap::new();

        let g1 = heap.uninterned_symbol("g");
        let g2 = heap.uninterned_symbol("g");
        let interned = heap.symbol("g");

        assert_ne!(g1, g2);
        assert_ne!(g1, interned);
        assert!(!heap.get::<Symbol>(g1).unwrap().interned);

        let stats = heap.collect(&Roots(vec![g1]));
        assert_eq!(stats.freed, 1); // g2 goes; g1 is rooted, `interned` is in the table
        assert!(heap.contains(g1));
        assert!(heap.contains(interned));
    }

    #[test]
    fn pins_keep_native_temporaries_alive_across_an_allocation() {
        let mut heap = Heap::new();
        let pins = heap.pins();

        let survived = {
            let scope = pins.scope();
            let first = scope.pin(heap.cons(Value::TRUE, Value::NIL));
            heap.string("unrooted");

            // Nothing but the pin stack knows about `first`.
            let stats = heap.collect(&());
            assert_eq!(stats.freed, 1);
            assert!(heap.contains(first.get()));

            // A pin is a slot, so a native procedure can keep an accumulator rooted.
            first.set(heap.cons(Value::FALSE, first.get()));
            heap.collect(&());
            first.get()
        };

        // The scope has closed, so the pin is released.
        assert!(pins.is_empty());
        assert_eq!(heap.collect(&()).freed, 2);
        assert!(!heap.contains(survived));
    }

    #[test]
    fn nested_scopes_release_only_their_own_pins() {
        let mut heap = Heap::new();
        let pins = heap.pins();

        let outer = pins.scope();
        let kept = outer.pin(heap.cons(Value::TRUE, Value::NIL));
        {
            let inner = pins.scope();
            inner.pin(heap.cons(Value::FALSE, Value::NIL));
            assert_eq!(pins.len(), 2);
            assert_eq!(heap.collect(&()).freed, 0);
        }
        assert_eq!(pins.len(), 1);

        // The inner scope's pin is gone; the outer one still holds.
        assert_eq!(heap.collect(&()).freed, 1);
        assert!(heap.contains(kept.get()));
    }

    /// The reason a pin is a slot and not a position on a stack.
    ///
    /// An accumulator rooted in the enclosing scope while an inner scope churns through
    /// temporaries is the whole point of nesting. Under a shadow stack that truncated to a
    /// remembered length, `accumulator` would land *above* the inner scope's mark, the
    /// inner scope's close would silently unroot it, and the next two pins would reuse its
    /// index — leaving `kept` reading a stranger's value.
    #[test]
    fn a_pin_from_an_enclosing_scope_survives_an_inner_scope_closing() {
        let mut heap = Heap::new();
        let pins = heap.pins();

        let outer = pins.scope();
        let first = outer.pin(heap.string("first"));

        let kept;
        {
            let inner = pins.scope();
            inner.pin(heap.string("temporary"));
            kept = outer.pin(heap.string("accumulator"));
            assert_eq!(pins.len(), 3);
        }
        assert_eq!(pins.len(), 2);

        // Refill the slot the inner scope released.
        let refill = outer.pin(heap.string("refill"));

        assert_eq!(
            heap.collect(&()).freed,
            1,
            "only the temporary is unreachable"
        );
        assert_eq!(heap.get::<Str>(kept.get()).unwrap().chars, "accumulator");
        assert_eq!(heap.get::<Str>(first.get()).unwrap().chars, "first");
        assert_eq!(heap.get::<Str>(refill.get()).unwrap().chars, "refill");
    }

    #[test]
    fn pin_scopes_may_be_dropped_in_any_order() {
        let mut heap = Heap::new();
        let pins = heap.pins();

        let outer = pins.scope();
        let inner = pins.scope();

        let outer_value = outer.pin(heap.string("outer")).get();
        let inner_pin = inner.pin(heap.string("inner"));

        // Deliberately backwards. A truncating shadow stack would take the inner scope's
        // pin down with it.
        drop(outer);

        assert_eq!(heap.collect(&()).freed, 1);
        assert!(!heap.contains(outer_value));
        assert_eq!(heap.get::<Str>(inner_pin.get()).unwrap().chars, "inner");

        drop(inner);
        assert!(pins.is_empty());
        assert_eq!(heap.collect(&()).freed, 1);
    }

    /// Leaking a scope must fail in the safe direction: values stay rooted forever, which
    /// wastes memory. The unsafe direction — silently releasing them — is what a truncating
    /// stack does, because the truncation lives in the `Drop` that never runs.
    #[test]
    #[cfg_attr(
        miri,
        ignore = "deliberately leaks a scope, which Miri's leak checker correctly reports"
    )]
    fn forgetting_a_scope_leaks_its_pins_rather_than_releasing_them() {
        let mut heap = Heap::new();
        let pins = heap.pins();

        let leaked = {
            let scope = pins.scope();
            let value = scope.pin(heap.string("leaked")).get();
            core::mem::forget(scope);
            value
        };

        assert_eq!(pins.len(), 1);
        assert_eq!(heap.collect(&()).freed, 0);
        assert_eq!(heap.get::<Str>(leaked).unwrap().chars, "leaked");
    }

    /// A `Value` is `Copy` and carries no lifetime, so one held across a collection that did
    /// not reach it is dangling and nothing in the type system says so. In release this is
    /// a use-after-free — and usually a silent one, since the next allocation reuses the
    /// block. Debug builds trade that for a loud failure at the point of use.
    #[cfg(debug_assertions)]
    #[test]
    #[should_panic(expected = "this heap does not own")]
    fn a_value_stale_after_a_collection_trips_the_debug_liveness_check() {
        let mut heap = Heap::new();
        let stale = heap.cons(Value::TRUE, Value::NIL);
        assert_eq!(heap.collect(&()).freed, 1);
        let _ = heap.tag_of(stale);
    }

    /// A recursive mark would recurse once per cons cell and overflow the stack somewhere
    /// around a hundred thousand of them. The grey worklist is what makes this survivable.
    #[test]
    fn a_very_long_list_marks_without_recursing() {
        // Miri interprets every allocation, so the full-size list would take hours there.
        // A few thousand cells still drives the worklist through the same drain path, which
        // is what Miri is here to check; the stack-depth claim is what the native-speed run
        // establishes.
        const CELLS: usize = if cfg!(miri) { 2_000 } else { 200_000 };

        let mut heap = Heap::new();
        let mut head = Value::NIL;
        for i in 0..CELLS {
            head = heap.cons(Value::fixnum(i as i64).unwrap(), head);
        }
        assert_eq!(heap.live_objects(), CELLS);

        let stats = heap.collect(&head);
        assert_eq!(stats.freed, 0);
        assert_eq!(heap.live_objects(), CELLS);

        // And the whole chain is still walkable.
        let mut n = 0;
        let mut cur = head;
        while let Some(pair) = heap.get::<Pair>(cur) {
            n += 1;
            cur = pair.cdr;
        }
        assert_eq!(n, CELLS);

        assert_eq!(heap.collect(&()).freed, CELLS);
    }

    #[test]
    fn integers_promote_and_demote_at_the_fixnum_boundary() {
        let mut heap = Heap::new();

        for n in [0, 1, -1, FIXNUM_MAX, FIXNUM_MIN] {
            let v = heap.integer(n);
            assert!(v.is_fixnum(), "{n} should be immediate");
            assert_eq!(heap.integer_to_i64(v), Some(n));
        }

        for n in [FIXNUM_MAX + 1, FIXNUM_MIN - 1, i64::MAX, i64::MIN] {
            let v = heap.integer(n);
            assert!(v.is_heap(), "{n} should be a bignum");
            assert_eq!(heap.tag_of(v), Some(HeapTag::Bignum));
            assert_eq!(heap.integer_to_i64(v), Some(n));
        }

        // A bignum that fits comes back as a fixnum, so the fast paths keep firing.
        let demoted = heap.integer_from_big(BigInt::from(7));
        assert!(demoted.is_fixnum());
        assert_eq!(demoted.as_fixnum(), Some(7));

        let huge: BigInt = BigInt::from(1u8) << 200;
        let promoted = heap.integer_from_big(huge.clone());
        assert!(promoted.is_heap());
        assert_eq!(heap.get::<Bignum>(promoted).unwrap().value, huge);
        assert_eq!(heap.integer_to_i64(promoted), None);
    }

    #[test]
    fn accounting_grows_on_allocation_and_shrinks_on_sweep() {
        let mut heap = Heap::new();
        assert_eq!(heap.bytes_allocated(), 0);
        assert!(!heap.should_collect());

        heap.string("x".repeat(4096));
        let peak = heap.bytes_allocated();
        assert!(peak >= 4096, "string storage should be counted, got {peak}");

        heap.collect(&());
        assert_eq!(heap.bytes_allocated(), 0);
        assert_eq!(heap.live_objects(), 0);
    }

    #[test]
    fn record_fields_are_traced() {
        let mut heap = Heap::new();

        let name = heap.symbol("kons");
        let rt = heap.record_type(name, vec![]);
        let payload = heap.string("in a field");
        let r = heap.record(rt, vec![payload]);

        // Only the record is rooted; the type and the field must be reached through it.
        let stats = heap.collect(&Roots(vec![r]));
        assert_eq!(stats.freed, 0);
        assert!(heap.contains(rt));
        assert!(heap.contains(payload));

        assert_eq!(heap.collect(&()).freed, 3);
    }

    #[test]
    fn vectors_and_cells_are_traced() {
        let mut heap = Heap::new();

        let inner = heap.string("reached through a vector");
        let vec_val = heap.vector(vec![Value::NIL, inner]);
        let cell = heap.upvalue_cell(vec_val);

        assert_eq!(heap.collect(&Roots(vec![cell])).freed, 0);
        assert!(heap.contains(inner));

        assert_eq!(heap.collect(&()).freed, 3);
    }
}

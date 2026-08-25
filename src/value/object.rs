//! The concrete heap objects.
//!
//! Every one of them is `#[repr(C)]` with its [`GcHeader`] first, which is what lets the
//! collector read a tag off an untyped pointer and recover the real type from it. The
//! offsets are asserted at compile time in [`crate::value::layout`].
//!
//! Objects are never constructed directly: [`Heap`](crate::gc::Heap) owns their lifecycle,
//! because an object that is not on the heap's all-objects list is invisible to the sweep.

// Declarations only: the `unsafe impl HeapObject` blocks below assert layout and tag
// invariants the collector relies on. This file contains no unsafe *operations*.
#![allow(unsafe_code)]

use std::rc::Rc;

use num_bigint::BigInt;

use crate::bytecode::Proto;
use crate::gc::Tracer;
use crate::value::Value;
use crate::value::layout::{GcHeader, HeapTag};

/// Write an object's header. The allocator's only route to it.
pub(crate) fn set_header<T: HeapObject>(obj: &mut T, header: GcHeader) {
    *obj.header_mut() = header;
}

/// The crate-private half of [`HeapObject`], which also seals it.
///
/// Header access lives here rather than on the public trait because the header's tag is
/// what [`Heap::get`](crate::gc::Heap::get) and the sweep dispatch on: safe outside code
/// able to write it could retag a `Pair` as a `Str` and make the sweep reconstruct the
/// wrong `Box`. Sealing matters for the same reason — `Heap::get::<T>` is generic over
/// `HeapObject`, so an outside `unsafe impl` claiming `TAG = Pair` would hand out a
/// reference of its own type over a pair's memory.
mod sealed {
    use crate::value::layout::GcHeader;

    pub trait Header {
        fn header_mut(&mut self) -> &mut GcHeader;
    }
}

use sealed::Header;

/// A type that can live on the GC heap.
///
/// Sealed: only this crate can implement it, and the header it carries is not reachable
/// from outside. Both restrictions guard the same thing — the collector decides what an
/// untyped pointer really is by reading the tag byte in that header.
///
/// The header cannot be written from outside the crate:
///
/// ```compile_fail
/// use ruse::gc::Heap;
/// use ruse::value::Value;
/// use ruse::value::object::{HeapObject, Pair};
///
/// let mut heap = Heap::new();
/// let p = heap.cons(Value::TRUE, Value::NIL);
/// heap.get_mut::<Pair>(p).unwrap().header_mut();
/// ```
///
/// and the trait cannot be implemented from outside it:
///
/// ```compile_fail
/// use ruse::gc::Tracer;
/// use ruse::value::layout::HeapTag;
/// use ruse::value::object::HeapObject;
///
/// struct Impostor(u64);
///
/// unsafe impl HeapObject for Impostor {
///     const TAG: HeapTag = HeapTag::Pair;
///     fn trace_fields(&self, _tracer: &mut Tracer<'_>) {}
/// }
/// ```
///
/// # Safety
///
/// An implementor must be `#[repr(C)]` with a [`GcHeader`] as its first field, so that a
/// `*mut Self` and a `*mut GcHeader` name the same address, and [`Self::TAG`] must be the
/// tag under which the collector will recover `Self`. A wrong tag makes the sweep reconstruct
/// one type's `Box` from another type's allocation.
pub unsafe trait HeapObject: Sized + Header {
    /// The tag written into this object's header at allocation.
    const TAG: HeapTag;

    /// Grey every [`Value`] this object holds directly.
    ///
    /// Missing one is a use-after-free: the collector's notion of "reachable" is exactly
    /// what these functions report.
    fn trace_fields(&self, tracer: &mut Tracer<'_>);

    /// Bytes this object owns on the Rust heap beyond `size_of::<Self>()`, for the
    /// collection trigger's accounting. An estimate is fine; it steers a heuristic.
    fn extra_bytes(&self) -> usize {
        0
    }
}

/// A cons cell.
#[repr(C)]
pub struct Pair {
    pub(crate) header: GcHeader,
    /// The first component.
    pub car: Value,
    /// The second component.
    pub cdr: Value,
}

impl Pair {
    pub(crate) fn new(car: Value, cdr: Value) -> Self {
        Self {
            header: GcHeader::unlinked(HeapTag::Pair),
            car,
            cdr,
        }
    }
}

impl Header for Pair {
    fn header_mut(&mut self) -> &mut GcHeader {
        &mut self.header
    }
}

// SAFETY: `#[repr(C)]` with `GcHeader` first, asserted in `layout`, and `TAG` is the tag
// the allocator writes into that header. `trace_fields` reports `car` and `cdr`, which are its only
// `Value` fields.
unsafe impl HeapObject for Pair {
    const TAG: HeapTag = HeapTag::Pair;

    fn trace_fields(&self, tracer: &mut Tracer<'_>) {
        tracer.mark(self.car);
        tracer.mark(self.cdr);
    }
}

/// A mutable Scheme string.
///
/// Stored as UTF-8, which makes `string-ref` O(n) in the index. That is the same trade
/// chibi-scheme makes and it keeps strings compact; if the conformance work in M9 shows it
/// hurting, the choice is contained behind this one type.
#[repr(C)]
pub struct Str {
    pub(crate) header: GcHeader,
    /// The characters.
    pub chars: String,
}

impl Str {
    pub(crate) fn new(chars: String) -> Self {
        Self {
            header: GcHeader::unlinked(HeapTag::Str),
            chars,
        }
    }
}

impl Header for Str {
    fn header_mut(&mut self) -> &mut GcHeader {
        &mut self.header
    }
}

// SAFETY: `#[repr(C)]` with `GcHeader` first, asserted in `layout`, and `TAG` is the tag
// the allocator writes into that header. A `Str` owns a `String` and no `Value`s, so there is nothing
// to trace.
unsafe impl HeapObject for Str {
    const TAG: HeapTag = HeapTag::Str;

    fn trace_fields(&self, _tracer: &mut Tracer<'_>) {}

    fn extra_bytes(&self) -> usize {
        self.chars.capacity()
    }
}

/// A symbol.
///
/// Interned symbols are the ones the reader produces, and `eq?` on them is pointer
/// identity. Uninterned symbols (the generated names hygiene will need in M8) carry the same
/// representation but are absent from the interner, so they are `eq?` only to themselves.
#[repr(C)]
pub struct Symbol {
    pub(crate) header: GcHeader,
    /// The symbol's name, shared with the interner's key when interned.
    pub name: Rc<str>,
    /// Whether the interner holds this symbol.
    pub interned: bool,
}

impl Symbol {
    pub(crate) fn new(name: Rc<str>, interned: bool) -> Self {
        Self {
            header: GcHeader::unlinked(HeapTag::Symbol),
            name,
            interned,
        }
    }
}

impl Header for Symbol {
    fn header_mut(&mut self) -> &mut GcHeader {
        &mut self.header
    }
}

// SAFETY: `#[repr(C)]` with `GcHeader` first, asserted in `layout`, and `TAG` is the tag
// the allocator writes into that header. A `Symbol` owns its name and no `Value`s, so there is nothing
// to trace.
unsafe impl HeapObject for Symbol {
    const TAG: HeapTag = HeapTag::Symbol;

    fn trace_fields(&self, _tracer: &mut Tracer<'_>) {}

    fn extra_bytes(&self) -> usize {
        self.name.len()
    }
}

/// A mutable vector of values.
#[repr(C)]
pub struct Vector {
    pub(crate) header: GcHeader,
    /// The elements.
    pub elems: Vec<Value>,
}

impl Vector {
    pub(crate) fn new(elems: Vec<Value>) -> Self {
        Self {
            header: GcHeader::unlinked(HeapTag::Vector),
            elems,
        }
    }
}

impl Header for Vector {
    fn header_mut(&mut self) -> &mut GcHeader {
        &mut self.header
    }
}

// SAFETY: `#[repr(C)]` with `GcHeader` first, asserted in `layout`, and `TAG` is the tag
// the allocator writes into that header. `trace_fields` reports every element of `elems`, its only
// `Value` field.
unsafe impl HeapObject for Vector {
    const TAG: HeapTag = HeapTag::Vector;

    fn trace_fields(&self, tracer: &mut Tracer<'_>) {
        for &v in &self.elems {
            tracer.mark(v);
        }
    }

    fn extra_bytes(&self) -> usize {
        self.elems.capacity() * size_of::<Value>()
    }
}

/// A mutable vector of octets.
#[repr(C)]
pub struct Bytevector {
    pub(crate) header: GcHeader,
    /// The octets.
    pub bytes: Vec<u8>,
}

impl Bytevector {
    pub(crate) fn new(bytes: Vec<u8>) -> Self {
        Self {
            header: GcHeader::unlinked(HeapTag::Bytevector),
            bytes,
        }
    }
}

impl Header for Bytevector {
    fn header_mut(&mut self) -> &mut GcHeader {
        &mut self.header
    }
}

// SAFETY: `#[repr(C)]` with `GcHeader` first, asserted in `layout`, and `TAG` is the tag
// the allocator writes into that header. A `Bytevector` owns octets and no `Value`s, so there is nothing
// to trace.
unsafe impl HeapObject for Bytevector {
    const TAG: HeapTag = HeapTag::Bytevector;

    fn trace_fields(&self, _tracer: &mut Tracer<'_>) {}

    fn extra_bytes(&self) -> usize {
        self.bytes.capacity()
    }
}

/// A closure: a compiled prototype plus the captured variables it runs over.
///
/// The prototype is shared by `Rc` — every closure over one `lambda` executes the same
/// immutable [`Proto`] — and it is not itself a heap object, so the closure is what makes
/// the prototype's constants reachable: `trace_fields` walks the whole prototype tree.
///
/// `upvals` holds one value per entry in the prototype's descriptor table: the closure's
/// view of each captured binding. In the settled M4 design these are [`UpvalueCell`]s
/// (shared, so `set!` through one closure is visible through another); the open/closed
/// upvalue mechanics live in the VM, not here.
#[repr(C)]
pub struct Closure {
    pub(crate) header: GcHeader,
    /// The compiled function this closure executes.
    pub proto: Rc<Proto>,
    /// The captured bindings, in the prototype's upvalue order.
    pub upvals: Vec<Value>,
}

impl Closure {
    pub(crate) fn new(proto: Rc<Proto>, upvals: Vec<Value>) -> Self {
        Self {
            header: GcHeader::unlinked(HeapTag::Closure),
            proto,
            upvals,
        }
    }
}

impl Header for Closure {
    fn header_mut(&mut self) -> &mut GcHeader {
        &mut self.header
    }
}

// SAFETY: `#[repr(C)]` with `GcHeader` first, asserted in `layout`, and `TAG` is the tag
// the allocator writes into that header. `trace_fields` reports every captured value and every
// constant in the prototype tree, which together are all the `Value`s reachable from a closure.
unsafe impl HeapObject for Closure {
    const TAG: HeapTag = HeapTag::Closure;

    fn trace_fields(&self, tracer: &mut Tracer<'_>) {
        for &v in &self.upvals {
            tracer.mark(v);
        }
        self.proto.trace_values(tracer);
    }

    fn extra_bytes(&self) -> usize {
        // Only what this closure uniquely owns. The prototype is shared, so charging its
        // size to every closure over it would inflate the collection trigger; the price
        // is that a sole-owner closure under-reports, which only delays a collection.
        self.upvals.capacity() * size_of::<Value>()
    }
}

/// The shared box a captured variable lives in.
///
/// A closed upvalue holds the variable itself; several closures over the same binding share
/// one cell, which is what makes `set!` on a captured variable visible to all of them.
#[repr(C)]
pub struct UpvalueCell {
    pub(crate) header: GcHeader,
    /// The captured binding.
    pub value: Value,
}

impl UpvalueCell {
    pub(crate) fn new(value: Value) -> Self {
        Self {
            header: GcHeader::unlinked(HeapTag::UpvalueCell),
            value,
        }
    }
}

impl Header for UpvalueCell {
    fn header_mut(&mut self) -> &mut GcHeader {
        &mut self.header
    }
}

// SAFETY: `#[repr(C)]` with `GcHeader` first, asserted in `layout`, and `TAG` is the tag
// the allocator writes into that header. `trace_fields` reports `value`, its only `Value` field.
unsafe impl HeapObject for UpvalueCell {
    const TAG: HeapTag = HeapTag::UpvalueCell;

    fn trace_fields(&self, tracer: &mut Tracer<'_>) {
        tracer.mark(self.value);
    }
}

/// An exact integer too large for a fixnum.
///
/// Only ever reached through [`Heap::integer`](crate::gc::Heap::integer) and
/// [`Heap::integer_from_big`](crate::gc::Heap::integer_from_big), which is what keeps the
/// fixnum/bignum boundary in one place.
#[repr(C)]
pub struct Bignum {
    pub(crate) header: GcHeader,
    /// The integer.
    pub value: BigInt,
}

impl Bignum {
    pub(crate) fn new(value: BigInt) -> Self {
        Self {
            header: GcHeader::unlinked(HeapTag::Bignum),
            value,
        }
    }
}

impl Header for Bignum {
    fn header_mut(&mut self) -> &mut GcHeader {
        &mut self.header
    }
}

// SAFETY: `#[repr(C)]` with `GcHeader` first, asserted in `layout`, and `TAG` is the tag
// the allocator writes into that header. A `Bignum` owns a `BigInt` and no `Value`s, so there is nothing
// to trace.
unsafe impl HeapObject for Bignum {
    const TAG: HeapTag = HeapTag::Bignum;

    fn trace_fields(&self, _tracer: &mut Tracer<'_>) {}

    fn extra_bytes(&self) -> usize {
        (self.value.bits() as usize).div_ceil(8)
    }
}

/// An instance of a `define-record-type` type.
#[repr(C)]
pub struct Record {
    pub(crate) header: GcHeader,
    /// The [`RecordType`] this instance belongs to.
    pub rtype: Value,
    /// Field values, positionally matching the type's field names.
    pub fields: Vec<Value>,
}

impl Record {
    pub(crate) fn new(rtype: Value, fields: Vec<Value>) -> Self {
        Self {
            header: GcHeader::unlinked(HeapTag::Record),
            rtype,
            fields,
        }
    }
}

impl Header for Record {
    fn header_mut(&mut self) -> &mut GcHeader {
        &mut self.header
    }
}

// SAFETY: `#[repr(C)]` with `GcHeader` first, asserted in `layout`, and `TAG` is the tag
// the allocator writes into that header. `trace_fields` reports `rtype` and every element of `fields`,
// which together are all of its `Value` fields.
unsafe impl HeapObject for Record {
    const TAG: HeapTag = HeapTag::Record;

    fn trace_fields(&self, tracer: &mut Tracer<'_>) {
        tracer.mark(self.rtype);
        for &v in &self.fields {
            tracer.mark(v);
        }
    }

    fn extra_bytes(&self) -> usize {
        self.fields.capacity() * size_of::<Value>()
    }
}

/// The descriptor a [`Record`] points at: what `define-record-type` creates.
#[repr(C)]
pub struct RecordType {
    pub(crate) header: GcHeader,
    /// The type's name, as a symbol.
    pub name: Value,
    /// Field names, as symbols, in field order.
    pub field_names: Vec<Value>,
}

impl RecordType {
    pub(crate) fn new(name: Value, field_names: Vec<Value>) -> Self {
        Self {
            header: GcHeader::unlinked(HeapTag::RecordType),
            name,
            field_names,
        }
    }
}

impl Header for RecordType {
    fn header_mut(&mut self) -> &mut GcHeader {
        &mut self.header
    }
}

// SAFETY: `#[repr(C)]` with `GcHeader` first, asserted in `layout`, and `TAG` is the tag
// the allocator writes into that header. `trace_fields` reports `name` and every element of
// `field_names`, which together are all of its `Value` fields.
unsafe impl HeapObject for RecordType {
    const TAG: HeapTag = HeapTag::RecordType;

    fn trace_fields(&self, tracer: &mut Tracer<'_>) {
        tracer.mark(self.name);
        for &v in &self.field_names {
            tracer.mark(v);
        }
    }

    fn extra_bytes(&self) -> usize {
        self.field_names.capacity() * size_of::<Value>()
    }
}

/// A primitive procedure implemented in Rust.
///
/// The object carries only the *identity* of the primitive: `index` names an entry in
/// the VM's native-function table, which owns the function pointer and arity metadata.
/// Keeping the table as the single authority means a serialized prototype could never
/// smuggle in a stale function pointer, and the printer can render the name without
/// consulting the VM.
#[repr(C)]
pub struct NativeProc {
    pub(crate) header: GcHeader,
    /// The primitive's name, for `write`/`display` and error messages.
    pub name: Rc<str>,
    /// Index into the VM's native-function table.
    pub index: u32,
}

impl NativeProc {
    pub(crate) fn new(name: Rc<str>, index: u32) -> Self {
        Self {
            header: GcHeader::unlinked(HeapTag::NativeProc),
            name,
            index,
        }
    }
}

impl Header for NativeProc {
    fn header_mut(&mut self) -> &mut GcHeader {
        &mut self.header
    }
}

// SAFETY: `#[repr(C)]` with `GcHeader` first, asserted in `layout`, and `TAG` is the tag
// the allocator writes into that header. A `NativeProc` owns its name and no `Value`s, so
// there is nothing to trace.
unsafe impl HeapObject for NativeProc {
    const TAG: HeapTag = HeapTag::NativeProc;

    fn trace_fields(&self, _tracer: &mut Tracer<'_>) {}

    fn extra_bytes(&self) -> usize {
        self.name.len()
    }
}

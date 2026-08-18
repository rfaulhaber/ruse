//! The concrete heap objects.
//!
//! Every one of them is `#[repr(C)]` with its [`GcHeader`] first, which is what lets the
//! collector read a tag off an untyped pointer and recover the real type from it. The
//! offsets are asserted at compile time in [`crate::value::layout`].
//!
//! Objects are never constructed directly: [`Heap`](crate::gc::Heap) owns their lifecycle,
//! because an object that is not on the heap's all-objects list is invisible to the sweep.

use std::rc::Rc;

use num_bigint::BigInt;

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

unsafe impl HeapObject for Bytevector {
    const TAG: HeapTag = HeapTag::Bytevector;

    fn trace_fields(&self, _tracer: &mut Tracer<'_>) {}

    fn extra_bytes(&self) -> usize {
        self.bytes.capacity()
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

//! The memory map of the runtime: the NaN-box encoding and the heap-object header.
//!
//! Every mask, shift, tag constant and field offset in ruse is defined exactly once, here.
//! The allocator, the collector, the printer and the compiler read these definitions
//! rather than restating them; tag logic duplicated across those consumers is the classic
//! way a NaN-boxed runtime drifts out of agreement with itself.
//!
//! # The encoding
//!
//! A [`Value`](crate::value::Value) is a 64-bit word that is either an immediate IEEE-754
//! double or a *boxed* value — a 3-bit tag and a 48-bit payload smuggled into the
//! negative-quiet-NaN region of the double space:
//!
//! ```text
//!  63   62..52     51    50..48   47..0
//!  s    exponent   q     tag      payload
//!  1    all ones   1     0..=7    48 bits     -> boxed value
//!  anything else                              -> immediate flonum
//! ```
//!
//! The signature region is *negative* quiet NaNs, so the only doubles it can collide with
//! are NaNs — and [`Value::flonum`](crate::value::Value::flonum) canonicalizes every NaN to
//! [`CANON_NAN`], a *positive* quiet NaN that lies outside the region. `+nan.0` therefore
//! stays an immediate flonum and is never misread as a pointer.
//!
//! That canonicalization is not optional: x86-64 produces `0xFFF8_0000_0000_0000` (the
//! "indefinite" QNaN) for `0.0 / 0.0`, which is exactly the signature word with tag 0 and a
//! zero payload. Singleton ordinal 0 is [`SINGLETON_UNDEFINED`] for that reason — should an
//! uncanonicalized NaN ever leak in, it decodes as the black-hole marker, which errors
//! loudly downstream, rather than as `#f` (a silently wrong answer) or a null heap pointer.

use core::mem::{align_of, offset_of, size_of};

use crate::value::object::{
    Bignum, Bytevector, Pair, Record, RecordType, Str, Symbol, UpvalueCell, Vector,
};

/// Bits set in every boxed value: the sign bit, the eleven exponent bits and the quiet-NaN
/// bit. A word is an immediate flonum exactly when these are *not* all set.
pub const SIG_MASK: u64 = 0xFFF8_0000_0000_0000;

/// Signature plus tag: the discriminating half-word of a boxed value.
pub const TAG_MASK: u64 = 0xFFFF_0000_0000_0000;

/// The 48 bits available to a boxed value's payload.
pub const PAYLOAD_MASK: u64 = 0x0000_FFFF_FFFF_FFFF;

/// Bit position of the 3-bit tag field.
pub const TAG_SHIFT: u32 = 48;

/// Immediate singletons; the payload is an ordinal below.
pub const TAG_SINGLETON: u64 = SIG_MASK | (0 << TAG_SHIFT);
/// A Unicode scalar value in the payload.
pub const TAG_CHAR: u64 = SIG_MASK | (1 << TAG_SHIFT);
/// A 48-bit two's-complement integer in the payload.
pub const TAG_FIXNUM: u64 = SIG_MASK | (2 << TAG_SHIFT);
/// The address of a [`GcHeader`] in the payload.
pub const TAG_HEAP: u64 = SIG_MASK | (4 << TAG_SHIFT);

/// The one NaN bit pattern ruse ever stores. Positive quiet NaN, outside [`SIG_MASK`].
pub const CANON_NAN: u64 = 0x7FF8_0000_0000_0000;

/// Undefined: the `letrec*` black hole and the unbound-global marker.
pub const SINGLETON_UNDEFINED: u64 = 0;
/// The value of expressions whose result R7RS leaves unspecified.
pub const SINGLETON_UNSPECIFIED: u64 = 1;
/// The empty list, `'()`.
pub const SINGLETON_NULL: u64 = 2;
/// The end-of-file object.
pub const SINGLETON_EOF: u64 = 3;
/// `#f`. Adjacent to `#t` so that a boolean test is one mask and one compare.
pub const SINGLETON_FALSE: u64 = 4;
/// `#t`.
pub const SINGLETON_TRUE: u64 = 5;

/// Number of payload bits a fixnum occupies.
pub const FIXNUM_BITS: u32 = 48;
/// Most negative exact integer representable without allocating.
pub const FIXNUM_MIN: i64 = -(1 << (FIXNUM_BITS - 1));
/// Most positive exact integer representable without allocating.
pub const FIXNUM_MAX: i64 = (1 << (FIXNUM_BITS - 1)) - 1;

/// Mark colour in the tri-colour invariant: white is unreached, grey is reached but not yet
/// scanned, black is reached and scanned.
///
/// A stop-the-world collector could get by with a single mark bit. The third colour is here
/// because decision B commits to emitting write barriers from day one, and a barrier has
/// nothing to say without a black/grey distinction to restore.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum Color {
    /// Not yet reached by the current mark phase; swept if it stays this way.
    White = 0,
    /// Reached, fields not yet scanned; sitting on the grey worklist.
    Gray = 1,
    /// Reached and scanned.
    Black = 2,
}

/// The concrete type of a heap object. A [`Value`](crate::value::Value) carries only "this
/// is a pointer"; the type lives here, in the object header.
///
/// The numbering is frozen: it is the discriminant the collector dispatches on.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum HeapTag {
    /// [`Pair`]: `car` and `cdr`.
    Pair = 0,
    /// [`Str`]: a mutable Scheme string.
    Str = 1,
    /// [`Symbol`]: interned or uninterned.
    Symbol = 2,
    /// [`Vector`]: a mutable vector of values.
    Vector = 3,
    /// [`Bytevector`]: a mutable vector of octets.
    Bytevector = 4,
    /// A closure over a compiled prototype.
    ///
    /// The object itself lands with `Proto` in M2; the tag is reserved now so the numbering
    /// never has to move. Nothing can allocate one yet.
    Closure = 5,
    /// [`UpvalueCell`]: the shared box a captured variable lives in.
    UpvalueCell = 6,
    /// [`Bignum`]: an exact integer too large for a fixnum.
    Bignum = 7,
    /// [`Record`]: an instance of a `define-record-type` type.
    Record = 8,
    /// [`RecordType`]: the descriptor such instances point at.
    RecordType = 9,
}

/// The first field of every heap object.
///
/// `next` threads every live object onto one intrusive list in allocation order, which is
/// what the sweep walks. Because the collector never moves objects, that list is the only
/// bookkeeping an object needs.
#[derive(Debug)]
#[repr(C)]
pub struct GcHeader {
    /// Next object on the heap's all-objects list, or null at the end.
    pub(crate) next: *mut GcHeader,
    /// The object's concrete type.
    ///
    /// Crate-private, and it must stay that way. Both [`Heap::get`](crate::gc::Heap::get)
    /// and the sweep decide which concrete type an untyped pointer really is by reading
    /// this byte, so code able to write it could make the sweep reconstruct one type's
    /// `Box` from another type's allocation. Read it through
    /// [`Heap::tag_of`](crate::gc::Heap::tag_of).
    pub(crate) tag: HeapTag,
    /// Mark colour for the current collection.
    pub(crate) color: Color,
    /// Per-object bits. Reserved for the literal-immutability flag of R7RS 4.1.2 / 6.7.
    pub(crate) flags: u8,
    _reserved: u8,
}

impl GcHeader {
    /// A header for an object that is not yet on the heap's list. [`Heap::alloc`] overwrites
    /// it with the real one; it exists so object constructors can be plain `fn new`.
    ///
    /// [`Heap::alloc`]: crate::gc::Heap
    pub(crate) const fn unlinked(tag: HeapTag) -> Self {
        Self {
            next: core::ptr::null_mut(),
            tag,
            color: Color::White,
            flags: 0,
            _reserved: 0,
        }
    }

    /// A header linked ahead of `next` on the all-objects list.
    pub(crate) const fn linked(tag: HeapTag, next: *mut GcHeader) -> Self {
        Self {
            next,
            tag,
            color: Color::White,
            flags: 0,
            _reserved: 0,
        }
    }
}

// The encoding's own invariants, and the layout guarantee the collector relies on: every
// heap object starts with its `GcHeader`, so a `*mut T` and a `*mut GcHeader` are the same
// address and the sweep can read a tag before it knows a type.
const _: () = {
    assert!(TAG_SINGLETON & SIG_MASK == SIG_MASK);
    assert!(TAG_CHAR & SIG_MASK == SIG_MASK);
    assert!(TAG_FIXNUM & SIG_MASK == SIG_MASK);
    assert!(TAG_HEAP & SIG_MASK == SIG_MASK);
    assert!(TAG_SINGLETON & PAYLOAD_MASK == 0);
    assert!(TAG_MASK | PAYLOAD_MASK == u64::MAX);
    assert!(TAG_MASK & PAYLOAD_MASK == 0);

    // The canonical NaN must decode as a flonum, or `+nan.0` becomes a tagged value.
    assert!(CANON_NAN & SIG_MASK != SIG_MASK);
    assert!(f64::from_bits(CANON_NAN).is_nan());

    // `#f` and `#t` differ in exactly the low bit, which is what makes `is_boolean` one
    // mask and one compare.
    assert!(SINGLETON_TRUE == SINGLETON_FALSE | 1);
    assert!(SINGLETON_FALSE & 1 == 0);

    // An uncanonicalized x86-64 indefinite QNaN must decode as `undefined`, not as `#f` and
    // not as a null heap pointer.
    assert!(TAG_SINGLETON | SINGLETON_UNDEFINED == 0xFFF8_0000_0000_0000);

    assert!(FIXNUM_MIN == -140_737_488_355_328);
    assert!(FIXNUM_MAX == 140_737_488_355_327);

    // A 48-bit payload cannot hold a pointer on a target whose pointers are not 64 bits,
    // and `Value` would silently truncate one.
    assert!(size_of::<usize>() == 8);

    assert!(size_of::<GcHeader>() == 16);
    assert!(align_of::<GcHeader>() == 8);
    assert!(size_of::<HeapTag>() == 1);
    assert!(size_of::<Color>() == 1);

    assert!(offset_of!(GcHeader, next) == 0);
    assert!(offset_of!(GcHeader, tag) == 8);
    assert!(offset_of!(GcHeader, color) == 9);
    assert!(offset_of!(GcHeader, flags) == 10);

    assert!(offset_of!(Pair, header) == 0);
    assert!(offset_of!(Str, header) == 0);
    assert!(offset_of!(Symbol, header) == 0);
    assert!(offset_of!(Vector, header) == 0);
    assert!(offset_of!(Bytevector, header) == 0);
    assert!(offset_of!(UpvalueCell, header) == 0);
    assert!(offset_of!(Bignum, header) == 0);
    assert!(offset_of!(Record, header) == 0);
    assert!(offset_of!(RecordType, header) == 0);

    // Payloads start where the header ends. These catch a field reordering or a lost
    // `repr(C)` — either of which would leave the collector reading a `Vec`'s pointer as a
    // tag while every unit test still passed.
    assert!(offset_of!(Pair, car) == 16);
    assert!(offset_of!(Pair, cdr) == 24);
    assert!(offset_of!(Str, chars) == 16);
    assert!(offset_of!(Symbol, name) == 16);
    assert!(offset_of!(Symbol, interned) == 32);
    assert!(offset_of!(Vector, elems) == 16);
    assert!(offset_of!(Bytevector, bytes) == 16);
    assert!(offset_of!(UpvalueCell, value) == 16);
    assert!(offset_of!(Bignum, value) == 16);
    assert!(offset_of!(Record, rtype) == 16);
    assert!(offset_of!(Record, fields) == 24);
    assert!(offset_of!(RecordType, name) == 16);
    assert!(offset_of!(RecordType, field_names) == 24);

    // A pair is the allocation the runtime makes most; keep an eye on its size.
    assert!(size_of::<Pair>() == 32);
};

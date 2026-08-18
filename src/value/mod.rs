//! The NaN-boxed [`Value`] and the heap objects it can point at.
//!
//! The encoding itself — every mask, tag and offset — lives in [`layout`]; this module is
//! the API over it.

pub mod layout;
pub mod object;

use core::fmt;
use core::marker::PhantomData;
use core::ptr::with_exposed_provenance_mut;

use layout::{
    CANON_NAN, GcHeader, PAYLOAD_MASK, SIG_MASK, SINGLETON_EOF, SINGLETON_FALSE, SINGLETON_NULL,
    SINGLETON_TRUE, SINGLETON_UNDEFINED, SINGLETON_UNSPECIFIED, TAG_CHAR, TAG_FIXNUM, TAG_HEAP,
    TAG_MASK, TAG_SINGLETON,
};

pub use layout::{FIXNUM_MAX, FIXNUM_MIN};

/// A Scheme value: one 64-bit word, NaN-boxed.
///
/// Flonums are the identity encoding — an `f64`'s own bits — so floating-point arithmetic
/// costs nothing to box or unbox. Everything else hides in the negative-quiet-NaN region of
/// the double space under a 3-bit tag; see [`layout`] for the map.
///
/// ```
/// use ruse::Value;
///
/// let n = Value::fixnum(42).unwrap();
/// assert_eq!(n.as_fixnum(), Some(42));
/// assert!(n.truthy());
///
/// let x = Value::flonum(1.5);
/// assert_eq!(x.as_flonum(), Some(1.5));
///
/// // Only `#f` is false.
/// assert!(Value::NIL.truthy());
/// assert!(!Value::FALSE.truthy());
/// ```
///
/// # Thread affinity
///
/// A `Value` can hold a raw pointer into a [`Heap`](crate::gc::Heap), so it is deliberately
/// neither `Send` nor `Sync`: moving one to another thread would let that thread dereference
/// an object this thread owns and may free.
///
/// # Equality
///
/// `PartialEq` is bitwise, which makes it R7RS `eq?` — pointer identity for heap objects,
/// bit identity for immediates. It is *not* `eqv?` or `equal?`: `eqv?` must compare numbers
/// by value across representations (a fixnum `1` and a bignum `1` are `eqv?` but not `eq?`;
/// `+0.0` and `-0.0` are not `eqv?` although R7RS leaves `eq?` on them unspecified). Those
/// live in the runtime, not here.
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(u64, PhantomData<*const ()>);

impl Value {
    /// The `letrec*` black hole, and the marker an unassigned global slot holds.
    pub const UNDEFINED: Self = Self::singleton(SINGLETON_UNDEFINED);
    /// The value of expressions whose result R7RS leaves unspecified.
    pub const UNSPECIFIED: Self = Self::singleton(SINGLETON_UNSPECIFIED);
    /// The empty list, `'()`.
    pub const NIL: Self = Self::singleton(SINGLETON_NULL);
    /// The end-of-file object.
    pub const EOF: Self = Self::singleton(SINGLETON_EOF);
    /// `#f` — the only false value in Scheme.
    pub const FALSE: Self = Self::singleton(SINGLETON_FALSE);
    /// `#t`.
    pub const TRUE: Self = Self::singleton(SINGLETON_TRUE);

    #[inline]
    const fn from_bits(bits: u64) -> Self {
        Self(bits, PhantomData)
    }

    #[inline]
    const fn singleton(ordinal: u64) -> Self {
        Self::from_bits(TAG_SINGLETON | ordinal)
    }

    /// The raw encoded word. For the collector, the disassembler and tests; the VM should
    /// go through the accessors.
    #[inline]
    pub const fn to_bits(self) -> u64 {
        self.0
    }

    /// `#t` or `#f`.
    #[inline]
    pub const fn boolean(b: bool) -> Self {
        Self::singleton(SINGLETON_FALSE | b as u64)
    }

    /// An inexact real.
    ///
    /// Every NaN — whatever its sign, payload, or signalling bit — is canonicalized to one
    /// pattern outside the boxed-value signature region, so `+nan.0` stays an immediate
    /// flonum rather than decoding as a tagged value. Without this, x86-64's indefinite
    /// QNaN from `0.0 / 0.0` would read back as a boxed value.
    ///
    /// ```
    /// use ruse::Value;
    ///
    /// let nan = Value::flonum(f64::NAN);
    /// assert!(nan.is_flonum());
    /// assert!(nan.as_flonum().unwrap().is_nan());
    /// // Sign, payload and signalling bit are all erased.
    /// assert_eq!(nan, Value::flonum(-f64::NAN));
    /// assert_eq!(nan, Value::flonum(f64::from_bits(0xFFF8_0000_0000_0001)));
    /// ```
    #[inline]
    pub fn flonum(x: f64) -> Self {
        Self::from_bits(if x.is_nan() { CANON_NAN } else { x.to_bits() })
    }

    /// An exact integer, if it fits in the 48-bit immediate range.
    ///
    /// Returns `None` outside `[FIXNUM_MIN, FIXNUM_MAX]`; use
    /// [`Heap::integer`](crate::gc::Heap::integer) to get a bignum instead of a `None`.
    #[inline]
    pub const fn fixnum(n: i64) -> Option<Self> {
        if n < FIXNUM_MIN || n > FIXNUM_MAX {
            None
        } else {
            Some(Self::from_bits(TAG_FIXNUM | (n as u64 & PAYLOAD_MASK)))
        }
    }

    /// A character.
    #[inline]
    pub const fn char(c: char) -> Self {
        Self::from_bits(TAG_CHAR | c as u32 as u64)
    }

    /// Wrap a live heap object's header.
    ///
    /// # Panics
    ///
    /// If the address does not fit in 48 bits. That means the process is running under
    /// 5-level paging with an allocation above 2^47, which this encoding cannot represent;
    /// decision A's tagged-enum fallback is the answer there, and it is not built yet.
    /// Failing loudly beats silently truncating a pointer.
    #[inline]
    pub(crate) fn from_header_ptr(p: *mut GcHeader) -> Self {
        let addr = p.expose_provenance() as u64;
        assert!(
            addr & !PAYLOAD_MASK == 0,
            "heap address {addr:#018x} exceeds the 48 bits a NaN-boxed pointer can hold"
        );
        Self::from_bits(TAG_HEAP | addr)
    }

    #[inline]
    const fn tag(self) -> u64 {
        self.0 & TAG_MASK
    }

    /// Whether this is an immediate inexact real.
    #[inline]
    pub const fn is_flonum(self) -> bool {
        self.0 & SIG_MASK != SIG_MASK
    }

    /// Whether this is an immediate exact integer.
    #[inline]
    pub const fn is_fixnum(self) -> bool {
        self.tag() == TAG_FIXNUM
    }

    /// Whether this is a character.
    #[inline]
    pub const fn is_char(self) -> bool {
        self.tag() == TAG_CHAR
    }

    /// Whether this is one of the six immediate singletons.
    #[inline]
    pub const fn is_singleton(self) -> bool {
        self.tag() == TAG_SINGLETON
    }

    /// Whether this points at a heap object.
    #[inline]
    pub const fn is_heap(self) -> bool {
        self.tag() == TAG_HEAP
    }

    /// Whether this is `#t` or `#f`.
    #[inline]
    pub const fn is_boolean(self) -> bool {
        self.0 & !1 == Self::FALSE.0
    }

    /// Whether this is the empty list.
    #[inline]
    pub const fn is_null(self) -> bool {
        self.0 == Self::NIL.0
    }

    /// Whether this is the end-of-file object.
    #[inline]
    pub const fn is_eof(self) -> bool {
        self.0 == Self::EOF.0
    }

    /// Whether this is the `letrec*` black hole / unbound marker.
    #[inline]
    pub const fn is_undefined(self) -> bool {
        self.0 == Self::UNDEFINED.0
    }

    /// Whether this is the unspecified value.
    #[inline]
    pub const fn is_unspecified(self) -> bool {
        self.0 == Self::UNSPECIFIED.0
    }

    /// Scheme truthiness: everything except `#f` is true (R7RS 6.3).
    #[inline]
    pub const fn truthy(self) -> bool {
        self.0 != Self::FALSE.0
    }

    /// The inexact real, if this is one.
    #[inline]
    pub const fn as_flonum(self) -> Option<f64> {
        if self.is_flonum() {
            Some(f64::from_bits(self.0))
        } else {
            None
        }
    }

    /// The exact integer, if this is an immediate one.
    ///
    /// Sign-extends from bit 47, so the full `[FIXNUM_MIN, FIXNUM_MAX]` range round-trips.
    #[inline]
    pub const fn as_fixnum(self) -> Option<i64> {
        if self.is_fixnum() {
            Some(((self.0 << (64 - layout::FIXNUM_BITS)) as i64) >> (64 - layout::FIXNUM_BITS))
        } else {
            None
        }
    }

    /// The character, if this is one.
    #[inline]
    pub fn as_char(self) -> Option<char> {
        if self.is_char() {
            u32::try_from(self.0 & PAYLOAD_MASK)
                .ok()
                .and_then(char::from_u32)
        } else {
            None
        }
    }

    /// The boolean, if this is one.
    #[inline]
    pub const fn as_boolean(self) -> Option<bool> {
        if self.is_boolean() {
            Some(self.0 & 1 != 0)
        } else {
            None
        }
    }

    /// The header of the object this points at, if it points at one.
    ///
    /// Returning the raw pointer is safe; dereferencing it is not, because nothing here
    /// proves the object is still live — a `Value` is `Copy` and carries no lifetime, so
    /// one held across a collection that did not reach it is dangling. Go through
    /// [`Heap::get`](crate::gc::Heap::get), which at least ties the resulting *reference*
    /// to a heap that cannot collect while it is held, and which checks liveness in debug
    /// builds. Keeping the value itself alive is the caller's job, through a root or a pin.
    #[inline]
    pub fn header_ptr(self) -> Option<*mut GcHeader> {
        if self.is_heap() {
            Some(with_exposed_provenance_mut(
                (self.0 & PAYLOAD_MASK) as usize,
            ))
        } else {
            None
        }
    }
}

/// Shows the encoding, never the pointee.
///
/// Following a heap pointer here would mean dereferencing without proof that the object is
/// live, and would loop forever on a cyclic list. `write`/`display` arrive with the printer
/// in M3.
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(x) = self.as_flonum() {
            write!(f, "Value(flonum {x:?})")
        } else if let Some(n) = self.as_fixnum() {
            write!(f, "Value(fixnum {n})")
        } else if let Some(c) = self.as_char() {
            write!(f, "Value(char {c:?})")
        } else if self.is_heap() {
            write!(f, "Value(heap {:#018x})", self.0 & PAYLOAD_MASK)
        } else if self.is_singleton() {
            let name = match self.0 & PAYLOAD_MASK {
                SINGLETON_UNDEFINED => "undefined",
                SINGLETON_UNSPECIFIED => "unspecified",
                SINGLETON_NULL => "()",
                SINGLETON_EOF => "eof",
                SINGLETON_FALSE => "#f",
                SINGLETON_TRUE => "#t",
                _ => "singleton?",
            };
            write!(f, "Value({name})")
        } else {
            write!(f, "Value(reserved {:#018x})", self.0)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every constructible value must answer exactly one of the four boxed-or-flonum
    /// questions. A value that answers two, or none, is a hole in the encoding.
    fn classify(v: Value) -> u32 {
        u32::from(v.is_flonum())
            + u32::from(v.is_fixnum())
            + u32::from(v.is_char())
            + u32::from(v.is_singleton())
            + u32::from(v.is_heap())
    }

    #[test]
    fn fixnums_round_trip_across_the_whole_range() {
        for n in [
            0,
            1,
            -1,
            2,
            -2,
            127,
            -128,
            65_535,
            -65_536,
            1 << 46,
            -(1 << 46),
            FIXNUM_MAX,
            FIXNUM_MIN,
            FIXNUM_MAX - 1,
            FIXNUM_MIN + 1,
        ] {
            let v = Value::fixnum(n).unwrap();
            assert_eq!(v.as_fixnum(), Some(n), "round trip failed for {n}");
            assert_eq!(classify(v), 1, "{n} is not exactly one type");
            assert!(v.truthy(), "{n} must be true; only #f is false");
        }
    }

    #[test]
    fn fixnums_outside_48_bits_are_rejected() {
        assert!(Value::fixnum(FIXNUM_MAX + 1).is_none());
        assert!(Value::fixnum(FIXNUM_MIN - 1).is_none());
        assert!(Value::fixnum(i64::MAX).is_none());
        assert!(Value::fixnum(i64::MIN).is_none());
    }

    #[test]
    fn flonums_round_trip_bit_for_bit() {
        for x in [
            0.0f64,
            -0.0,
            1.0,
            -1.0,
            0.5,
            f64::MIN,
            f64::MAX,
            f64::MIN_POSITIVE,
            f64::EPSILON,
            f64::INFINITY,
            f64::NEG_INFINITY,
            f64::from_bits(1),                     // smallest subnormal
            f64::from_bits(0x000F_FFFF_FFFF_FFFF), // largest subnormal
            core::f64::consts::PI,
        ] {
            let v = Value::flonum(x);
            assert_eq!(classify(v), 1, "{x} is not exactly one type");
            assert_eq!(v.as_flonum().map(f64::to_bits), Some(x.to_bits()), "{x}");
        }
        // -0.0 and 0.0 are distinct words, as IEEE requires.
        assert_ne!(Value::flonum(0.0), Value::flonum(-0.0));
    }

    #[test]
    fn every_nan_encoding_canonicalizes_to_one_flonum() {
        let canonical = Value::flonum(f64::NAN);
        for bits in [
            0x7FF8_0000_0000_0000, // positive quiet NaN
            0xFFF8_0000_0000_0000, // x86-64 indefinite QNaN, the dangerous one
            0x7FF8_0000_DEAD_BEEF, // quiet NaN with a payload
            0xFFFF_FFFF_FFFF_FFFF, // all bits set: quiet NaN, deep in the signature region
            0x7FF0_0000_0000_0001, // positive signalling NaN
            0xFFF0_0000_0000_0001, // negative signalling NaN
        ] {
            let x = f64::from_bits(bits);
            assert!(x.is_nan(), "{bits:#018x} should be a NaN");
            let v = Value::flonum(x);
            assert_eq!(v, canonical, "{bits:#018x} did not canonicalize");
            assert!(v.is_flonum(), "{bits:#018x} must stay a flonum");
            assert_eq!(classify(v), 1);
        }
    }

    /// Infinities sit right next to the signature region; -inf shares its sign and exponent
    /// and differs only in the quiet bit.
    #[test]
    fn infinities_are_flonums_not_boxed_values() {
        for x in [f64::INFINITY, f64::NEG_INFINITY] {
            let v = Value::flonum(x);
            assert!(v.is_flonum());
            assert_eq!(v.as_flonum(), Some(x));
            assert_eq!(classify(v), 1);
        }
        assert_eq!(
            Value::flonum(f64::NEG_INFINITY).to_bits(),
            0xFFF0_0000_0000_0000
        );
    }

    #[test]
    fn chars_round_trip_including_the_extremes() {
        for c in ['\0', 'a', 'λ', '\u{7F}', '\u{D7FF}', '\u{E000}', char::MAX] {
            let v = Value::char(c);
            assert_eq!(v.as_char(), Some(c), "round trip failed for {c:?}");
            assert_eq!(classify(v), 1, "{c:?} is not exactly one type");
            assert!(v.truthy());
        }
    }

    #[test]
    fn singletons_are_distinct_and_only_false_is_false() {
        let all = [
            Value::UNDEFINED,
            Value::UNSPECIFIED,
            Value::NIL,
            Value::EOF,
            Value::FALSE,
            Value::TRUE,
        ];
        for (i, a) in all.iter().enumerate() {
            assert_eq!(classify(*a), 1, "{a:?} is not exactly one type");
            assert!(a.is_singleton());
            for b in &all[i + 1..] {
                assert_ne!(a, b, "{a:?} and {b:?} collide");
            }
        }
        assert!(!Value::FALSE.truthy());
        for v in [
            Value::TRUE,
            Value::NIL,
            Value::EOF,
            Value::UNSPECIFIED,
            Value::UNDEFINED,
        ] {
            assert!(v.truthy(), "{v:?} must be true; only #f is false");
        }
    }

    #[test]
    fn booleans() {
        assert_eq!(Value::boolean(true), Value::TRUE);
        assert_eq!(Value::boolean(false), Value::FALSE);
        assert_eq!(Value::TRUE.as_boolean(), Some(true));
        assert_eq!(Value::FALSE.as_boolean(), Some(false));
        assert!(Value::TRUE.is_boolean() && Value::FALSE.is_boolean());
        for v in [
            Value::NIL,
            Value::EOF,
            Value::UNDEFINED,
            Value::UNSPECIFIED,
            Value::fixnum(4).unwrap(),
            Value::fixnum(5).unwrap(),
            Value::char('\u{4}'),
            Value::flonum(0.0),
        ] {
            assert!(!v.is_boolean(), "{v:?} must not read as a boolean");
            assert_eq!(v.as_boolean(), None);
        }
    }

    /// The reason singleton ordinal 0 is `undefined`: if an uncanonicalized hardware NaN
    /// ever reaches a `Value`, this is what it decodes as, and `undefined` is the one
    /// decoding that errors loudly instead of silently answering `#f` or dereferencing null.
    #[test]
    fn the_x86_indefinite_nan_pattern_decodes_as_undefined() {
        assert_eq!(Value::UNDEFINED.to_bits(), 0xFFF8_0000_0000_0000);
        assert!(!Value::UNDEFINED.is_heap());
        assert!(!Value::UNDEFINED.is_boolean());
    }

    #[test]
    fn accessors_reject_the_wrong_type() {
        let n = Value::fixnum(1).unwrap();
        assert_eq!(n.as_flonum(), None);
        assert_eq!(n.as_char(), None);
        assert_eq!(n.as_boolean(), None);
        assert_eq!(n.header_ptr(), None);

        let x = Value::flonum(1.0);
        assert_eq!(x.as_fixnum(), None);
        assert_eq!(x.as_char(), None);
        assert_eq!(x.header_ptr(), None);

        let c = Value::char('a');
        assert_eq!(c.as_fixnum(), None);
        assert_eq!(c.as_flonum(), None);
        assert_eq!(c.header_ptr(), None);
    }

    #[test]
    fn a_value_is_one_machine_word() {
        assert_eq!(size_of::<Value>(), 8);
        assert_eq!(align_of::<Value>(), 8);
        assert_eq!(size_of::<Option<Value>>(), 16);
    }

    #[test]
    fn debug_never_follows_a_pointer() {
        assert_eq!(
            format!("{:?}", Value::fixnum(-7).unwrap()),
            "Value(fixnum -7)"
        );
        assert_eq!(format!("{:?}", Value::NIL), "Value(())");
        assert_eq!(format!("{:?}", Value::char('q')), "Value(char 'q')");
    }
}

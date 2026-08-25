//! The 32-bit instruction word: spec §3 accessors and their symmetric encoders.
//!
//! ```text
//!  31              24 23              16 15               8 7                0
//! ┌──────────────────┬──────────────────┬──────────────────┬──────────────────┐
//! │        C : 8     │        B : 8     │        A : 8     │      OP : 8      │  iABC
//! ├──────────────────┴──────────────────┼──────────────────┼──────────────────┤
//! │              Bx : 16 (unsigned)     │        A : 8     │      OP : 8      │  iABx
//! ├─────────────────────────────────────┼──────────────────┼──────────────────┤
//! │             sBx : 16 (signed)       │        A : 8     │      OP : 8      │  iAsBx
//! ├─────────────────────────────────────┴──────────────────┼──────────────────┤
//! │                      Ax : 24 (unsigned)                │      OP : 8      │  iAx
//! └─────────────────────────────────────────────────────────┴─────────────────┘
//! ```
//!
//! The accessors are total — any field can be read from any word — because the dispatch
//! loop decodes before it knows the opcode is valid. Which fields *mean* anything is the
//! opcode's business, and whether they are in range is the verifier's.

use core::fmt;

use crate::bytecode::op::{Format, Op};

/// One RBC-1 instruction: an opcode in the low byte and 24 bits of operands.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Insn(pub u32);

impl Insn {
    // ---------------------------------------------------------------- encoders
    //
    // Every encoder asserts — in release builds too — that the opcode really has the
    // format being encoded. The check has to be loud because the failure is not: an
    // instruction encoded through the wrong constructor is a perfectly well-formed word
    // whose fields decode under the *opcode's* format, and the verifier cannot tell it
    // from intent. One comparison per encode is nothing next to that debugging session.

    /// Encode an `iABC` instruction.
    ///
    /// Also used for the opcodes that read C as a signed byte or a flag — pass
    /// `sc as u8` for the former; the bit pattern is the same.
    ///
    /// # Panics
    ///
    /// If `op` is not an iABC opcode.
    #[inline]
    pub const fn iabc(op: Op, a: u8, b: u8, c: u8) -> Self {
        assert!(matches!(op.format(), Format::Abc), "opcode is not iABC");
        Self(op as u32 | (a as u32) << 8 | (b as u32) << 16 | (c as u32) << 24)
    }

    /// Encode an `iABx` instruction.
    ///
    /// # Panics
    ///
    /// If `op` is not an iABx opcode.
    #[inline]
    pub const fn iabx(op: Op, a: u8, bx: u16) -> Self {
        assert!(matches!(op.format(), Format::Abx), "opcode is not iABx");
        Self(op as u32 | (a as u32) << 8 | (bx as u32) << 16)
    }

    /// Encode an `iAsBx` instruction. `sbx` is stored as 16-bit two's complement.
    ///
    /// # Panics
    ///
    /// If `op` is not an iAsBx opcode.
    #[inline]
    pub const fn iasbx(op: Op, a: u8, sbx: i16) -> Self {
        assert!(matches!(op.format(), Format::Asbx), "opcode is not iAsBx");
        Self(op as u32 | (a as u32) << 8 | (sbx as u16 as u32) << 16)
    }

    /// Encode an `iAx` instruction.
    ///
    /// # Panics
    ///
    /// If `op` is not an iAx opcode, or `ax` does not fit in 24 bits. The only producer
    /// is the compiler spilling a constant-pool index through EXTRAARG, and an index past
    /// 2^24 is a compiler bug that must fail loudly, not wrap into a different constant.
    #[inline]
    pub const fn iax(op: Op, ax: u32) -> Self {
        assert!(matches!(op.format(), Format::Ax), "opcode is not iAx");
        assert!(ax < 1 << 24, "Ax operand exceeds 24 bits");
        Self(op as u32 | ax << 8)
    }

    // ---------------------------------------------------------------- accessors (spec §3.1)

    /// The opcode byte, undecoded.
    #[inline(always)]
    pub const fn op(self) -> u8 {
        (self.0 & 0xFF) as u8
    }

    /// The opcode, if the byte names one.
    #[inline]
    pub const fn opcode(self) -> Option<Op> {
        Op::from_byte(self.op())
    }

    /// The A field (bits 8–15).
    #[inline(always)]
    pub const fn a(self) -> u8 {
        (self.0 >> 8) as u8
    }

    /// The B field (bits 16–23).
    #[inline(always)]
    pub const fn b(self) -> u8 {
        (self.0 >> 16) as u8
    }

    /// The C field (bits 24–31).
    #[inline(always)]
    pub const fn c(self) -> u8 {
        (self.0 >> 24) as u8
    }

    /// The C field as a signed byte (`sC`), for [`Op::AddI`].
    #[inline(always)]
    pub const fn sc(self) -> i8 {
        self.c() as i8
    }

    /// The Bx field (bits 16–31, unsigned).
    #[inline(always)]
    pub const fn bx(self) -> u16 {
        (self.0 >> 16) as u16
    }

    /// The sBx field (bits 16–31, two's complement).
    #[inline(always)]
    pub const fn sbx(self) -> i16 {
        (self.0 >> 16) as u16 as i16
    }

    /// The Ax field (bits 8–31, unsigned).
    #[inline(always)]
    pub const fn ax(self) -> u32 {
        self.0 >> 8
    }
}

/// Shows the fields the opcode's format defines, or the raw word when the byte is not an
/// opcode.
impl fmt::Debug for Insn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.opcode() {
            Some(op) => match op.format() {
                Format::Abc => write!(f, "Insn({op} a={} b={} c={})", self.a(), self.b(), self.c()),
                Format::Abx => write!(f, "Insn({op} a={} bx={})", self.a(), self.bx()),
                Format::Asbx => write!(f, "Insn({op} a={} sbx={})", self.a(), self.sbx()),
                Format::Ax => write!(f, "Insn({op} ax={})", self.ax()),
            },
            None => write!(f, "Insn(invalid {:#010x})", self.0),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn abc_fields_round_trip() {
        for (a, b, c) in [(0, 0, 0), (1, 2, 3), (249, 255, 255), (0x80, 0x7F, 0xFF)] {
            let i = Insn::iabc(Op::Add, a, b, c);
            assert_eq!(i.opcode(), Some(Op::Add));
            assert_eq!((i.a(), i.b(), i.c()), (a, b, c));
        }
    }

    #[test]
    fn the_signed_byte_view_of_c_round_trips() {
        for sc in [i8::MIN, -1, 0, 1, i8::MAX] {
            let i = Insn::iabc(Op::AddI, 3, 1, sc as u8);
            assert_eq!(i.sc(), sc);
        }
    }

    #[test]
    fn abx_fields_round_trip() {
        for bx in [0u16, 1, 255, 256, u16::MAX] {
            let i = Insn::iabx(Op::LoadK, 7, bx);
            assert_eq!(i.opcode(), Some(Op::LoadK));
            assert_eq!((i.a(), i.bx()), (7, bx));
        }
    }

    #[test]
    fn sbx_is_twos_complement() {
        for sbx in [i16::MIN, -32767, -1, 0, 1, 32766, i16::MAX] {
            let i = Insn::iasbx(Op::Jmp, 0, sbx);
            assert_eq!(i.sbx(), sbx, "sbx {sbx} did not round trip");
        }
        // The bit pattern is the unsigned view of the same bits, per the encoding diagram.
        assert_eq!(Insn::iasbx(Op::Jmp, 0, -1).bx(), u16::MAX);
    }

    #[test]
    fn ax_spans_the_full_24_bits() {
        for ax in [0u32, 1, 0xFFFF, 0x10000, (1 << 24) - 1] {
            let i = Insn::iax(Op::ExtraArg, ax);
            assert_eq!(i.opcode(), Some(Op::ExtraArg));
            assert_eq!(i.ax(), ax);
        }
    }

    #[test]
    #[should_panic(expected = "24 bits")]
    fn an_oversized_ax_fails_loudly() {
        let _ = Insn::iax(Op::ExtraArg, 1 << 24);
    }

    /// A wrong-format encode would be a well-formed word the verifier cannot flag, so the
    /// constructor is the last place the mistake is visible.
    #[test]
    #[should_panic(expected = "not iABC")]
    fn encoding_through_the_wrong_format_fails_loudly() {
        let _ = Insn::iabc(Op::Jmp, 0, 0, 0);
    }

    #[test]
    fn the_opcode_sits_in_the_low_byte() {
        // The one raw-byte assertion the encoding itself owes: little-endian storage of the
        // u32 puts the opcode first in memory, which is what "OP : 8 in bits 0..8" means.
        let i = Insn::iabc(Op::Move, 1, 2, 0);
        assert_eq!(i.0 & 0xFF, Op::Move as u32);
        assert_eq!(i.op(), Op::Move as u8);
    }

    #[test]
    fn an_unassigned_byte_decodes_to_none_but_fields_still_read() {
        let i = Insn(0x0102_0340); // opcode 0x40: reserved for RBC-2
        assert_eq!(i.opcode(), None);
        assert_eq!(i.a(), 3);
        assert_eq!(i.b(), 2);
        assert_eq!(i.c(), 1);
        assert_eq!(format!("{i:?}"), "Insn(invalid 0x01020340)");
    }

    #[test]
    fn debug_shows_the_format_the_opcode_defines() {
        assert_eq!(
            format!("{:?}", Insn::iabc(Op::Add, 1, 2, 3)),
            "Insn(ADD a=1 b=2 c=3)"
        );
        assert_eq!(
            format!("{:?}", Insn::iabx(Op::LoadK, 0, 9)),
            "Insn(LOADK a=0 bx=9)"
        );
        assert_eq!(
            format!("{:?}", Insn::iasbx(Op::Jmp, 0, -2)),
            "Insn(JMP a=0 sbx=-2)"
        );
        assert_eq!(
            format!("{:?}", Insn::iax(Op::ExtraArg, 70000)),
            "Insn(EXTRAARG ax=70000)"
        );
    }
}

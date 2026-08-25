//! The RBC-1 opcode set: 50 opcodes, frozen.
//!
//! The byte assignments follow the grouped table in `ruse-bytecode-spec.md` §5: related
//! opcodes share 16-aligned columns, and the gaps (`0x0C`–`0x0F`, `0x1E`–`0x1F`,
//! `0x2A`–`0x2F`, `0x3E`–`0x3F`) are room for each group to grow without renumbering its
//! neighbours. `0x40`–`0x45` are reserved for the RBC-2 concurrency set and must not be
//! assigned here.
//!
//! Renumbering is deliberately cheap: nothing outside this module matches on raw bytes, and
//! the test suite keys on disassembly text (a frozen decision recorded in
//! `docs/project_plan.org`), so a new table costs one edit here and a snapshot review.

use core::fmt;

/// The four instruction formats of spec §3.
///
/// Every instruction is 32 bits with the opcode in the low byte; the format says how the
/// remaining 24 bits split into operand fields.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Format {
    /// `iABC` — three 8-bit fields. Some opcodes read C as a signed byte (`sC`), as a 0/1
    /// flag (`k`), or not at all; that interpretation is per-opcode, not part of the format.
    Abc,
    /// `iABx` — an 8-bit A and a 16-bit unsigned Bx (constant, global-key, or proto index).
    Abx,
    /// `iAsBx` — an 8-bit A and a 16-bit signed sBx (jump offset or immediate integer).
    Asbx,
    /// `iAx` — one 24-bit unsigned operand. Only [`Op::ExtraArg`] uses it.
    Ax,
}

macro_rules! ops {
    ($( $(#[doc = $doc:expr])* $byte:literal $name:ident $mnemonic:literal $format:ident; )+) => {
        /// One RBC-1 opcode.
        ///
        /// The discriminant is the encoded byte, so `op as u8` and the dispatch loop's
        /// `match` agree by construction. Semantics live in `ruse-bytecode-spec.md` §4;
        /// operand *validity* is [`verify`](crate::bytecode::verify())'s department.
        #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
        #[repr(u8)]
        pub enum Op {
            $( $(#[doc = $doc])* $name = $byte, )+
        }

        impl Op {
            /// Every opcode, in byte order. For exhaustive tests and table builders.
            pub const ALL: [Op; Self::COUNT] = [ $( Op::$name, )+ ];

            /// How many opcodes RBC-1 defines.
            pub const COUNT: usize = 0 $( + { let _ = $byte; 1 } )+;

            /// Decode a byte, if it names an opcode.
            #[inline]
            pub const fn from_byte(byte: u8) -> Option<Self> {
                match byte {
                    $( $byte => Some(Op::$name), )+
                    _ => None,
                }
            }

            /// The mnemonic the disassembler prints.
            pub const fn mnemonic(self) -> &'static str {
                match self {
                    $( Op::$name => $mnemonic, )+
                }
            }

            /// The instruction format this opcode is encoded in.
            pub const fn format(self) -> Format {
                match self {
                    $( Op::$name => Format::$format, )+
                }
            }
        }
    };
}

ops! {
    // -------------------------------------------------------------- data movement
    /// `RA := RB`.
    0x00 Move "MOVE" Abc;
    /// `RA := K[Bx]`.
    0x01 LoadK "LOADK" Abx;
    /// `RA := K[Ax]`, with `Ax` carried by the [`Op::ExtraArg`] that must follow.
    /// For constant pools past 65,536 entries. Bx is unused and must be zero.
    0x02 LoadKx "LOADKX" Abx;
    /// `RA :=` the immediate singleton whose ordinal is Bx. The operand *is* the
    /// `value::layout` singleton ordinal: 0=undefined 1=unspecified 2=`'()` 3=eof 4=`#f`
    /// 5=`#t`. There is no second table.
    0x03 LoadImm "LOADIMM" Abx;
    /// `RA :=` the fixnum sBx.
    0x04 LoadI "LOADI" Asbx;

    // -------------------------------------------------------------- arithmetic
    /// `RA := RB + RC`. Fixnum fast path; numeric tower otherwise.
    0x05 Add "ADD" Abc;
    /// `RA := RB - RC`.
    0x06 Sub "SUB" Abc;
    /// `RA := RB * RC`.
    0x07 Mul "MUL" Abc;
    /// `RA := RB / RC`, **exact**: `(/ 1 3)` is the rational `1/3` (R7RS §6.2.6).
    0x08 Div "DIV" Abc;
    /// `RA := (truncate-quotient RB RC)`. The machine-division escape hatch.
    0x09 Quot "QUOT" Abc;
    /// `RA := -RB`. C is unused.
    0x0A Neg "NEG" Abc;
    /// `RA := RB + sC`, with C read as a signed byte. The loop-counter fast path.
    0x0B AddI "ADDI" Abc;

    // -------------------------------------------------------------- comparison (skip-next)
    /// Skip the next instruction unless `(= RA RB)` equals the flag in C.
    0x10 NumEq "NUMEQ" Abc;
    /// Skip the next instruction unless `(< RA RB)` equals the flag in C.
    0x11 NumLt "NUMLT" Abc;
    /// Skip the next instruction unless `(<= RA RB)` equals the flag in C.
    0x12 NumLe "NUMLE" Abc;
    /// Skip the next instruction unless `(eq? RA RB)` equals the flag in C.
    0x13 Eq "EQ" Abc;
    /// Skip the next instruction unless `(eqv? RA RB)` equals the flag in C.
    0x14 Eqv "EQV" Abc;
    /// Skip the next instruction unless RA's truthiness equals the flag in C. B is unused.
    0x15 Test "TEST" Abc;

    // -------------------------------------------------------------- control flow
    /// `PC += sBx`. If `A > 0`, also close every open upvalue at register `A-1` and above.
    0x16 Jmp "JMP" Asbx;
    /// Carries a 24-bit operand for the [`Op::LoadKx`] immediately before it.
    /// Never executed alone; never a jump target.
    0x17 ExtraArg "EXTRAARG" Ax;
    /// Computed jump: `PC += RB` if `RA < RC`, else fall through. `case` dispatch tables.
    0x18 JmpIdx "JMPIDX" Abc;

    // -------------------------------------------------------------- pairs and lists
    /// `RA := (cons RB RC)`.
    0x19 Cons "CONS" Abc;
    /// `RA := (car RB)`.
    0x1A Car "CAR" Abc;
    /// `RA := (cdr RB)`.
    0x1B Cdr "CDR" Abc;
    /// `(set-car! RA RB)`. Write barrier.
    0x1C SetCar "SETCAR" Abc;
    /// `(set-cdr! RA RB)`. Write barrier.
    0x1D SetCdr "SETCDR" Abc;

    // -------------------------------------------------------------- fused accessors
    /// `RA :=` a `caar`…`cddddr` walk of RB, path encoded in C. The C-byte layout is
    /// open question 1 in `docs/project_plan.org` and is **not yet frozen**; until it is,
    /// the compiler emits plain CAR/CDR chains and the verifier does not constrain C.
    0x20 Cadr "CADR" Abc;
    /// Skip the next instruction unless RA has the type class selected by C. B is unused.
    /// The selector enum is open question 2 and is **not yet frozen**.
    0x21 TypeP "TYPEP" Abc;

    // -------------------------------------------------------------- calls and returns
    /// Call RA with `B-1` args from `R[A+1..=A+B-1]`; `C-1` results land at RA.
    /// `B=0` forwards everything from `R[A+1]` to the frame top; `C=0` accepts all results.
    0x22 Call "CALL" Abc;
    /// Proper tail call: replace the current frame. Same B convention as CALL; C unused.
    0x23 TailCall "TAILCALL" Abc;
    /// Return `B-1` values from `R[A..=A+B-2]`. `B=0` returns from RA to the frame top;
    /// `B=1` returns zero values. C is unused.
    0x24 Return "RETURN" Abc;
    /// Return exactly RA. The common case, minus RETURN's count arithmetic.
    0x25 Return1 "RETURN1" Abc;
    /// Like CALL, but the last argument register holds a list to spread. C is unused.
    0x26 Apply "APPLY" Abc;

    // -------------------------------------------------------------- closures and variables
    /// `RA :=` a new closure over child prototype Bx. Each upvalue's capture source is
    /// described by the child's own [`UpvalDesc`](crate::bytecode::UpvalDesc) table —
    /// there are no trailing pseudo-instructions.
    0x27 Closure "CLOSURE" Abx;
    /// `RA := U[B]`. C is unused.
    0x28 GetUpval "GETUPVAL" Abc;
    /// `U[B] := RA`. Write barrier. C is unused.
    0x29 SetUpval "SETUPVAL" Abc;
    /// `RA := G[K[Bx]]`; unbound (undefined) slots are an error.
    0x30 GetGlobal "GETGLOBAL" Abx;
    /// `G[K[Bx]] := RA`.
    0x31 SetGlobal "SETGLOBAL" Abx;
    /// `RA := R[B]` across a known frame boundary. Reserved for a future inliner; RBC-1
    /// compilers emit plain MOVE. C is unused.
    0x32 GetLocalN "GETLOCALN" Abc;
    /// Close every open upvalue at register A and above, without jumping. B and C unused.
    0x33 CloseUpvals "CLOSEUPVALS" Abc;

    // -------------------------------------------------------------- vectors and the bridge
    /// `RA := (vector-ref RB RC)`.
    0x34 VecRef "VECREF" Abc;
    /// `(vector-set! RA RB RC)`. Write barrier.
    0x35 VecSet "VECSET" Abc;
    /// `RA := (make-vector RB RC)` (length, fill).
    0x36 NewVec "NEWVEC" Abc;
    /// Call native function C with `B-1` args from `R[A+1..]`; result in RA. No bytecode
    /// frame. The escape hatch for every procedure without a dedicated opcode.
    0x37 PrimCall "PRIMCALL" Abc;

    // -------------------------------------------------------------- first-class control
    /// `RA :=` a continuation snapshotting the fiber's frame, wind and handler stacks.
    /// B and C unused.
    0x38 CaptureCc "CAPTURECC" Abc;
    /// Push a dynamic-wind record: before-thunk RA, after-thunk RB. C unused.
    0x39 WindPush "WINDPUSH" Abc;
    /// Pop the top wind record. No operands.
    0x3A WindPop "WINDPOP" Abc;
    /// Push exception handler RA, with `PC + sBx` as the escape target. The target is
    /// used only by the `guard` lowering: `with-exception-handler` is **non-transferring**
    /// (a frozen R7RS decision — `raise-continuable` returns to the raise point), so the
    /// spec draft's unconditional "on raise, control transfers" wording is superseded.
    0x3B HandlerPush "HANDLERPUSH" Asbx;
    /// Pop the top handler. No operands.
    0x3C HandlerPop "HANDLERPOP" Abc;
    /// Raise the condition in RA. B is a flag: 1 = continuable. C unused.
    0x3D Raise "RAISE" Abc;
}

/// First of the six opcode bytes reserved for the RBC-2 concurrency set
/// (`SPAWN`, `YIELD`, `RESUME`, `CHANMAKE`, `CHANSEND`, `CHANRECV`).
pub const RESERVED_RBC2_FIRST: u8 = 0x40;
/// Last of the reserved RBC-2 opcode bytes.
pub const RESERVED_RBC2_LAST: u8 = 0x45;

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.mnemonic())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn there_are_exactly_fifty_opcodes() {
        assert_eq!(Op::COUNT, 50);
        assert_eq!(Op::ALL.len(), 50);
    }

    #[test]
    fn every_opcode_round_trips_through_its_byte() {
        for op in Op::ALL {
            assert_eq!(Op::from_byte(op as u8), Some(op), "{op} lost its byte");
        }
    }

    #[test]
    fn bytes_between_opcodes_and_beyond_them_do_not_decode() {
        let assigned: Vec<u8> = Op::ALL.iter().map(|&op| op as u8).collect();
        for byte in 0..=u8::MAX {
            if assigned.contains(&byte) {
                continue;
            }
            assert_eq!(
                Op::from_byte(byte),
                None,
                "{byte:#04x} should be unassigned"
            );
        }
    }

    #[test]
    fn the_rbc2_reservation_is_untouched() {
        for byte in RESERVED_RBC2_FIRST..=RESERVED_RBC2_LAST {
            assert_eq!(
                Op::from_byte(byte),
                None,
                "{byte:#04x} is reserved for RBC-2 concurrency"
            );
        }
        // And nothing is assigned past the reservation either.
        assert!(Op::ALL.iter().all(|&op| (op as u8) < RESERVED_RBC2_FIRST));
    }

    #[test]
    fn mnemonics_are_unique_and_uppercase() {
        let mut seen = std::collections::HashSet::new();
        for op in Op::ALL {
            let m = op.mnemonic();
            assert!(seen.insert(m), "duplicate mnemonic {m}");
            assert!(
                m.chars()
                    .all(|c| c.is_ascii_uppercase() || c.is_ascii_digit()),
                "mnemonic {m} is not uppercase"
            );
        }
    }

    /// The complete spec §5 assignment, restated independently of the `ops!` table so a
    /// renumbering cannot slip through — it must edit this test, deliberately. (The
    /// round-trip tests above derive both sides from the same macro and so cannot catch
    /// a changed byte.)
    #[test]
    fn the_frozen_byte_table_matches_spec_section_5() {
        let table: [(Op, u8); 50] = [
            (Op::Move, 0x00),
            (Op::LoadK, 0x01),
            (Op::LoadKx, 0x02),
            (Op::LoadImm, 0x03),
            (Op::LoadI, 0x04),
            (Op::Add, 0x05),
            (Op::Sub, 0x06),
            (Op::Mul, 0x07),
            (Op::Div, 0x08),
            (Op::Quot, 0x09),
            (Op::Neg, 0x0A),
            (Op::AddI, 0x0B),
            (Op::NumEq, 0x10),
            (Op::NumLt, 0x11),
            (Op::NumLe, 0x12),
            (Op::Eq, 0x13),
            (Op::Eqv, 0x14),
            (Op::Test, 0x15),
            (Op::Jmp, 0x16),
            (Op::ExtraArg, 0x17),
            (Op::JmpIdx, 0x18),
            (Op::Cons, 0x19),
            (Op::Car, 0x1A),
            (Op::Cdr, 0x1B),
            (Op::SetCar, 0x1C),
            (Op::SetCdr, 0x1D),
            (Op::Cadr, 0x20),
            (Op::TypeP, 0x21),
            (Op::Call, 0x22),
            (Op::TailCall, 0x23),
            (Op::Return, 0x24),
            (Op::Return1, 0x25),
            (Op::Apply, 0x26),
            (Op::Closure, 0x27),
            (Op::GetUpval, 0x28),
            (Op::SetUpval, 0x29),
            (Op::GetGlobal, 0x30),
            (Op::SetGlobal, 0x31),
            (Op::GetLocalN, 0x32),
            (Op::CloseUpvals, 0x33),
            (Op::VecRef, 0x34),
            (Op::VecSet, 0x35),
            (Op::NewVec, 0x36),
            (Op::PrimCall, 0x37),
            (Op::CaptureCc, 0x38),
            (Op::WindPush, 0x39),
            (Op::WindPop, 0x3A),
            (Op::HandlerPush, 0x3B),
            (Op::HandlerPop, 0x3C),
            (Op::Raise, 0x3D),
        ];
        for (op, byte) in table {
            assert_eq!(op as u8, byte, "{op} is not at its frozen byte");
        }
        // The table above and Op::ALL must be the same 50 opcodes.
        let pinned: std::collections::HashSet<u8> = table.iter().map(|&(op, _)| op as u8).collect();
        assert_eq!(pinned.len(), Op::COUNT);
    }
}

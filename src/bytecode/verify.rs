//! The load-time verifier: proves a [`Proto`] tree cannot make the VM read out of bounds.
//!
//! The dispatch loop trusts what it executes — that is what makes it fast — so everything
//! it trusts is checked once, here, when a prototype is loaded. The rules:
//!
//! - every register operand names a register inside the frame's window, and the window
//!   itself is at most [`Proto::MAX_WINDOW`];
//! - every constant, child-prototype and upvalue index is in bounds;
//! - every static control transfer — `JMP`, `HANDLERPUSH`, and the skip of the comparison
//!   family — lands on a real instruction, and never on an `EXTRAARG`;
//! - `LOADKX` and `EXTRAARG` occur only as an adjacent pair;
//! - execution cannot fall off the end of the code: the last instruction is `RETURN`,
//!   `RETURN1`, `TAILCALL` or `JMP`;
//! - fields an opcode does not use are zero, so every instruction has exactly one encoding
//!   and an encoder bug cannot hide in a byte nothing reads yet;
//! - flag operands are 0 or 1, and `LOADIMM`'s operand is a real singleton ordinal;
//! - each child's [`UpvalDesc`] table captures only registers and upvalues its parent
//!   actually has, and the root prototype captures nothing at all.
//!
//! Two operand bytes are deliberately *not* constrained: `CADR`'s path and `TYPEP`'s type
//! selector, whose encodings are open questions 1 and 2 in `docs/project_plan.org`.
//!
//! # What passing does *not* prove — the VM's residual obligations
//!
//! - `JMPIDX`'s target is a register value, invisible here. The VM must check at
//!   execution time both that the computed target is in bounds *and* that it does not
//!   land on an `EXTRAARG` — the runtime counterpart of the static transfer rule.
//! - `GETGLOBAL`/`SETGLOBAL` key constants are index-checked only. Whether `K[Bx]` is
//!   actually a symbol needs the heap, which this pass deliberately does not take; the
//!   global-slot linker resolves the key at load time and must reject a non-symbol there.
//! - `PRIMCALL`'s C indexes the native-function table, an M3 artifact; until it exists
//!   the index is unchecked.

use crate::bytecode::op::Op;
use crate::bytecode::proto::{Proto, UpvalDesc};

use core::fmt;

/// Why a prototype failed verification, and where.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VerifyError {
    /// Which prototype, as a path label: the root's name (or `main`), then `.p<i>` per
    /// child index, e.g. `main.p0.p1`.
    pub proto: String,
    /// The offending instruction, when the failure is at one.
    pub pc: Option<usize>,
    /// What was wrong.
    pub kind: VerifyErrorKind,
}

impl fmt::Display for VerifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.pc {
            Some(pc) => write!(f, "proto {}, pc {:04}: {}", self.proto, pc, self.kind),
            None => write!(f, "proto {}: {}", self.proto, self.kind),
        }
    }
}

impl std::error::Error for VerifyError {}

/// The individual verification rules a [`Proto`] can break.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum VerifyErrorKind {
    #[error("byte {byte:#04x} is not an RBC-1 opcode")]
    UnknownOpcode { byte: u8 },
    #[error("register r{reg} is outside the window of {window}")]
    RegisterOutOfWindow { reg: u16, window: u8 },
    #[error("window of {window} exceeds the maximum of 250")]
    WindowTooLarge { window: u8 },
    #[error("{nparams} parameters (has_rest {has_rest}) do not fit the window of {window}")]
    ParamsExceedWindow {
        nparams: u8,
        has_rest: bool,
        window: u8,
    },
    #[error("constant k{index} is out of bounds ({len} constants)")]
    ConstOutOfBounds { index: u32, len: usize },
    #[error("child prototype p{index} is out of bounds ({len} children)")]
    ProtoOutOfBounds { index: u16, len: usize },
    #[error("upvalue u{index} is out of bounds ({len} upvalues)")]
    UpvalOutOfBounds { index: u8, len: usize },
    #[error("{imm} is not a singleton ordinal (0..=5)")]
    BadSingleton { imm: u16 },
    #[error("flag field {field} must be 0 or 1, got {value}")]
    BadFlag { field: &'static str, value: u8 },
    #[error("unused field {field} must be 0, got {value}")]
    NonzeroUnusedField { field: &'static str, value: u32 },
    #[error("jump target {target} is outside the code ({len} instructions)")]
    JumpOutOfBounds { target: i64, len: usize },
    #[error("control transfer lands on the EXTRAARG at pc {target:04}")]
    JumpOntoExtraArg { target: usize },
    #[error("a skip here could step past the end of the code")]
    SkipPastEnd,
    #[error("LOADKX is not followed by EXTRAARG")]
    LoadKxWithoutExtraArg,
    #[error("EXTRAARG is not preceded by LOADKX")]
    OrphanExtraArg,
    #[error("APPLY with B=1 has no list argument to spread")]
    ApplyNeedsList,
    #[error("the code is empty")]
    EmptyCode,
    #[error("span table has {spans} entries for {code} instructions (must be 0 or equal)")]
    SpanTableLengthMismatch { spans: usize, code: usize },
    #[error("execution can fall off the end: {mnemonic} is not RETURN/RETURN1/TAILCALL/JMP")]
    BadTerminator { mnemonic: &'static str },
    #[error("the root prototype declares {len} upvalues but has no parent to capture from")]
    RootHasUpvals { len: usize },
    #[error("upvalue u{index} captures parent local r{reg}, outside the parent window of {window}")]
    CaptureLocalOutOfBounds { index: usize, reg: u8, window: u8 },
    #[error("upvalue u{index} captures parent upvalue u{upval}, but the parent has {len}")]
    CaptureUpvalOutOfBounds { index: usize, upval: u8, len: usize },
}

/// Verify `root` and, recursively, every child prototype.
///
/// `root` is verified as a *root*: it may not declare upvalues, because there is no
/// enclosing frame for them to capture. Children's [`UpvalDesc`] tables are checked
/// against their parent.
///
/// ```
/// use ruse::bytecode::{verify, Insn, Op, Proto, VerifyErrorKind};
///
/// let mut fine = Proto {
///     code: vec![Insn::iabc(Op::Return1, 0, 0, 0)],
///     max_window: 1,
///     ..Proto::default()
/// };
/// assert!(verify(&fine).is_ok());
///
/// // r7 does not exist in a window of 1.
/// fine.code[0] = Insn::iabc(Op::Return1, 7, 0, 0);
/// let err = verify(&fine).unwrap_err();
/// assert_eq!(err.kind, VerifyErrorKind::RegisterOutOfWindow { reg: 7, window: 1 });
/// # assert_eq!(err.pc, Some(0));
/// ```
pub fn verify(root: &Proto) -> Result<(), VerifyError> {
    // An explicit worklist rather than Rust recursion: a hand-assembled prototype chain can
    // be arbitrarily deep, and the verifier is exactly the wrong place to overflow a stack.
    let mut work: Vec<(&Proto, Option<&Proto>, String)> = vec![(root, None, root_label(root))];

    while let Some((proto, parent, label)) = work.pop() {
        verify_one(proto, parent, &label)?;
        for (i, child) in proto.protos.iter().enumerate() {
            work.push((child, Some(proto), child_label(&label, i)));
        }
    }
    Ok(())
}

pub(crate) fn root_label(proto: &Proto) -> String {
    proto.name.clone().unwrap_or_else(|| "main".to_string())
}

/// Always the path, never the child's own name: a compiler names every `named let` body
/// `loop`, so names cannot locate a prototype uniquely — the position in the tree can.
pub(crate) fn child_label(parent: &str, index: usize) -> String {
    format!("{parent}.p{index}")
}

/// One instruction's checking context; every method returns the error already located.
struct Site<'a> {
    proto: &'a Proto,
    label: &'a str,
    pc: usize,
}

impl Site<'_> {
    fn fail(&self, kind: VerifyErrorKind) -> VerifyError {
        VerifyError {
            proto: self.label.to_string(),
            pc: Some(self.pc),
            kind,
        }
    }

    /// `reg` must be inside the window. Takes `u16` because compound operands like
    /// `A+B-1` can exceed a byte.
    fn reg(&self, reg: u16) -> Result<(), VerifyError> {
        if reg >= u16::from(self.proto.max_window) {
            return Err(self.fail(VerifyErrorKind::RegisterOutOfWindow {
                reg,
                window: self.proto.max_window,
            }));
        }
        Ok(())
    }

    fn flag(&self, field: &'static str, value: u8) -> Result<(), VerifyError> {
        if value > 1 {
            return Err(self.fail(VerifyErrorKind::BadFlag { field, value }));
        }
        Ok(())
    }

    fn zero(&self, field: &'static str, value: u32) -> Result<(), VerifyError> {
        if value != 0 {
            return Err(self.fail(VerifyErrorKind::NonzeroUnusedField { field, value }));
        }
        Ok(())
    }

    fn konst(&self, index: u32) -> Result<(), VerifyError> {
        if index as usize >= self.proto.consts.len() {
            return Err(self.fail(VerifyErrorKind::ConstOutOfBounds {
                index,
                len: self.proto.consts.len(),
            }));
        }
        Ok(())
    }

    /// A static control transfer to `pc + 1 + offset` must land on a real instruction,
    /// and not on an `EXTRAARG`.
    fn transfer(&self, offset: i64) -> Result<(), VerifyError> {
        let len = self.proto.code.len();
        let target = self.pc as i64 + 1 + offset;
        let Ok(target) = usize::try_from(target) else {
            return Err(self.fail(VerifyErrorKind::JumpOutOfBounds { target, len }));
        };
        if target >= len {
            return Err(self.fail(VerifyErrorKind::JumpOutOfBounds {
                target: target as i64,
                len,
            }));
        }
        if self.proto.code[target].opcode() == Some(Op::ExtraArg) {
            return Err(self.fail(VerifyErrorKind::JumpOntoExtraArg { target }));
        }
        Ok(())
    }

    /// The skip-next family: the not-skipped path executes `pc+1`, the skipped path
    /// executes `pc+2`, so both must exist and the skip target may not be an `EXTRAARG`.
    fn skip(&self) -> Result<(), VerifyError> {
        if self.pc + 2 >= self.proto.code.len() {
            return Err(self.fail(VerifyErrorKind::SkipPastEnd));
        }
        self.transfer(1)
    }

    /// The CALL-family argument block: callee (or first value) in `A`, then `count`-style
    /// `B`: `0` is open-ended, `1` is none, `n>=2` spans up to `A + n - 1`.
    fn arg_block(&self, a: u8, b: u8) -> Result<(), VerifyError> {
        self.reg(u16::from(a))?;
        if b >= 2 {
            self.reg(u16::from(a) + u16::from(b) - 1)?;
        }
        Ok(())
    }
}

fn verify_one(proto: &Proto, parent: Option<&Proto>, label: &str) -> Result<(), VerifyError> {
    let proto_err = |kind| VerifyError {
        proto: label.to_string(),
        pc: None,
        kind,
    };

    // ---------------------------------------------------------------- prototype metadata
    if proto.max_window > Proto::MAX_WINDOW {
        return Err(proto_err(VerifyErrorKind::WindowTooLarge {
            window: proto.max_window,
        }));
    }
    let regs_for_params = u16::from(proto.nparams) + u16::from(proto.has_rest);
    if regs_for_params > u16::from(proto.max_window) {
        return Err(proto_err(VerifyErrorKind::ParamsExceedWindow {
            nparams: proto.nparams,
            has_rest: proto.has_rest,
            window: proto.max_window,
        }));
    }

    match parent {
        None => {
            if !proto.upvals.is_empty() {
                return Err(proto_err(VerifyErrorKind::RootHasUpvals {
                    len: proto.upvals.len(),
                }));
            }
        }
        Some(parent) => {
            for (index, desc) in proto.upvals.iter().enumerate() {
                match *desc {
                    UpvalDesc::ParentLocal(reg) => {
                        if reg >= parent.max_window {
                            return Err(proto_err(VerifyErrorKind::CaptureLocalOutOfBounds {
                                index,
                                reg,
                                window: parent.max_window,
                            }));
                        }
                    }
                    UpvalDesc::ParentUpval(upval) => {
                        if usize::from(upval) >= parent.upvals.len() {
                            return Err(proto_err(VerifyErrorKind::CaptureUpvalOutOfBounds {
                                index,
                                upval,
                                len: parent.upvals.len(),
                            }));
                        }
                    }
                }
            }
        }
    }

    if proto.code.is_empty() {
        return Err(proto_err(VerifyErrorKind::EmptyCode));
    }

    // The span table is either absent or parallel to the code; a consumer indexing
    // `spans[pc]` on an error path must never be the one to find out otherwise.
    if !proto.spans.is_empty() && proto.spans.len() != proto.code.len() {
        return Err(proto_err(VerifyErrorKind::SpanTableLengthMismatch {
            spans: proto.spans.len(),
            code: proto.code.len(),
        }));
    }

    // ---------------------------------------------------------------- instructions
    for (pc, &insn) in proto.code.iter().enumerate() {
        let s = Site { proto, label, pc };
        let op = insn
            .opcode()
            .ok_or_else(|| s.fail(VerifyErrorKind::UnknownOpcode { byte: insn.op() }))?;
        let (a, b, c) = (insn.a(), insn.b(), insn.c());
        let ra = u16::from(a);
        let rb = u16::from(b);
        let rc = u16::from(c);

        match op {
            // ------------------------------------------------------ data movement
            Op::Move => {
                s.reg(ra)?;
                s.reg(rb)?;
                s.zero("C", rc.into())?;
            }
            Op::LoadK => {
                s.reg(ra)?;
                s.konst(insn.bx().into())?;
            }
            Op::LoadKx => {
                s.reg(ra)?;
                s.zero("Bx", insn.bx().into())?;
                match proto.code.get(pc + 1).map(|n| (n.opcode(), n.ax())) {
                    Some((Some(Op::ExtraArg), ax)) => s.konst(ax)?,
                    _ => return Err(s.fail(VerifyErrorKind::LoadKxWithoutExtraArg)),
                }
            }
            Op::LoadImm => {
                s.reg(ra)?;
                if insn.bx() > 5 {
                    return Err(s.fail(VerifyErrorKind::BadSingleton { imm: insn.bx() }));
                }
            }
            Op::LoadI => s.reg(ra)?,

            // ------------------------------------------------------ arithmetic
            Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Quot => {
                s.reg(ra)?;
                s.reg(rb)?;
                s.reg(rc)?;
            }
            Op::Neg => {
                s.reg(ra)?;
                s.reg(rb)?;
                s.zero("C", rc.into())?;
            }
            Op::AddI => {
                s.reg(ra)?;
                s.reg(rb)?;
                // sC is an arbitrary signed byte; nothing to range-check.
            }

            // ------------------------------------------------------ comparison (skip-next)
            Op::NumEq | Op::NumLt | Op::NumLe | Op::Eq | Op::Eqv => {
                s.reg(ra)?;
                s.reg(rb)?;
                s.flag("C", c)?;
                s.skip()?;
            }
            Op::Test => {
                s.reg(ra)?;
                s.zero("B", rb.into())?;
                s.flag("C", c)?;
                s.skip()?;
            }
            Op::TypeP => {
                s.reg(ra)?;
                s.zero("B", rb.into())?;
                // C is the type selector; its enum is open question 2, so any byte passes.
                s.skip()?;
            }

            // ------------------------------------------------------ control flow
            Op::Jmp => {
                // A=0 closes nothing; A>0 closes from register A-1, which must exist.
                if a > 0 {
                    s.reg(ra - 1)?;
                }
                s.transfer(insn.sbx().into())?;
            }
            Op::ExtraArg => {
                let paired = pc > 0 && proto.code[pc - 1].opcode() == Some(Op::LoadKx);
                if !paired {
                    return Err(s.fail(VerifyErrorKind::OrphanExtraArg));
                }
            }
            Op::JmpIdx => {
                // The offset in RB is a runtime value; only the registers are checkable.
                s.reg(ra)?;
                s.reg(rb)?;
                s.reg(rc)?;
            }

            // ------------------------------------------------------ pairs
            Op::Cons => {
                s.reg(ra)?;
                s.reg(rb)?;
                s.reg(rc)?;
            }
            Op::Car | Op::Cdr | Op::SetCar | Op::SetCdr => {
                s.reg(ra)?;
                s.reg(rb)?;
                s.zero("C", rc.into())?;
            }
            Op::Cadr => {
                s.reg(ra)?;
                s.reg(rb)?;
                // C is the fused-accessor path; its layout is open question 1.
            }

            // ------------------------------------------------------ calls and returns
            Op::Call => {
                s.arg_block(a, b)?;
                if c >= 2 {
                    s.reg(ra + rc - 2)?;
                }
            }
            Op::TailCall => {
                s.arg_block(a, b)?;
                s.zero("C", rc.into())?;
            }
            Op::Return => {
                match b {
                    // B=1 returns zero values; A carries no information, so it is zero.
                    1 => s.zero("A", ra.into())?,
                    0 => s.reg(ra)?,
                    _ => {
                        s.reg(ra)?;
                        s.reg(ra + rb - 2)?;
                    }
                }
                s.zero("C", rc.into())?;
            }
            Op::Return1 => {
                s.reg(ra)?;
                s.zero("B", rb.into())?;
                s.zero("C", rc.into())?;
            }
            Op::Apply => {
                if b == 1 {
                    return Err(s.fail(VerifyErrorKind::ApplyNeedsList));
                }
                s.arg_block(a, b)?;
                s.zero("C", rc.into())?;
            }

            // ------------------------------------------------------ closures and variables
            Op::Closure => {
                s.reg(ra)?;
                let index = insn.bx();
                if usize::from(index) >= proto.protos.len() {
                    return Err(s.fail(VerifyErrorKind::ProtoOutOfBounds {
                        index,
                        len: proto.protos.len(),
                    }));
                }
            }
            Op::GetUpval | Op::SetUpval => {
                s.reg(ra)?;
                if usize::from(b) >= proto.upvals.len() {
                    return Err(s.fail(VerifyErrorKind::UpvalOutOfBounds {
                        index: b,
                        len: proto.upvals.len(),
                    }));
                }
                s.zero("C", rc.into())?;
            }
            Op::GetGlobal | Op::SetGlobal => {
                s.reg(ra)?;
                s.konst(insn.bx().into())?;
            }
            Op::GetLocalN => {
                s.reg(ra)?;
                s.reg(rb)?;
                s.zero("C", rc.into())?;
            }
            Op::CloseUpvals => {
                s.reg(ra)?;
                s.zero("B", rb.into())?;
                s.zero("C", rc.into())?;
            }

            // ------------------------------------------------------ vectors and the bridge
            Op::VecRef | Op::VecSet | Op::NewVec => {
                s.reg(ra)?;
                s.reg(rb)?;
                s.reg(rc)?;
            }
            Op::PrimCall => {
                // C is the native-function index; the table it indexes is an M3 artifact,
                // so it cannot be bounds-checked yet.
                s.arg_block(a, b)?;
            }

            // ------------------------------------------------------ first-class control
            Op::CaptureCc => {
                s.reg(ra)?;
                s.zero("B", rb.into())?;
                s.zero("C", rc.into())?;
            }
            Op::WindPush => {
                s.reg(ra)?;
                s.reg(rb)?;
                s.zero("C", rc.into())?;
            }
            Op::WindPop | Op::HandlerPop => {
                s.zero("A", ra.into())?;
                s.zero("B", rb.into())?;
                s.zero("C", rc.into())?;
            }
            Op::HandlerPush => {
                s.reg(ra)?;
                s.transfer(insn.sbx().into())?;
            }
            Op::Raise => {
                s.reg(ra)?;
                s.flag("B", b)?;
                s.zero("C", rc.into())?;
            }
        }
    }

    // ---------------------------------------------------------------- termination
    // `RAISE` is deliberately absent from the terminator set: a continuable raise resumes
    // at the next instruction, so the compiler must place a real terminator after it.
    let last_pc = proto.code.len() - 1;
    let last = proto.code[last_pc];
    match last.opcode() {
        Some(Op::Return | Op::Return1 | Op::TailCall | Op::Jmp) => Ok(()),
        Some(op) => Err(VerifyError {
            proto: label.to_string(),
            pc: Some(last_pc),
            kind: VerifyErrorKind::BadTerminator {
                mnemonic: op.mnemonic(),
            },
        }),
        // Unreachable in practice: an unknown byte already failed above.
        None => Err(VerifyError {
            proto: label.to_string(),
            pc: Some(last_pc),
            kind: VerifyErrorKind::UnknownOpcode { byte: last.op() },
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::insn::Insn;
    use std::rc::Rc;

    fn ret1(a: u8) -> Insn {
        Insn::iabc(Op::Return1, a, 0, 0)
    }

    /// A window-1 prototype around the given code, valid unless the code is not.
    fn proto(code: Vec<Insn>) -> Proto {
        Proto {
            code,
            max_window: 1,
            ..Proto::default()
        }
    }

    fn kind_of(p: &Proto) -> VerifyErrorKind {
        verify(p).unwrap_err().kind
    }

    #[test]
    fn a_minimal_prototype_verifies() {
        assert_eq!(verify(&proto(vec![ret1(0)])), Ok(()));
    }

    #[test]
    fn an_unknown_opcode_byte_is_rejected() {
        // 0x40 is the first RBC-2 reservation; it must not execute in RBC-1.
        let p = proto(vec![Insn(0x40), ret1(0)]);
        assert_eq!(kind_of(&p), VerifyErrorKind::UnknownOpcode { byte: 0x40 });
    }

    #[test]
    fn compound_call_operands_are_range_checked_as_a_block() {
        // Window of 3: CALL r1 with 2 args touches r2 and r3, and r3 does not exist.
        let mut p = proto(vec![Insn::iabc(Op::Call, 1, 3, 1), ret1(0)]);
        p.max_window = 3;
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::RegisterOutOfWindow { reg: 3, window: 3 }
        );
        // One register more and the same code is fine.
        p.max_window = 4;
        assert_eq!(verify(&p), Ok(()));
    }

    #[test]
    fn result_blocks_are_range_checked_too() {
        // CALL r0 wanting 3 results writes r0..=r2 in a window of 2.
        let mut p = proto(vec![Insn::iabc(Op::Call, 0, 1, 4), ret1(0)]);
        p.max_window = 2;
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::RegisterOutOfWindow { reg: 2, window: 2 }
        );
    }

    #[test]
    fn the_window_is_capped_at_250() {
        let mut p = proto(vec![ret1(0)]);
        p.max_window = 251;
        assert_eq!(kind_of(&p), VerifyErrorKind::WindowTooLarge { window: 251 });
        p.max_window = 250;
        assert_eq!(verify(&p), Ok(()));
    }

    #[test]
    fn parameters_must_fit_the_window() {
        let mut p = proto(vec![ret1(0)]);
        p.nparams = 2;
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::ParamsExceedWindow {
                nparams: 2,
                has_rest: false,
                window: 1
            }
        );
        // A rest parameter needs one more register than nparams alone.
        p.nparams = 1;
        p.has_rest = true;
        assert!(matches!(
            kind_of(&p),
            VerifyErrorKind::ParamsExceedWindow { .. }
        ));
        p.max_window = 2;
        assert_eq!(verify(&p), Ok(()));
    }

    #[test]
    fn constant_indices_are_bounds_checked() {
        let p = proto(vec![Insn::iabx(Op::LoadK, 0, 3), ret1(0)]);
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::ConstOutOfBounds { index: 3, len: 0 }
        );
    }

    #[test]
    fn global_slot_names_are_constant_indices() {
        let p = proto(vec![Insn::iabx(Op::GetGlobal, 0, 0), ret1(0)]);
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::ConstOutOfBounds { index: 0, len: 0 }
        );
    }

    #[test]
    fn child_prototype_indices_are_bounds_checked() {
        let p = proto(vec![Insn::iabx(Op::Closure, 0, 0), ret1(0)]);
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::ProtoOutOfBounds { index: 0, len: 0 }
        );
    }

    #[test]
    fn upvalue_indices_are_bounds_checked() {
        let p = proto(vec![Insn::iabc(Op::GetUpval, 0, 0, 0), ret1(0)]);
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::UpvalOutOfBounds { index: 0, len: 0 }
        );
    }

    #[test]
    fn loadimm_takes_only_real_singleton_ordinals() {
        let p = proto(vec![Insn::iabx(Op::LoadImm, 0, 6), ret1(0)]);
        assert_eq!(kind_of(&p), VerifyErrorKind::BadSingleton { imm: 6 });
        for ordinal in 0..=5 {
            let p = proto(vec![Insn::iabx(Op::LoadImm, 0, ordinal), ret1(0)]);
            assert_eq!(verify(&p), Ok(()), "ordinal {ordinal} should be valid");
        }
    }

    #[test]
    fn comparison_flags_are_zero_or_one() {
        let p = proto(vec![
            Insn::iabc(Op::NumEq, 0, 0, 2),
            ret1(0),
            ret1(0),
            ret1(0),
        ]);
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::BadFlag {
                field: "C",
                value: 2
            }
        );
    }

    #[test]
    fn a_continuable_flag_above_one_is_rejected() {
        let p = proto(vec![Insn::iabc(Op::Raise, 0, 2, 0), ret1(0)]);
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::BadFlag {
                field: "B",
                value: 2
            }
        );
    }

    #[test]
    fn unused_fields_must_be_zero() {
        // Canonical encodings: a byte nothing reads yet is where an encoder bug hides.
        let p = proto(vec![Insn::iabc(Op::Move, 0, 0, 1), ret1(0)]);
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::NonzeroUnusedField {
                field: "C",
                value: 1
            }
        );
        // RETURN with B=1 returns no values, so its A carries no information either.
        let p = proto(vec![Insn::iabc(Op::Return, 1, 1, 0)]);
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::NonzeroUnusedField {
                field: "A",
                value: 1
            }
        );
    }

    #[test]
    fn jumps_stay_inside_the_code() {
        let p = proto(vec![Insn::iasbx(Op::Jmp, 0, 5), ret1(0)]);
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::JumpOutOfBounds { target: 6, len: 2 }
        );
        let p = proto(vec![Insn::iasbx(Op::Jmp, 0, -2), ret1(0)]);
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::JumpOutOfBounds { target: -1, len: 2 }
        );
        // A self-loop is in bounds.
        let p = proto(vec![Insn::iasbx(Op::Jmp, 0, -1)]);
        assert_eq!(verify(&p), Ok(()));
    }

    #[test]
    fn handler_targets_are_checked_like_jumps() {
        let p = proto(vec![Insn::iasbx(Op::HandlerPush, 0, 9), ret1(0)]);
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::JumpOutOfBounds { target: 10, len: 2 }
        );
    }

    #[test]
    fn nothing_may_jump_onto_an_extraarg() {
        let mut p = proto(vec![
            Insn::iasbx(Op::Jmp, 0, 1),
            Insn::iabx(Op::LoadKx, 0, 0),
            Insn::iax(Op::ExtraArg, 0),
            ret1(0),
        ]);
        p.consts = vec![crate::value::Value::TRUE];
        assert_eq!(kind_of(&p), VerifyErrorKind::JumpOntoExtraArg { target: 2 });
    }

    #[test]
    fn a_comparison_may_not_sit_at_the_end_of_the_code() {
        // Its skip would step to pc+2 == len, past the last instruction.
        let p = proto(vec![Insn::iabc(Op::Test, 0, 0, 0), ret1(0)]);
        assert_eq!(kind_of(&p), VerifyErrorKind::SkipPastEnd);
        // With a real landing pad it is fine.
        let p = proto(vec![Insn::iabc(Op::Test, 0, 0, 0), ret1(0), ret1(0)]);
        assert_eq!(verify(&p), Ok(()));
    }

    #[test]
    fn loadkx_and_extraarg_travel_only_as_a_pair() {
        let mut p = proto(vec![Insn::iabx(Op::LoadKx, 0, 0), ret1(0)]);
        p.consts = vec![crate::value::Value::TRUE];
        assert_eq!(kind_of(&p), VerifyErrorKind::LoadKxWithoutExtraArg);

        let p = proto(vec![Insn::iax(Op::ExtraArg, 0), ret1(0)]);
        assert_eq!(kind_of(&p), VerifyErrorKind::OrphanExtraArg);

        let mut p = proto(vec![
            Insn::iabx(Op::LoadKx, 0, 0),
            Insn::iax(Op::ExtraArg, 0),
            ret1(0),
        ]);
        p.consts = vec![crate::value::Value::TRUE];
        assert_eq!(verify(&p), Ok(()));

        // The pair's constant index is checked through EXTRAARG's 24 bits.
        p.code[1] = Insn::iax(Op::ExtraArg, 70_000);
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::ConstOutOfBounds {
                index: 70_000,
                len: 1
            }
        );
    }

    #[test]
    fn apply_must_have_a_list_to_spread() {
        let p = proto(vec![Insn::iabc(Op::Apply, 0, 1, 0), ret1(0)]);
        assert_eq!(kind_of(&p), VerifyErrorKind::ApplyNeedsList);
    }

    #[test]
    fn empty_code_is_rejected() {
        assert_eq!(kind_of(&proto(vec![])), VerifyErrorKind::EmptyCode);
    }

    #[test]
    fn a_partial_span_table_is_rejected() {
        use crate::span::Span;
        let mut p = proto(vec![ret1(0), ret1(0)]);
        p.spans = vec![Span::new(0, 1)];
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::SpanTableLengthMismatch { spans: 1, code: 2 }
        );
        // Absent and fully parallel are both fine.
        p.spans = vec![];
        assert_eq!(verify(&p), Ok(()));
        p.spans = vec![Span::new(0, 1), Span::new(1, 2)];
        assert_eq!(verify(&p), Ok(()));
    }

    #[test]
    fn execution_may_not_fall_off_the_end() {
        let p = proto(vec![Insn::iasbx(Op::LoadI, 0, 1)]);
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::BadTerminator { mnemonic: "LOADI" }
        );
        // RAISE is deliberately not a terminator: a continuable raise resumes after it.
        let p = proto(vec![Insn::iabc(Op::Raise, 0, 1, 0)]);
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::BadTerminator { mnemonic: "RAISE" }
        );
    }

    #[test]
    fn the_root_prototype_may_not_capture_anything() {
        let mut p = proto(vec![ret1(0)]);
        p.upvals = vec![UpvalDesc::ParentLocal(0)];
        assert_eq!(kind_of(&p), VerifyErrorKind::RootHasUpvals { len: 1 });
    }

    #[test]
    fn capture_descriptors_are_checked_against_the_parent() {
        let child = Proto {
            code: vec![ret1(0)],
            max_window: 1,
            upvals: vec![UpvalDesc::ParentLocal(5)],
            ..Proto::default()
        };
        let mut parent = proto(vec![Insn::iabx(Op::Closure, 0, 0), ret1(0)]);
        parent.protos = vec![Rc::new(child)];

        let err = verify(&parent).unwrap_err();
        assert_eq!(err.proto, "main.p0");
        assert_eq!(err.pc, None);
        assert_eq!(
            err.kind,
            VerifyErrorKind::CaptureLocalOutOfBounds {
                index: 0,
                reg: 5,
                window: 1
            }
        );
    }

    #[test]
    fn capturing_a_parent_upvalue_the_parent_does_not_have_is_rejected() {
        let child = Proto {
            code: vec![ret1(0)],
            max_window: 1,
            upvals: vec![UpvalDesc::ParentUpval(0)],
            ..Proto::default()
        };
        let mut parent = proto(vec![Insn::iabx(Op::Closure, 0, 0), ret1(0)]);
        parent.protos = vec![Rc::new(child)];
        assert_eq!(
            kind_of(&parent),
            VerifyErrorKind::CaptureUpvalOutOfBounds {
                index: 0,
                upval: 0,
                len: 0
            }
        );
    }

    #[test]
    fn jmp_that_closes_upvalues_needs_the_register_it_closes_from() {
        // A=3 closes from r2, which a window of 2 does not have.
        let mut p = proto(vec![Insn::iasbx(Op::Jmp, 3, -1)]);
        p.max_window = 2;
        assert_eq!(
            kind_of(&p),
            VerifyErrorKind::RegisterOutOfWindow { reg: 2, window: 2 }
        );
    }

    #[test]
    fn errors_locate_themselves() {
        let mut p = proto(vec![ret1(0), Insn::iabx(Op::LoadK, 0, 9), ret1(0)]);
        p.name = Some("fact".to_string());
        let err = verify(&p).unwrap_err();
        assert_eq!(err.pc, Some(1));
        assert_eq!(
            err.to_string(),
            "proto fact, pc 0001: constant k9 is out of bounds (0 constants)"
        );
    }
}

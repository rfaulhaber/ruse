//! RBC-1: the register bytecode the compiler emits and the VM executes.
//!
//! Specified in `ruse-bytecode-spec.md`; the frozen deviations from that draft are recorded
//! in `docs/project_plan.org`. Four pieces live here:
//!
//! - [`Insn`] — the 32-bit instruction word: spec §3 accessors and symmetric encoders;
//! - [`Op`] — the 50-opcode enum, whose discriminants are the frozen byte table;
//! - [`Proto`] — the compiled-function prototype, shared by `Rc`, holding code, constants,
//!   [`UpvalDesc`] capture tables, child prototypes and arity metadata;
//! - [`verify()`] — the load-time verifier that makes trusting a prototype sound.
//!
//! The disassembler is deliberately elsewhere (`crate::disasm`): it is a *consumer* of the
//! bytecode, on the same footing as the future VM, and nothing in here depends on how
//! instructions print. Tests assert against its output rather than raw bytes — the frozen
//! decision that keeps this module's byte table cheap to change.

pub mod insn;
pub mod op;
pub mod proto;
pub mod verify;

pub use insn::Insn;
pub use op::{Format, Op};
pub use proto::{Proto, UpvalDesc};
pub use verify::{VerifyError, VerifyErrorKind, verify};

//! Typed runtime errors: every way the VM can refuse to continue.
//!
//! The dispatch loop, the runtime functions in [`crate::rt`], and the native primitives
//! all return these rather than panicking — the lint wall (`unwrap_used`, `panic`) makes
//! "no user program can crash the VM" a build property instead of a review question. The
//! one thing an error may not do is lie: [`VmErrorKind::Internal`] exists so that a broken
//! VM invariant surfaces as itself, never as a misleading user-facing message.

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::span::Span;

/// Why execution stopped, plus the source span of the faulting instruction when the
/// prototype kept its debug info ([`Proto::spans`](crate::bytecode::Proto::spans)).
#[derive(Debug, Clone, PartialEq, Eq, Error, Diagnostic)]
#[error("{kind}")]
pub struct VmError {
    /// What went wrong.
    pub kind: VmErrorKind,
    /// Where in the source, when known.
    #[label("{kind}")]
    pub span: Option<SourceSpan>,
}

impl VmError {
    /// An error with no location yet; the dispatch loop attaches one on the way out.
    pub fn new(kind: VmErrorKind) -> Self {
        Self { kind, span: None }
    }

    /// Attach `span` unless a more precise one is already present. Runtime functions
    /// return spanless errors; the dispatch loop knows the faulting instruction and calls
    /// this exactly once, so the innermost location wins.
    #[must_use]
    pub fn with_span(mut self, span: Span) -> Self {
        if self.span.is_none() {
            self.span = Some(SourceSpan::new(span.start.into(), span.len()));
        }
        self
    }
}

impl From<VmErrorKind> for VmError {
    fn from(kind: VmErrorKind) -> Self {
        Self::new(kind)
    }
}

/// The individual failure modes.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum VmErrorKind {
    #[error("unbound variable `{name}`")]
    UnboundVariable { name: String },
    #[error("{op}: expected {expected}, got {got}")]
    WrongType {
        op: &'static str,
        expected: &'static str,
        got: &'static str,
    },
    #[error("cannot call a {got} as a procedure")]
    NotCallable { got: &'static str },
    #[error("{name}: expected {expected} argument(s), got {got}")]
    WrongArity {
        name: String,
        expected: String,
        got: usize,
    },
    #[error("call stack overflow ({limit} frames); deep recursion must be in tail position")]
    StackOverflow { limit: usize },
    #[error("vector index {index} is out of bounds for length {len}")]
    IndexOutOfBounds { index: i64, len: usize },
    #[error("output failed: {message}")]
    Io { message: String },
    /// [`Vm::execute`](crate::vm::Vm::execute) refused the prototype at the trust
    /// boundary: the load-time verifier found it malformed, or a residual obligation
    /// the verifier documents as the VM's (a non-symbol global key) failed at run time.
    /// Reachable only with hand-assembled bytecode — the compiler's output always
    /// verifies and always emits symbol keys.
    #[error("prototype rejected: {message}")]
    Rejected { message: String },
    /// A native-function reference named no entry in this VM's table: a `PRIMCALL` index
    /// the verifier deliberately leaves to the VM, or a
    /// [`NativeProc`](crate::value::object::NativeProc) minted against a different VM's
    /// table. Malformed input, not a ruse bug.
    #[error("native-function index {index} names no entry in this VM's primitive table")]
    UnknownNative { index: u32 },
    /// An opcode or calling-convention feature whose milestone has not landed yet.
    /// Typed rather than `unreachable!` so a hand-assembled prototype degrades into a
    /// diagnostic instead of a crash.
    #[error("{what} is not implemented until {milestone}")]
    Unimplemented {
        what: &'static str,
        milestone: &'static str,
    },
    /// A VM invariant the verifier or the dispatch loop should have made impossible.
    /// Reaching this is a bug in ruse, not in the user's program, and the message says so.
    #[error("internal VM error (this is a ruse bug): {detail}")]
    Internal { detail: String },
}

impl VmErrorKind {
    /// Shorthand used by the dispatch loop and natives.
    pub(crate) fn err<T>(self) -> Result<T, VmError> {
        Err(VmError::new(self))
    }
}

/// Everything that can go wrong between source text and a value: the umbrella error of
/// [`Vm::eval_str`](crate::vm::Vm::eval_str). Each stage's diagnostic passes through
/// transparently, so miette renders the innermost report.
#[derive(Debug, Error, Diagnostic)]
pub enum RuseError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Parse(#[from] crate::parser::ParseError),
    #[error(transparent)]
    #[diagnostic(transparent)]
    Compile(#[from] crate::compiler::CompileError),
    #[error(transparent)]
    #[diagnostic(transparent)]
    Vm(#[from] VmError),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn errors_render_their_kind() {
        let e = VmError::new(VmErrorKind::UnboundVariable {
            name: "frobnicate".into(),
        });
        assert_eq!(e.to_string(), "unbound variable `frobnicate`");
        assert_eq!(e.span, None);
    }

    #[test]
    fn the_innermost_span_wins() {
        let e = VmError::new(VmErrorKind::StackOverflow { limit: 8 })
            .with_span(Span::new(3, 7))
            .with_span(Span::new(0, 100));
        assert_eq!(e.span, Some(SourceSpan::new(3.into(), 4)));
    }
}

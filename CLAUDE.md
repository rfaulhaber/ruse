# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Ruse is "Ryan's Useful Scheme Experiment" - a set of R7RS Scheme implementations written in Rust. This is a language interpreter/compiler project implementing the R7RS specification.

## Architecture

Standard Cargo layout — sources live in `src/`:
- `src/main.rs` - Binary entry point (`ruse` executable); dispatches to the REPL or file evaluation
- `src/lib.rs` - Library crate root; re-exports the public API
- `src/lexer.rs` - Hand-written lexer producing `Token`s with byte spans
- `src/parser.rs` - Recursive-descent `Parser`, producing the `Expr` AST
- `src/ast.rs` - `Expr` s-expression AST, span-carrying
- `src/span.rs` - `Span` / `SourceFile` source-location utilities
- `src/value/` - The runtime value representation
  - `src/value/layout.rs` - **The memory map.** Every NaN-box mask, tag constant and heap-object
    field offset is defined here exactly once, with `const` assertions enforcing the layout
  - `src/value/mod.rs` - `Value`, the NaN-boxed 64-bit word
  - `src/value/object.rs` - The `repr(C)` heap objects (including `Closure`) and the
    `HeapObject` trait
- `src/gc/` - The heap and collector
  - `src/gc/mod.rs` - `Heap`: allocation, symbol interning, mark-sweep, `Drop`-on-sweep
  - `src/gc/trace.rs` - The `Trace` root trait and the grey worklist `Tracer`
  - `src/gc/handle.rs` - The pin shadow stack for native-procedure temporaries
- `src/bytecode/` - The RBC-1 bytecode layer (M2)
  - `src/bytecode/op.rs` - **The frozen opcode table.** `Op` (`repr(u8)`), 50 opcodes; the
    discriminants are the byte assignments, defined nowhere else
  - `src/bytecode/insn.rs` - `Insn(u32)`: spec §3.1 accessors and the symmetric
    `iabc`/`iabx`/`iasbx`/`iax` encoders
  - `src/bytecode/proto.rs` - `Proto` (code, constants, `UpvalDesc` capture tables, child
    protos, arity/window metadata, provisional span table), shared by `Rc`
  - `src/bytecode/verify.rs` - The load-time verifier; everything the VM will trust is
    checked here once
- `src/vm/` - The register VM (M3)
  - `src/vm/mod.rs` - `Vm`: the flat register file, `Frame` stack, `match`-dispatch loop,
    calling convention (`CALL`/`TAILCALL`/`RETURN`), native invocation, and the
    `eval_str`/`eval_expr`/`execute` public API
  - `src/vm/error.rs` - `VmError`/`VmErrorKind` (typed, miette-rendering, span-carrying)
    and the `RuseError` umbrella
  - `src/vm/globals.rs` - `Globals`: the append-only stable-slot global table, plus the
    pristine-builtin flag that licenses primitive inlining
  - `src/vm/roots.rs` - The VM's `Trace` root set and its one safepoint — the sole caller
    of the unsafe `Heap::collect`
- `src/rt/` - Opcode and primitive semantics as standalone functions (habit 2: dispatch
  arms decode operands and call in here; nothing semantic lives inline in the loop)
  - `src/rt/arith.rs` - The M3 numeric slice (fixnum/bignum/flonum; M5 replaces the insides)
  - `src/rt/pairs.rs`, `src/rt/vectors.rs` - Pair and vector opcode semantics
  - `src/rt/equal.rs` - `eqv?` and the worklist-driven non-cyclic `equal?`
  - `src/rt/write.rs` - The printer: `display`/`write` styles, worklist-driven
  - `src/rt/prims.rs` - The `PRIMCALL` native-function table (`PrimTable`, `NativeCtx`)
    and the M3 primitive set, each also installed as a `NativeProc` global
- `src/compiler/` - The M3 compiler (decision C: no CPS, no ANF)
  - `src/compiler/ir.rs` - Lowering `Expr` → typed Core IR; owns syntax, form arities, and
    keyword shadowing (a lexical binding hides a special form)
  - `src/compiler/emit.rs` - Destination-register + tail-flag codegen over a Lua-style
    `Func` register allocator; primitive inlining, `ADDI` peephole, jump patching
- `src/disasm.rs` - The disassembler; its text output is the frozen test surface for all
  bytecode (tests snapshot it and never assert on raw bytes)
- `src/cli.rs` - clap-derived argument parser
- `src/repl.rs` - The REPL: parse → eval → `write` each form's value
- `tests/gc_drop.rs` - Global-allocator proof that sweeping runs `Drop`, not just `dealloc`
- `tests/disasm.rs` - `insta` snapshots of the derived spec §6 compilations plus
  all-opcode and malformed-input listings (snapshots in `tests/snapshots/`)
- `tests/compile.rs` - `insta` snapshots of *compiler* output, as disassembly text
- `tests/eval.rs` - End-to-end tests: the M3 exit criteria in executable form (constant
  frame-depth tail calls, bignum promotion, safepoint survival, typed errors)
- `tests/r7rs_progress.rs` - The incremental, error-tolerant conformance driver over the
  suite: splits top-level forms textually, evaluates what compiles, tallies a score
- `tests/r7rs_suite/r7rs.scm` - R7RS compliance test suite (driven by `r7rs_progress`)
- `ruse-bytecode-spec.md` - **RBC-1**, the target bytecode ISA (Lua-style register VM); the implementation goal
- Project uses Nix flakes for the development environment

### Current status

M0–M4 are done: source text goes reader → compiler → verifier → dispatch loop → value,
end to end, with real closures. The language slice is `lambda` (fixed, rest-only and
dotted parameter lists), `define` (top-level and internal-as-`letrec*`), `if`, `quote`,
`quasiquote` with depth tracking, `set!` (locals, captures and globals), `begin`, the
whole `let` family (`let`/`let*`/`letrec`/`letrec*`/named `let`/`do`/single-value
`let-values`), `and`/`or`/`when`/`unless`/`cond`/`case`, and ~37 native primitives.
Upvalues are Lua-style open→closed cells (`src/value/object.rs::UpvalueCell`, the VM's
open list in `src/vm/mod.rs`); `TAILCALL` reuses frames *and* closes the reused frame's
cells first, so tail recursion — named-`let` and `do` loops included — runs in constant
space with per-iteration captures independent. Fixnum overflow promotes to bignums, and
collections happen only at the dispatch loop's safepoint with the frame windows plus the
open-upvalue list as the root set. Still to come: the numeric tower (M5), reader breadth
(M6), first-class control and multiple values (M7), hygienic macros and libraries (M8),
stdlib/ports/conformance (M9). Opcodes owned by those milestones execute as typed
`Unimplemented` errors, never `unreachable!`. The goal is a bytecode-compiled R7RS
implementation targeting the register VM described in `ruse-bytecode-spec.md`.

### Bytecode invariants (M2)

- **The opcode byte table is frozen** as the `Op` discriminants in `src/bytecode/op.rs`
  (the spec §5 grouped table: `0x00`–`0x3D` with aligned gaps, `0x40`–`0x45` reserved for
  RBC-2). Renumbering is cheap *only* because tests key on disassembly text — never assert
  on raw instruction words.
- **Canonical encodings**: the skip-family `k` flag always lives in field C (including
  `TEST`); fields an opcode does not use must encode as zero; the verifier enforces both.
- **`LOADIMM`'s operand is the `src/value/layout.rs` singleton ordinal** (0=undefined
  1=unspecified 2=`'()` 3=eof 4=`#f` 5=`#t`). There is no second table.
- **Upvalue capture is data, not code**: `CLOSURE` reads the child `Proto`'s `UpvalDesc`
  table (Lua 5.4's pattern); there are no trailing pseudo-instructions.
- **`CADR`'s path byte and `TYPEP`'s selector are deliberately unfrozen** (plan open
  questions 1–2); the verifier passes their C field through unchecked until VM and
  compiler freeze them jointly.
- **A `Proto` is not a heap object.** It is immutable, `Rc`-shared, and acyclic by
  construction; a `Closure` (or, in M3, the VM's root set) is what keeps its constants
  alive, by tracing the whole prototype tree via `Proto::trace_values`.
- The verifier is the trust boundary: the M3 dispatch loop may assume any verified
  prototype's static operands are in bounds, but `JMPIDX`'s computed target is a runtime
  value and must be bounds-checked at execution time.

### Execution invariants (M3)

- **`Heap::collect` is `unsafe fn`** — the ratified answer to the safe-versus-unsafe
  heap-access contract: the *invalidating* operation carries the obligation (the
  `Vec::set_len` shape). The VM's one safepoint (`src/vm/roots.rs`) is its one caller
  inside the interpreter; the contract is that every `Value` read afterwards was
  reachable from the roots, the interner, or the pin stack at the moment of the call.
- **Register windows are complete root sets by construction.** Every register in a
  frame's `[base, base + max_window)` is cleared to `undefined` at frame entry, so the
  root walk traces whole windows blindly; stale words from popped deeper frames sit only
  above every live window, unreported and unread. Collections happen only at the
  dispatch-loop safepoint; natives run entirely between safepoints and cannot trigger one.
- **Opcode semantics live in `src/rt/`, not in match arms** (habit 2). Dispatch arms
  decode operands, call one `rt::` function, and store the result — nothing more.
- **The dispatch loop never holds a `&mut` slice across a call**: registers are read out
  as `Copy` values by `usize` index and written back after the runtime call returns.
- **Global access is one indirection** through `Globals`' append-only slot vector
  (symbol → slot → value); `GETGLOBAL` never resolves to a direct address (R7RS §5.4).
- **Primitive inlining is licensed, not assumed**: `(+ a b)` compiles to `ADD` (and
  `display` to `PRIMCALL`) only while the name is lexically unbound *and* still the
  pristine boot-time global (`Globals::is_pristine_builtin`). Redefinition revokes the
  licence for every later-compiled form; snapshot `plus_redefined` pins this. So the
  revocation sequences *inside* a top-level `begin` too (R7RS §5.1), `Vm::eval_expr`
  splices one into separate compilation units before compiling.
- **A `NativeProc` heap value names an entry in the per-VM `PrimTable`** — the table owns
  the function pointer and arity, the object just points. `CALL`/`TAILCALL` invoke
  natives framelessly; `PRIMCALL C` indexes the same table, with the index checked at
  execution time (the verifier has no VM to check it against). Table order is not frozen
  while nothing serializes bytecode.
- **Errors are values**: every failure path in the VM, the runtime and the natives is a
  typed `VmErrorKind` (spans attached from `Proto.spans` at the faulting instruction).
  Milestone-future opcodes return `Unimplemented`; a broken VM invariant returns
  `Internal` and says it is a ruse bug. No `unwrap`, no `unreachable!` — the lint wall
  makes this a build property.

### Upvalue invariants (M4)

- **An open upvalue names its register by absolute index, never by pointer** — the
  register file is a `Vec` that reallocates. `UpvalueCell.location` is `Some(abs)` while
  open; closing copies the register's value in and clears it. The VM's open list
  (`VmState::open_upvals`) maps index → cell and **is part of the root set**: a cell must
  outlive every closure sharing it, because closing writes to it.
- **Every close point is covered**: explicit `CLOSEUPVALS`/closing `JMP` (emitted only
  where a captured scope falls through non-tail), `RETURN`/`RETURN1` and `TAILCALL`
  (before the argument slide) for frame exit, and `Vm::execute`'s teardown for error
  unwinds. No open cell may ever survive its register's extent — that is the
  use-after-free the design exists to prevent.
- **`undefined` reaching `GETUPVAL` is the letrec\* black hole** and errors as
  `UninitializedVariable`; the compiler statically rejects same-function forward
  references (`PrematureReference`), so the runtime check only fires through captures.
- **Rest arguments are call-entry metadata, not an opcode**: `CALL`/`TAILCALL` cons the
  extras into a list placed in `R[nparams]` after the callee window is sized.

### Value and GC invariants (M1)

- `Value` is one 64-bit NaN-boxed word. Doubles are the identity encoding; everything else
  hides under a 3-bit tag in the negative-quiet-NaN region. **Every NaN is canonicalized on
  the way in** — without that, x86-64's indefinite QNaN decodes as a tagged value.
- A `Value` is `!Send`/`!Sync`: it can hold a raw pointer into a thread-local `Heap`.
- `PartialEq` on `Value` is bitwise, which is R7RS `eq?`. It is **not** `eqv?` or `equal?`.
- **Allocation never collects.** A precise collector can only free what a root reports, so a
  collection inside `cons` would reap the caller's un-rooted temporaries. `Heap::collect` is
  called at explicit safepoints; `Heap::should_collect` says when one is due. Native code
  that allocates twice in a row pins across the gap (`src/gc/handle.rs`).
- Roots are declared through the `unsafe trait Trace`. Missing a field there is a
  use-after-free — that is why the trait is `unsafe`.
- Interned symbols are permanent roots, so `eq?` on symbols stays pointer identity.
- Heap objects are reached through `Heap::get`/`get_mut`. The heap borrow gives the returned
  *reference* a correct lifetime — it cannot be held across an allocation or a collection.
  It does **not** prove the `Value` is live: a `Value` is `Copy` with no lifetime, so one
  held across a collection that did not reach it is dangling. Debug builds check that
  against a registry of owned addresses; the permanent fix is an open question in the plan.
- The `GcHeader` tag byte is crate-private and `HeapObject` is sealed. Both guard the same
  thing: the collector decides what an untyped pointer is by reading that tag, so writing it
  from outside would make the sweep reconstruct the wrong `Box`.

## Development Commands

### Build and Run
- `cargo build` - Build the project
- `cargo run` - Run the ruse binary
- `cargo check` - Check code without building

### Testing
- `cargo test` - Run standard tests
- `cargo nextest run` - Run tests with nextest (available in dev environment)
- Disassembly is snapshot-tested with `insta` (`tests/disasm.rs`, snapshots in
  `tests/snapshots/`). After an intentional output change, regenerate with
  `INSTA_UPDATE=always cargo test --test disasm` and review the diff; snapshots are
  committed, so CI and `nix build` fail on drift

### Code Quality
- `cargo clippy` - Run linting (clippy available in dev environment)
- `cargo fmt` - Format code

### Unsafe code and Miri

`unsafe_code` is **denied for the whole package** in `Cargo.toml`. Five files opt out, and
adding an `unsafe` block anywhere else is a build error rather than a review question:

| File | Why |
|---|---|
| `src/gc/mod.rs` | the collector: raw pointers, tag dispatch, `Box::from_raw` on sweep |
| `src/gc/trace.rs` | the grey worklist, and the `unsafe impl Trace` forwarding impls |
| `src/value/object.rs` | declarations only — `unsafe impl HeapObject`, no unsafe operations |
| `src/vm/roots.rs` | the VM's `unsafe impl Trace` root set, and the safepoint that calls the unsafe `Heap::collect` |
| `tests/gc_drop.rs` | a `GlobalAlloc` implementation is unsafe by definition |

`src/value/mod.rs`, `src/value/layout.rs` and `src/gc/handle.rs` contain no `unsafe` at all;
the latter two say so with `#![forbid(unsafe_code)]`, which cannot be overridden from within.

`clippy::undocumented_unsafe_blocks` is denied, so every `unsafe` block and every
`unsafe impl` must carry a `// SAFETY:` comment justifying it. The comment must begin
literally `SAFETY:` for the lint to see it.

- `nix run .#miri` - Run the test suite under Miri, in **both** aliasing models (Stacked
  Borrows and Tree Borrows), with leak checking on. Extra arguments are forwarded, so
  `nix run .#miri -- gc::tests::cycles` filters. This is what CI's gating Miri job runs.

Miri needs nightly, pinned in `nightly-toolchain.toml`; `flake.nix` and the CI job both read
that file, so a local run and CI use the same compiler. Bump it deliberately alongside the
`rust-overlay` input in `flake.lock`.

Miri reports one *integer-to-pointer cast* advisory at `Value::header_ptr`. That is inherent
to NaN-boxing — strict provenance cannot express packing a pointer into 48 bits of an
integer — so the code uses `expose_provenance`/`with_exposed_provenance_mut` rather than bare
`as` casts, which keeps everything downstream of the cast checkable. `-Zmiri-permissive-provenance`
silences the advisory; do not "fix" it by reaching for strict provenance.

Several tests are Miri-aware: the long-list/deep-chain tests in `gc`, `rt::equal` and
`rt::write` shrink their sizes under `cfg!(miri)` (the full sizes would take hours
interpreted), and the test that deliberately `mem::forget`s a pin scope is
`#[cfg_attr(miri, ignore)]` because Miri's leak checker is correct to flag it.

### Nix Integration
- `nix develop` - Enter development shell with all dependencies
- `nix build` - Build the project using Nix
- Development shell includes: Rust toolchain, clippy, rust-analyzer, cargo-nextest

## R7RS Implementation Notes

The project targets the R7RS Scheme specification, compiled to the RBC-1 register
bytecode VM in `ruse-bytecode-spec.md`. The suite in `tests/r7rs_suite/r7rs.scm` is the
conformance target, driven incrementally by `tests/r7rs_progress.rs`: the driver splits
the file into top-level forms textually (the lexer cannot read all of it yet), evaluates
what compiles, skips what errors, and gates on the passing-test tally never going down.

### Roadmap & frozen decisions

The phased implementation plan lives in **`docs/project_plan.org`** (milestones M0–M10, the
eight architecture decisions, and the open spec questions). It supersedes `docs/ROADMAP.md`,
which is kept for the full rationale tables. Ratified decisions that supersede the spec where
they conflict:

- **The opcode set is frozen at 50 real opcodes**, `0x40`–`0x45` reserved for RBC-2. The
  spec's "56 opcodes / nine groups" prose is inconsistent with its own §4 enumeration
  (ten groups, 50 opcodes); §4 is authoritative. Test against disassembly text, not raw
  bytes, so the byte table stays cheap to renumber.
- **R7RS semantics are followed strictly**: exact-rational `DIV` (`(/ 1 3)` ⇒ `1/3`);
  `with-exception-handler` is non-transferring so `raise-continuable` can return (only
  `guard` transfers control — the spec's §4.10 `HANDLERPUSH` control-transfer wording is
  guard-only); inexact `(/ 1.0 0.0)` ⇒ `+inf.0`.

## Error Reporting and Source Tracking

The lexer and parser include comprehensive source span tracking with professional-grade error reporting:

- All tokens and AST nodes track their source spans (byte ranges)
- Errors include precise source locations with beautiful formatting via **miette**
- `SourceFile` provides utilities for line/column mapping and span slicing
- Rich diagnostic output with syntax highlighting, labels, and helpful suggestions
- Spans track byte positions for accurate multi-line error reporting

### Error Types (using thiserror + miette)
- `LexError` - Lexical analysis errors with diagnostic labels and help text
- `ParseError` - Parser errors with contextual information
- Both implement miette's `Diagnostic` trait for rich error display
- Professional error messages with source code highlighting and precise span labeling

### Usage Example
```rust
use ruse::Parser;
use miette::{NamedSource, Result};

fn parse_file(content: &str, filename: &str) -> Result<()> {
    Parser::parse_from_str(content)
        .map_err(|e| miette::Report::new(e)
            .with_source_code(NamedSource::new(filename, content)))?;
    Ok(())
}
```
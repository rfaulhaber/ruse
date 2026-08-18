# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Ruse is "Ryan's Useful Scheme Experiment" - a set of R7RS Scheme implementations written in Rust. This is a language interpreter/compiler project implementing the R7RS specification.

## Architecture

Standard Cargo layout — sources live in `src/`:
- `src/main.rs` - Binary entry point (`ruse` executable); dispatches to the REPL or (TODO) file evaluation
- `src/lib.rs` - Library crate root; re-exports the public API
- `src/lexer.rs` - Hand-written lexer producing `Token`s with byte spans
- `src/parser.rs` - Recursive-descent `Parser`, producing the `Expr` AST
- `src/ast.rs` - `Expr` s-expression AST, span-carrying
- `src/span.rs` - `Span` / `SourceFile` source-location utilities
- `src/value/` - The runtime value representation
  - `src/value/layout.rs` - **The memory map.** Every NaN-box mask, tag constant and heap-object
    field offset is defined here exactly once, with `const` assertions enforcing the layout
  - `src/value/mod.rs` - `Value`, the NaN-boxed 64-bit word
  - `src/value/object.rs` - The `repr(C)` heap objects and the `HeapObject` trait
- `src/gc/` - The heap and collector
  - `src/gc/mod.rs` - `Heap`: allocation, symbol interning, mark-sweep, `Drop`-on-sweep
  - `src/gc/trace.rs` - The `Trace` root trait and the grey worklist `Tracer`
  - `src/gc/handle.rs` - The pin shadow stack for native-procedure temporaries
- `src/cli.rs` - clap-derived argument parser
- `src/repl.rs` - Parse-only REPL (no evaluation yet)
- `src/vm.rs` - Register-VM module (currently an empty placeholder)
- `tests/gc_drop.rs` - Global-allocator proof that sweeping runs `Drop`, not just `dealloc`
- `tests/r7rs_suite/r7rs.scm` - R7RS compliance test suite (not yet wired into the build)
- `ruse-bytecode-spec.md` - **RBC-1**, the target bytecode ISA (Lua-style register VM); the implementation goal
- Project uses Nix flakes for the development environment

### Current status

Reader plus runtime foundation (M0–M1 complete). The lexer and parser produce an `Expr`
AST; `Value` and the collector exist underneath, but nothing connects them yet. There is
still no evaluator, bytecode compiler, VM, numeric tower, macro expander, or standard
library. The goal is a bytecode-compiled R7RS implementation targeting the register VM
described in `ruse-bytecode-spec.md`.

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

### Code Quality
- `cargo clippy` - Run linting (clippy available in dev environment)
- `cargo fmt` - Format code

### Nix Integration
- `nix develop` - Enter development shell with all dependencies
- `nix build` - Build the project using Nix
- Development shell includes: Rust toolchain, clippy, rust-analyzer, cargo-nextest

## R7RS Implementation Notes

The project targets the R7RS Scheme specification, compiled to the RBC-1 register
bytecode VM in `ruse-bytecode-spec.md`. The suite in `tests/r7rs_suite/r7rs.scm` is the
conformance target (not yet wired into the build; it is all-or-nothing as written).

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
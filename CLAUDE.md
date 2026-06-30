# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Ruse is "Ryan's Useful Scheme Experiment" - a set of R7RS Scheme implementations written in Rust. This is a language interpreter/compiler project implementing the R7RS specification.

## Architecture

Standard Cargo layout — sources live in `src/`:
- `src/main.rs` - Binary entry point (`ruse` executable); dispatches to the REPL or (TODO) file evaluation
- `src/lib.rs` - Library crate root; re-exports the public API
- `src/lexer.rs` - Hand-written lexer producing `Token`s with byte spans
- `src/parser.rs` - Recursive-descent `Parser` plus a `StreamingParser`, producing the `Expr` AST
- `src/ast.rs` - `Expr` s-expression AST, span-carrying
- `src/span.rs` - `Span` / `SourceFile` source-location utilities
- `src/cli.rs` - clap-derived argument parser
- `src/repl.rs` - Parse-only REPL (no evaluation yet)
- `src/vm.rs` - Register-VM module (currently an empty placeholder)
- `tests/r7rs_suite/r7rs.scm` - R7RS compliance test suite (not yet wired into the build)
- `ruse-bytecode-spec.md` - **RBC-1**, the target bytecode ISA (Lua-style register VM); the implementation goal
- Project uses Nix flakes for the development environment

### Current status

Frontend-only: the lexer and parser produce an AST and stop. There is no evaluator,
bytecode compiler, value representation, runtime, GC, numeric tower, macro expander, or
standard library yet. The goal is a bytecode-compiled R7RS implementation targeting the
register VM described in `ruse-bytecode-spec.md`.

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

The phased implementation plan lives in **`docs/ROADMAP.md`** (milestones M0–M9, the eight
architecture decisions, and the open spec questions). Ratified decisions that supersede the
spec where they conflict:

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
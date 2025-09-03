# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Ruse is "Ryan's Useful Scheme Experiment" - a set of R7RS Scheme implementations written in Rust. This is a language interpreter/compiler project implementing the R7RS specification.

## Architecture

The project has a simple structure:
- `main.rs` - Binary entry point (`ruse` executable)
- `lib.rs` - Library crate with core functionality
- `tests/r7rs_suite/` - Contains R7RS compliance test suite
- Project uses Nix flakes for development environment setup

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

The project implements the R7RS Scheme specification. The test suite in `tests/r7rs_suite/r7rs.scm` contains comprehensive compliance tests for the R7RS standard.

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
use ruse::{Parser, NamedSource};
use miette::Result;

fn parse_file(content: &str, filename: &str) -> Result<()> {
    Parser::parse_from_str(content)
        .map_err(|e| miette::Report::new(e)
            .with_source_code(NamedSource::new(filename, content)))?;
    Ok(())
}
```
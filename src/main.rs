// A CLI/REPL binary legitimately writes to stdout/stderr; the `print_*` restriction
// lints exist to keep the *library* print-free, so they are allowed here at the bin root.
#![allow(clippy::print_stdout, clippy::print_stderr)]

use clap::Parser as _;
use miette::{IntoDiagnostic, NamedSource};
use ruse::Vm;

mod cli;
mod repl;

fn main() -> miette::Result<()> {
    let args = cli::Args::parse();

    match args.file {
        None => repl::start_repl()?,
        Some(path) => {
            let source = fs_err::read_to_string(&path).into_diagnostic()?;
            let mut vm = Vm::new();
            if let Err(e) = vm.eval_str(&source) {
                return Err(miette::Report::new(e).with_source_code(NamedSource::new(path, source)));
            }
        }
    }

    Ok(())
}

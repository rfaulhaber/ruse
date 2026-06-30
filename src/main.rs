// A CLI/REPL binary legitimately writes to stdout/stderr; the `print_*` restriction
// lints exist to keep the *library* print-free, so they are allowed here at the bin root.
#![allow(clippy::print_stdout, clippy::print_stderr)]

use clap::Parser;

mod cli;
mod repl;

fn main() -> miette::Result<()> {
    let args = cli::Args::parse();

    if args.file.is_none() {
        repl::start_repl()?;
    } else {
        println!("TODO: eval a file!");
    }

    Ok(())
}

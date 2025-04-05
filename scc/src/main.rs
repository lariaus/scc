use std::{path::PathBuf, process::exit};

use clap::Parser;
use scclib::scc_compiler::{OutputTarget, SCCCompiler, SCCCompilerOptions, SRCCResult};

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Input file path
    input: PathBuf,

    #[arg(long, action)]
    emit_cast: bool,

    #[arg(long, action)]
    emit_sir: bool,

    #[arg(long, action)]
    emit_lir: bool,
}

fn handle_err<T>(res: SRCCResult<T>) -> T {
    match res {
        Ok(res) => res,
        Err(err) => {
            eprintln!("scc: error: {}", err);
            exit(1);
        }
    }
}

fn main() {
    // Parse the opts;
    let cli = Cli::parse();

    // Prepare the compiler
    let scc_opts = SCCCompilerOptions::new();
    let mut scc = SCCCompiler::new(scc_opts);
    let src_file = cli.input.to_str().unwrap();
    scc.set_inputs_paths(&[src_file]);

    // Emit the requested output.
    if cli.emit_cast {
        handle_err(scc.emit_cast(OutputTarget::Stdout));
    } else if cli.emit_sir {
        handle_err(scc.emit_sir(OutputTarget::Stdout));
    } else if cli.emit_lir {
        handle_err(scc.emit_lir(OutputTarget::Stdout));
    } else {
        todo!("Compilation not supported")
    }
}

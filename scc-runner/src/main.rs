use std::path::PathBuf;

use clap::Parser;
use scclib::scc_compiler::{SCCCompiler, SCCCompilerOptions};
use sir_core::ir_printer::IRPrintableObject;
use sir_runner::runner::{SIRRunner, SIRRunnerMode, SIRRunnerOpts};

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Input file path
    input: PathBuf,

    // List of inputs values passed to the function.
    #[clap(long, value_parser, num_args = 0.., value_delimiter = ' ')]
    inputs: Vec<String>,

    // Name of the function to be executed.
    #[clap(short, long)]
    function: String,

    // Mode used to run the function.
    #[clap(long, value_enum)]
    mode: Mode,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, clap::ValueEnum)]
enum Mode {
    /// Use the interpreter.
    Interpreter,
}

fn get_runner_opts(cli: &Cli) -> SIRRunnerOpts {
    let mode = match cli.mode {
        Mode::Interpreter => SIRRunnerMode::Interpreter,
    };
    SIRRunnerOpts::new(mode)
}

fn main() {
    // Parse the command line arguments.
    let cli = Cli::parse();

    // Prepare the compiler.
    let compiler_opts = SCCCompilerOptions::new();
    let mut compiler = SCCCompiler::new(compiler_opts);
    let src_path = cli.input.to_str().unwrap();
    compiler.set_inputs_paths(&[src_path]);

    // Lower the source C file to LIR.
    let root = match compiler.get_lir() {
        Ok(root) => root,
        Err(err) => panic!("Failed to generate LIR: {}", err),
    };
    let ctx = compiler.get_sir_context();
    let root = ctx.get_generic_operation(root);

    // Prepare the runner.
    let opts = get_runner_opts(&cli);
    let mut runner = SIRRunner::new(opts, root);

    // Prepare the inputs.
    let inputs: Vec<_> = cli.inputs.iter().map(String::as_str).collect();
    let inputs = runner.parse_function_inputs(&cli.function, &inputs);

    // Run the function.
    let outputs = runner.execute_function(&cli.function, inputs.clone());

    // Print the results.
    println!(
        "Executed function `{}`: {} inputs, {} outputs",
        &cli.function,
        cli.inputs.len(),
        outputs.len()
    );
    for (idx, input_val) in (&inputs).iter().enumerate() {
        println!("input #{} = `{}`", idx, input_val.to_string_repr());
    }
    for (idx, output_val) in (&outputs).iter().enumerate() {
        println!("output #{} = `{}`", idx, output_val.to_string_repr());
    }
}

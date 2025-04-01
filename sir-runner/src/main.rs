use std::path::PathBuf;

use clap::Parser;
use diagnostics::diagnostics::CompilerInputs;
use iostreams::source_streams_set::SourceStreamsSet;
use sir_core::{
    ir_context::IRContext,
    ir_data::OperationID,
    ir_parser::{IRParser, IRParserOpts},
    ir_printer::IRPrintableObject,
};
use sir_func::func_ops::register_func_ops;
use sir_lir::lir_ops::register_lir_ops;
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

fn register_all_sir_ops(ctx: &mut IRContext) {
    register_func_ops(ctx);
    register_lir_ops(ctx);
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

    // Prepare the context.
    let mut ctx = IRContext::new();
    register_all_sir_ops(&mut ctx);

    // Parse the input file.
    let mut ss = SourceStreamsSet::new();
    let src_file = ss.add_source_file(cli.input.to_str().unwrap());
    let mut parser_opts = IRParserOpts::new();
    parser_opts.accept_unregistred_ops = false;
    let parser = IRParser::new(parser_opts, ss.open_stream(src_file));
    let root = parser.parse_all_with_context::<OperationID>(&mut ctx);
    let root = match root.resolve(CompilerInputs::Sources(&ss)) {
        Some(root) => root,
        None => panic!("Parsing failure"),
    };
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

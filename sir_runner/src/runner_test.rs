use core::panic;

use diagnostics::diagnostics::CompilerInputs;
use iostreams::source_streams_set::SourceStreamsSet;
use sir_core::{
    ir_context::IRContext,
    ir_data::OperationID,
    ir_parser::{IRParser, IRParserOpts},
    ir_printer::IRPrintableObject,
};
use utils::stringutils::split_with_commas;
use xtest::{
    test_driver::{OutputTestWriter, TestRunner},
    test_file::TestConfig,
};

use crate::runner::{SIRRunner, SIRRunnerMode, SIRRunnerOpts};

/// Interface used to be able to configure a SIRRunnerTestRunner object.
pub trait SIRRunnerTestInterface {
    fn setup_context(&self, ctx: &mut IRContext);
}

/// Class used for unit tests to execute programs with SIRRunner.
#[derive(Default)]
pub struct SIRRunnerTestRunner<T: SIRRunnerTestInterface> {
    int: T,
}

impl<T: SIRRunnerTestInterface> SIRRunnerTestRunner<T> {
    fn get_runner_opts(&self, cfg: &TestConfig) -> SIRRunnerOpts {
        let args = &cfg.command()[1..];
        assert!(
            args.len() == 1,
            "sir-runner must have a single argument, but got `{:?}`",
            args
        );

        let mode_str = &args[0];
        let mode = if args[0] == "--interpret" {
            SIRRunnerMode::Interpreter
        } else {
            panic!("Unsupported mode for sir-runner: `{}`", mode_str)
        };

        SIRRunnerOpts::new(mode)
    }

    fn run_subcommand(&self, runner: &mut SIRRunner, test_args: &[&str]) {
        // Parse the arguments.
        assert_eq!(test_args[0], "exec", "first argument must be exec");
        let fun_name = test_args[1];
        let mut inputs_vals = Vec::new();
        let mut outputs_vals = Vec::new();

        let mut i = 2;
        while i < test_args.len() {
            let arg = test_args[i];
            if arg == "--inputs" {
                // Parse the inputs.
                i += 1;
                while i < test_args.len() && !test_args[i].starts_with("--") {
                    inputs_vals.push(test_args[i]);
                    i += 1;
                }
            } else if arg == "--outputs" {
                // Parse the outputs.
                i += 1;
                while i < test_args.len() && !test_args[i].starts_with("--") {
                    outputs_vals.push(test_args[i]);
                    i += 1;
                }
            } else {
                panic!("Unknown test argument `{}`", arg);
            }
        }

        // Parse the inputs / outputs values.
        let inputs = runner.parse_function_inputs(fun_name, &inputs_vals);
        let exp_outputs = runner.parse_function_outputs(fun_name, &outputs_vals);

        // Run the function.
        let run_outputs = runner.execute_function(fun_name, inputs);

        // Verify the outputs.
        assert!(
            run_outputs.len() == exp_outputs.len(),
            "Executed function has {} outputs, but the test command expects {} outputs",
            run_outputs.len(),
            exp_outputs.len()
        );

        for (idx, (run_val, exp_val)) in run_outputs.into_iter().zip(exp_outputs).enumerate() {
            if run_val != exp_val {
                panic!("Function `{}` output #{} is incorrect: expected output is {}, but return value is {}",
                fun_name, idx, exp_val.to_string_repr(), run_val.to_string_repr());
            }
        }
    }
}

impl<T: SIRRunnerTestInterface> TestRunner for SIRRunnerTestRunner<T> {
    fn get_runner_name(&self) -> &'static str {
        "sir-runner"
    }

    fn run_test(&self, cfg: &TestConfig, os: &mut OutputTestWriter) -> i64 {
        // Get the runner opts.
        let runner_opts = self.get_runner_opts(cfg);

        // Prepare the context.
        let mut ctx = IRContext::new();
        self.int.setup_context(&mut ctx);

        // Check the

        // Parse the input file.
        let mut ss = SourceStreamsSet::new();
        let src_file = ss.add_source_file(cfg.path());
        let mut parser_opts = IRParserOpts::new();
        parser_opts.accept_unregistred_ops = false;
        let parser = IRParser::new(parser_opts, ss.open_stream(src_file));
        let root = parser.parse_all_with_context::<OperationID>(&mut ctx);
        let root = match root.resolve_with_stream(CompilerInputs::Sources(&ss), os) {
            Some(root) => root,
            None => return 1,
        };

        // Build the runner.
        let root = ctx.get_generic_operation(root);
        let mut runner = SIRRunner::new(runner_opts, root);

        // Go through all tests.
        if cfg.annotations().is_empty() {
            panic!("No test annotation found !");
        }
        for annot in cfg.annotations() {
            let test_args = split_with_commas(&annot.line);
            self.run_subcommand(&mut runner, &test_args);
        }

        0
    }
}

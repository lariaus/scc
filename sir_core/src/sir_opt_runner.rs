use std::fs::File;

use diagnostics::diagnostics::CompilerInputs;
use iostreams::source_streams_set::SourceStreamsSet;
use utils::{argparse::ArgumentsParser, stdout_writer::StdoutWriter};

use crate::{
    compiler_setup::CompilerSetup,
    ir_context::IRContext,
    ir_data::OperationID,
    ir_parser::{IRParser, IRParserOpts},
    ir_printer::{IRPrinter, IRPrinterOptions},
    ir_verifier::{IRVerifier, IRVerifierOptions},
    pass_manager::{PassManager, PassManagerOptions},
};

// This class is used to run and control the behaviour of the runner
pub struct SIROptRunner {
    cs: CompilerSetup,
    setup_fns: Vec<Box<dyn FnOnce(&mut CompilerSetup) -> ()>>,
    setup_ctx_fns: Vec<Box<dyn FnOnce(&mut IRContext) -> ()>>,
    input_path: Option<String>,
    output_path: Option<String>,
    allow_unregistered_ops: bool,
    manually_pass_input_file: bool,
    debug_mode: bool,
    print_ir_before_all_passes: bool,
    print_ir_after_all_passes: bool,
    print_ir_before_pass: Option<String>,
    print_ir_after_pass: Option<String>,
    ctx: Option<IRContext>,
    ap: ArgumentsParser,
}

impl SIROptRunner {
    // Create a new runner.
    pub fn new(bin_name: String, description: String, version: String) -> Self {
        Self {
            cs: CompilerSetup::new(),
            setup_fns: Vec::new(),
            setup_ctx_fns: Vec::new(),
            input_path: None,
            output_path: None,
            allow_unregistered_ops: false,
            manually_pass_input_file: false,
            debug_mode: false,
            print_ir_before_all_passes: false,
            print_ir_after_all_passes: false,
            print_ir_before_pass: None,
            print_ir_after_pass: None,
            ctx: None,
            ap: ArgumentsParser::new(bin_name, description, version),
        }
    }

    pub fn allow_unregistered_ops(&self) -> bool {
        self.allow_unregistered_ops
    }

    // If true, enable support for unregistered ops.
    pub fn set_allow_unregistered_ops(&mut self, allow_unregistered_ops: bool) {
        self.allow_unregistered_ops = allow_unregistered_ops;
    }

    pub fn manually_pass_input_file(&self) -> bool {
        self.manually_pass_input_file
    }

    // If true, we can't use the args to pass the input file.
    // It must be specified by the user.
    pub fn set_manually_pass_input_file(&mut self, manually_pass_input_file: bool) {
        self.manually_pass_input_file = manually_pass_input_file;
    }

    pub fn debug_mode(&self) -> bool {
        self.debug_mode
    }

    // If true, enable the debug mode for all executed passes.
    pub fn set_debug_mode(&mut self, debug_mode: bool) {
        self.debug_mode = debug_mode;
    }

    pub fn print_ir_before_all_passes(&self) -> bool {
        self.print_ir_before_all_passes
    }

    // If true, print the IR before running each pass.
    pub fn set_print_ir_before_all_passes(&mut self, print_ir_before_all_passes: bool) {
        self.print_ir_before_all_passes = print_ir_before_all_passes;
    }

    pub fn print_ir_after_all_passes(&self) -> bool {
        self.print_ir_after_all_passes
    }

    // If true, print the IR after running each pass.
    pub fn set_print_ir_after_all_passes(&mut self, print_ir_after_all_passes: bool) {
        self.print_ir_after_all_passes = print_ir_after_all_passes;
    }

    pub fn print_ir_before_pass(&self) -> Option<&str> {
        self.print_ir_before_pass.as_ref().map(|s| s.as_str())
    }

    // If true, print the IR before running the specified pass.
    pub fn set_print_ir_before_pass(&mut self, print_ir_before_pass: String) {
        self.print_ir_before_pass = Some(print_ir_before_pass);
    }

    pub fn print_ir_after_pass(&self) -> Option<&str> {
        self.print_ir_after_pass.as_ref().map(|s| s.as_str())
    }

    // If true, print the IR after running the specified pass.
    pub fn set_print_ir_after_pass(&mut self, print_ir_after_pass: String) {
        self.print_ir_after_pass = Some(print_ir_after_pass);
    }

    // Get the optional input file path.
    pub fn input_path(&self) -> Option<&str> {
        self.input_path.as_ref().map(|x| &x[..])
    }

    // Set the input file path.
    pub fn set_input_path(&mut self, path: String) {
        self.input_path = Some(path);
    }

    // Get the optional output file path.
    pub fn output_path(&self) -> Option<&str> {
        self.output_path.as_ref().map(|x| &x[..])
    }

    // Set the output file path.
    pub fn set_output_path(&mut self, path: String) {
        self.output_path = Some(path);
    }

    // Register a callback for the CompilerSetup
    pub fn register_setup_callback<F: FnOnce(&mut CompilerSetup) -> () + 'static>(
        &mut self,
        callback: F,
    ) {
        self.setup_fns.push(Box::new(callback));
    }

    // Register a setup callback for the IRContext.
    pub fn register_setup_ctx_callback<F: FnOnce(&mut IRContext) -> () + 'static>(
        &mut self,
        callback: F,
    ) {
        self.setup_ctx_fns.push(Box::new(callback));
    }

    // Setup the arguments parser.
    fn _setup_args(&mut self) {
        self.ap.add_flag(
            "--allow-unregistered-ops",
            None,
            false,
            "Allow having unregistered ops in the IR",
        );
        self.ap.add_flag(
            "--debug",
            None,
            false,
            "Enable debug_mode for all executed passes",
        );
        self.ap.add_flag(
            "--print-ir-before-all",
            None,
            false,
            "Print the IR before running each pass",
        );
        self.ap.add_flag(
            "--print-ir-after-all",
            None,
            false,
            "Print the IR after running each pass",
        );
        if !self.manually_pass_input_file {
            self.ap.add_option(
                "--input-path",
                Some('i'),
                true,
                false,
                "Input path to the IR file",
            );
        }
        self.ap.add_option(
            "--output-path",
            Some('o'),
            false,
            false,
            "Output path to the IR file",
        );

        // Setup the arguments for the passes.
        for infos in self.cs.get_registered_passes_builders().values() {
            let pass_arg = format!("--{}", infos.name);
            self.ap.add_flag(&pass_arg, None, true, infos.description);
        }
    }

    // Parse all the args.
    fn _parse_args(&mut self, args: Vec<String>) {
        self.ap.parse_args(args);
        self.allow_unregistered_ops = self.ap.get_flag("--allow-unregistered-ops");
        self.debug_mode = self.ap.get_flag("--debug");
        self.print_ir_before_all_passes = self.ap.get_flag("--print-ir-before-all");
        self.print_ir_after_all_passes = self.ap.get_flag("--print-ir-after-all");
        if !self.manually_pass_input_file {
            if let Some(input_path) = self.ap.get_option_value("--input-path") {
                self.input_path = Some(input_path.to_owned());
            }
        }
        if let Some(output_path) = self.ap.get_option_value("--output-path") {
            self.output_path = Some(output_path.to_owned());
        }
    }

    // Setup the runner with the args.
    pub fn setup(&mut self, args: Vec<String>) {
        // Setup the Compiler.
        let mut setup_fns = Vec::new();
        std::mem::swap(&mut setup_fns, &mut self.setup_fns);
        for fun in setup_fns {
            fun(&mut self.cs)
        }

        // Setup the CLI args.
        self._setup_args();

        // Create the context.
        let mut ctx = IRContext::new();

        // Call all the setup ctx fns
        let mut setup_ctx_fns = Vec::new();
        std::mem::swap(&mut setup_ctx_fns, &mut self.setup_ctx_fns);
        for fun in setup_ctx_fns {
            fun(&mut ctx)
        }

        // Parse the arguments.
        self._parse_args(args);

        self.ctx = Some(ctx);
    }

    // Execute the program, using os as the output stream.
    pub fn run_with_stream<W: std::io::Write>(&mut self, os: &mut W) -> i64 {
        // Get the context.
        let mut ctx = self.ctx.as_mut().expect("setup must be called before run");

        // Setup the input stream.
        // @TODO[I2][SIR-CORE]: Support using stdin for input of SIROptRunner.
        let input_path = self.input_path.as_ref().expect("Missing input file path");
        let mut ss = SourceStreamsSet::new();
        let src_file = ss.add_source_file(input_path);

        // Parse the graph.
        let mut parser_opts = IRParserOpts::new();
        parser_opts.accept_unregistred_ops = self.allow_unregistered_ops;
        let parser = IRParser::new(parser_opts, ss.open_stream(src_file));

        let root_op = parser.parse_all_with_context::<OperationID>(&mut ctx);
        let root_op = match root_op.resolve_with_stream(CompilerInputs::Sources(&ss), os) {
            Some(root_op) => root_op,
            None => return 1,
        };

        // Run the verifier.
        let mut verifier_opts = IRVerifierOptions::new();
        verifier_opts.allow_unregistered_ops = self.allow_unregistered_ops;
        let ir_verifier = IRVerifier::new(verifier_opts);
        if ir_verifier
            .verify(ctx.get_generic_operation(root_op))
            .resolve_with_stream(CompilerInputs::Sources(&ss), os)
            .is_none()
        {
            return 1;
        }

        // Setup the PassManager.
        let pm_passes = self.cs.get_registered_passes_builders();
        let mut pm_opts = PassManagerOptions::new();
        pm_opts.print_ir_before_all_passes = self.print_ir_before_all_passes;
        pm_opts.print_ir_after_all_passes = self.print_ir_after_all_passes;
        if let Some(name) = &self.print_ir_before_pass {
            pm_opts.print_ir_before_pass = Some(name.to_owned());
        }
        if let Some(name) = &self.print_ir_after_pass {
            pm_opts.print_ir_after_pass = Some(name.to_owned());
        }
        pm_opts.debug_mode = self.debug_mode;
        let mut pm = PassManager::new(pm_opts, &self.cs);
        // Add all the registered passes with the CLI.
        for arg in self.ap.get_parsed_args() {
            let name = &arg.argname()[2..];
            if pm_passes.contains_key(name) {
                pm.add_pass_by_name(name);
            }
        }
        let passes_runner = pm.make_runner();

        // Run the passes.
        if passes_runner
            .run_all(ctx, root_op)
            .resolve_with_stream(CompilerInputs::Sources(&ss), os)
            .is_none()
        {
            return 1;
        }

        // Print the graph.
        let printer_opts = IRPrinterOptions::new();
        if let Some(output_path) = &self.output_path {
            let file = File::open(output_path).expect("Failed to open the output file");
            let mut printer = IRPrinter::new(printer_opts, Box::new(file));
            printer.print(&ctx.get_generic_operation(root_op)).unwrap();
            write!(printer.os(), "\n").unwrap();
        } else {
            // Print to stdout.
            // TODO: Fix printer.
            let mut printer = IRPrinter::new_string_builder(printer_opts);
            printer.print(&ctx.get_generic_operation(root_op)).unwrap();
            write!(os, "{}\n", printer.take_output_string().unwrap()).unwrap();
        }

        return 0;
    }

    // Execute the program, outputing everything to stdout.
    pub fn run(&mut self) -> i64 {
        let mut os: StdoutWriter = Default::default();
        self.run_with_stream(&mut os)
    }
}

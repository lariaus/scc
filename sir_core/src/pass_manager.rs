use diagnostics::{
    diagnostics::{CompilerDiagnostics, DiagnosticsEmitter, ErrorOrSuccess},
    result::CompilerResult,
};

use crate::{
    canonicalize_pass::CanonicalizePass,
    compiler_setup::CompilerSetup,
    cse_pass::CSEPass,
    ir_context::IRContext,
    ir_data::OperationID,
    ir_printer::IRPrintableObject,
    ir_transforms::TransformsList,
    ir_verifier::{IRVerifier, IRVerifierOptions},
};

/// A Pass update the IR in a specific way.
/// It's pretty generic, and can do many things.
pub trait Pass {
    /// Called to enable the debug mode for this pass.
    fn set_debug_mode(&mut self, debug_mode: bool);

    /// Run the pass on `op`.
    /// It should only modify `op`` and its children.
    fn run_on_operation(
        &self,
        diagnostics: &mut DiagnosticsEmitter,
        ctx: &mut IRContext,
        op: OperationID,
    ) -> ErrorOrSuccess;

    /// Returns the list of transforms applied by this pass.
    /// This allows adding more transforms to this pass through the PassManager.
    fn get_exported_transforms_list(&mut self) -> Option<&mut TransformsList> {
        None
    }
}

// All pass must also implement the PassRegistration trait.
pub trait PassRegistration: Pass {
    /// Returns the name of the pass.
    fn get_pass_name() -> &'static str;

    /// Returns a description of what the pass does exactly.
    fn get_pass_description() -> &'static str;
}

// Store all infos needed to exec a pass
struct PassExecutionInfos {
    pass: Box<dyn Pass>,
    pass_name: &'static str,
    print_ir_before: bool,
    print_ir_after: bool,
}

pub struct PassManagerOptions {
    // Print the IR before executing all passes.
    pub print_ir_before_all_passes: bool,
    // Print IR before executing all passes.
    pub print_ir_after_all_passes: bool,
    // Print IR before executing a pass of this name.
    pub print_ir_before_pass: Option<String>,
    // Print IR after executing a pass of this name.
    pub print_ir_after_pass: Option<String>,
    // Enable debug_mode for all passes.
    pub debug_mode: bool,
}

impl PassManagerOptions {
    pub fn new() -> Self {
        Self {
            print_ir_before_all_passes: false,
            print_ir_after_all_passes: false,
            print_ir_before_pass: None,
            print_ir_after_pass: None,
            debug_mode: false,
        }
    }
}

// A pass Manager is used to run a sequence of passes in a specific order.
pub struct PassManager<'a> {
    sir_setup: &'a CompilerSetup,
    passes: Vec<PassExecutionInfos>,
    opts: PassManagerOptions,
}

impl<'a> PassManager<'a> {
    // Create an empty pass manager
    pub fn new(opts: PassManagerOptions, sir_setup: &'a CompilerSetup) -> Self {
        Self {
            sir_setup,
            passes: vec![],
            opts,
        }
    }

    // Add a pass that you want to execute on the IR.
    pub fn add_pass<T: PassRegistration + 'static>(&mut self, pass: T) {
        self._add_pass(Box::new(pass), T::get_pass_name());
    }

    fn _add_pass(&mut self, mut pass: Box<dyn Pass>, pass_name: &'static str) {
        if let Some(transforms_list) = pass.get_exported_transforms_list() {
            if let Some(builders) = self
                .sir_setup
                .get_registered_extra_transforms()
                .get(pass_name)
            {
                for builder in builders {
                    builder(transforms_list);
                }
            }
        }

        if self.opts.debug_mode {
            pass.set_debug_mode(true);
        }

        self.passes.push(PassExecutionInfos {
            pass,
            pass_name,
            print_ir_before: self.opts.print_ir_before_all_passes
                || Some(pass_name) == self.opts.print_ir_before_pass.as_ref().map(|x| x.as_str()),
            print_ir_after: self.opts.print_ir_after_all_passes
                || Some(pass_name) == self.opts.print_ir_after_pass.as_ref().map(|x| x.as_str()),
        });
    }

    // Add a pass that you want to execute on the IR.
    pub fn add_pass_by_name(&mut self, name: &str) {
        let infos = match self.sir_setup.get_registered_passes_builders().get(name) {
            Some(entry) => entry,
            None => panic!("No registered pass found for `{}`", name),
        };
        let pass = (infos.builder)();
        self._add_pass(pass, infos.name);
    }

    /// Enable the debug mode only for the passes named `pass_name`.
    /// Must be called after building all passes.
    pub fn enable_debug_mode_for_pass(&mut self, name: &str) {
        for infos in &mut self.passes {
            if infos.pass_name == name {
                infos.pass.set_debug_mode(true);
            }
        }
    }

    /// Build and return the runner with all the added passes.
    pub fn make_runner(self) -> PassesRunner {
        PassesRunner {
            passes: self.passes,
        }
    }
}

// Class to run a set of passes
pub struct PassesRunner {
    passes: Vec<PassExecutionInfos>,
}

impl PassesRunner {
    // Run all registered passes on `root`.
    // Returns any possible error returned by some of the pass.
    // Will stop as soon as a pass finds a hard error, or after all passes ran successfully.
    pub fn run_all(&self, ctx: &mut IRContext, root: OperationID) -> CompilerResult<()> {
        let mut diagnostics = CompilerDiagnostics::new();

        for infos in &self.passes {
            // Optionnaly print the IR before.
            if infos.print_ir_before {
                eprintln!("// IR Dump Before Running pass {}:", infos.pass_name);
                eprintln!("{}\n", ctx.get_generic_operation(root).to_string_repr());
            }

            let mut emitter = DiagnosticsEmitter::new(&mut diagnostics, infos.pass_name);
            if infos
                .pass
                .run_on_operation(&mut emitter, ctx, root)
                .is_err()
            {
                assert!(
                    diagnostics.has_any_errors(),
                    "pass returned err without any errors"
                );
                // Find an error, return early
                return CompilerResult::make(diagnostics, None);
            } else {
                assert!(
                    !diagnostics.has_any_errors(),
                    "pass returned success but has errors"
                );
            }

            // Optionnaly print the IR after.
            if infos.print_ir_after {
                eprintln!("// IR Dump After Running pass {}:", infos.pass_name);
                eprintln!("{}\n", ctx.get_generic_operation(root).to_string_repr());
            }

            // Let's verify if the IR is still valid.
            let verifier_opts = IRVerifierOptions::new();
            let verifier = IRVerifier::new(verifier_opts);
            let res = verifier.verify(ctx.get_generic_operation(root));
            let has_err = res.has_errors();

            // Extend the diagnostics object and return in case of error.
            diagnostics.merge_with(res.take_diagnostics());
            if has_err {
                assert!(
                    diagnostics.has_any_errors(),
                    "verifier returned err without any errors"
                );
                // Find an error, return early.
                return CompilerResult::make(diagnostics, None);
            }
        }

        // Return success.
        CompilerResult::make(diagnostics, Some(()))
    }
}

// Register all builtin passes of sir_core.
pub fn register_core_passes(cs: &mut CompilerSetup) {
    cs.register_pass(&CanonicalizePass::new);
    cs.register_pass(&CSEPass::new);
}

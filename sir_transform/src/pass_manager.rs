use diagnostics::{
    diagnostics::{CompilerDiagnostics, DiagnosticsEmitter},
    result::CompilerResult,
};

use sir_core::{
    ir_context::IRContext,
    ir_data::OperationID,
    ir_printer::IRPrintableObject,
    ir_verifier::{IRVerifier, IRVerifierOptions},
};

use crate::{
    pass::{Pass, PassRegistration},
    sir_backend::SIRBackend,
    transforms_registry::TransformsRegistry,
};

// Store all infos needed to exec a pass
struct PassExecutionInfos {
    pass: Option<Box<dyn Pass>>,
    pass_name: String,
    debug: bool,
    print_ir_before: bool,
    print_ir_after: bool,
}

/// Options to configure the PassManager.
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
    // Create new options with all defaults.
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
pub struct PassManager {
    passes: Vec<PassExecutionInfos>,
    opts: PassManagerOptions,
    is_setup: bool,
}

impl PassManager {
    // Create an empty pass manager
    pub fn new(opts: PassManagerOptions) -> Self {
        Self {
            passes: vec![],
            opts,
            is_setup: false,
        }
    }

    // Add a pass that you want to execute on the IR.
    pub fn add_pass<T: PassRegistration + 'static>(&mut self, pass: T) {
        self._add_pass(Some(Box::new(pass)), T::get_pass_name());
    }

    // Add a pass that you want to execute on the IR.
    pub fn add_pass_by_name(&mut self, name: &str) {
        self._add_pass(None, name);
    }

    /// Enable the debug mode only for the passes named `pass_name`.
    /// Must be called after building all passes.
    pub fn enable_debug_mode_for_pass(&mut self, name: &str) {
        for infos in &mut self.passes {
            if infos.pass_name == name {
                infos.debug = true;
            }
        }
    }

    fn _add_pass(&mut self, pass: Option<Box<dyn Pass>>, pass_name: &str) {
        self.passes.push(PassExecutionInfos {
            pass,
            pass_name: pass_name.to_owned(),
            debug: self.opts.debug_mode,
            print_ir_before: self.opts.print_ir_before_all_passes
                || Some(pass_name) == self.opts.print_ir_before_pass.as_ref().map(|x| x.as_str()),
            print_ir_after: self.opts.print_ir_after_all_passes
                || Some(pass_name) == self.opts.print_ir_after_pass.as_ref().map(|x| x.as_str()),
        });
    }

    fn _setup_all_passes(&mut self, cs: Option<&TransformsRegistry>) {
        if self.is_setup {
            return;
        }

        for pass_exec in &mut self.passes {
            // Build the pass if needed.
            if pass_exec.pass.is_none() {
                let name = &pass_exec.pass_name;
                // Get the pass from its name.
                let cs = match cs {
                    Some(cs) => cs,
                    None => panic!("No registered pass found for `{}`", name),
                };

                let infos = match cs.get_registered_passes_builders().get(&name[..]) {
                    Some(entry) => entry,
                    None => panic!("No registered pass found for `{}`", name),
                };
                pass_exec.pass = Some((infos.builder)());
            }

            // Setup the pass object.
            let pass = pass_exec.pass.as_mut().unwrap();
            if pass_exec.debug {
                pass.set_debug_mode(true);
            }

            let cs = match cs {
                Some(cs) => cs,
                None => continue,
            };

            // Add the extra transforms.
            if let Some(transforms_list) = pass.get_exported_transforms_list() {
                if let Some(builders) = cs
                    .get_registered_extra_transforms()
                    .get(&pass_exec.pass_name)
                {
                    for builder in builders {
                        builder(transforms_list);
                    }
                }
            }

            // Call the options callbacks.
            if let Some(dyn_opts) = pass.get_dynamic_options() {
                if let Some(callbacks) =
                    cs.get_registered_passes_configs().get(&pass_exec.pass_name)
                {
                    for callback in callbacks {
                        callback(dyn_opts);
                    }
                }
            }
        }

        self.is_setup = true;
    }

    // Run all passes on the selected IR,
    pub fn run(
        &mut self,
        backend: &SIRBackend,
        ctx: &mut IRContext,
        root: OperationID,
    ) -> CompilerResult<()> {
        // Setup the registered passes.
        self._setup_all_passes(TransformsRegistry::get(ctx));

        // Prepare the diagnostics.
        let mut diagnostics = CompilerDiagnostics::new();

        // Run all the passes.
        for infos in &self.passes {
            // Optionnaly print the IR before.
            if infos.print_ir_before {
                eprintln!("// IR Dump Before Running pass {}:", infos.pass_name);
                eprintln!("{}\n", ctx.get_generic_operation(root).to_string_repr());
            }

            let mut emitter = DiagnosticsEmitter::new(&mut diagnostics, &infos.pass_name);
            if infos
                .pass
                .as_ref()
                .unwrap()
                .run_on_operation(backend, &mut emitter, ctx, root)
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

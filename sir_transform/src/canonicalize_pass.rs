use sir_core::{ir_context::IRContext, ir_data::OperationID};

use crate::{
    ir_transforms::{apply_transforms_greedily, TransformsList},
    pass::{Pass, PassRegistration},
    sir_backend::SIRBackend,
};

// Pass to execute common canonicalization transforms
pub struct CanonicalizePass {
    transforms: TransformsList,
    debug_mode: bool,
}

impl CanonicalizePass {
    // Build the CanonicalizePass object.
    pub fn new() -> Self {
        Self {
            transforms: TransformsList::new(),
            debug_mode: false,
        }
    }
}

impl Pass for CanonicalizePass {
    fn run_on_operation(
        &self,
        _backend: &SIRBackend,
        diagnostics: &mut diagnostics::diagnostics::DiagnosticsEmitter,
        ctx: &mut IRContext,
        op: OperationID,
    ) -> diagnostics::diagnostics::ErrorOrSuccess {
        apply_transforms_greedily(ctx, diagnostics, &self.transforms, op, self.debug_mode);
        Ok(())
    }

    fn get_exported_transforms_list(&mut self) -> Option<&mut TransformsList> {
        Some(&mut self.transforms)
    }

    fn set_debug_mode(&mut self, debug_mode: bool) {
        self.debug_mode = debug_mode;
    }
}

impl PassRegistration for CanonicalizePass {
    fn get_pass_name() -> &'static str {
        "canonicalize"
    }

    fn get_pass_description() -> &'static str {
        "Run generic canonicalization transforms"
    }
}

use diagnostics::diagnostics::{DiagnosticsEmitter, ErrorOrSuccess};
use sir_core::{ir_context::IRContext, ir_data::OperationID};

use crate::{ir_transforms::TransformsList, sir_backend::SIRBackend};

/// A Pass update the IR in a specific way.
/// It's pretty generic, and can do many things.
pub trait Pass {
    /// Called to enable the debug mode for this pass.
    fn set_debug_mode(&mut self, debug_mode: bool);

    /// Run the pass on `op`.
    /// It should only modify `op`` and its children.
    fn run_on_operation(
        &self,
        backend: &SIRBackend,
        diagnostics: &mut DiagnosticsEmitter,
        ctx: &mut IRContext,
        op: OperationID,
    ) -> ErrorOrSuccess;

    /// Returns the list of transforms applied by this pass.
    /// This allows adding more transforms to this pass through the PassManager.
    fn get_exported_transforms_list(&mut self) -> Option<&mut TransformsList> {
        None
    }

    /// Returns a dynamic options object needed to setup the pass.
    fn get_dynamic_options(&mut self) -> Option<&mut dyn DynamicPassOptions> {
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

pub trait DynamicPassOptions {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any;
}

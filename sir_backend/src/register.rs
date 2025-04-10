use sir_core::ir_context::IRContext;

use crate::instruction_selection_pass::InstructionSelectionPass;

// Register all passes for the compiler backend.
pub fn register_backend_passes(ctx: &mut IRContext) {
    sir_transform::context_registry::ContextRegistry::exec_register_fn(
        ctx,
        "__sir/transforms/register_backend_passes",
        |mut registry| {
            registry.register_pass(&InstructionSelectionPass::new);
        },
    );
}

use sir_core::ir_context::IRContext;

use crate::{canonicalize_pass::CanonicalizePass, cse_pass::CSEPass};

// Register all builtin passes of sir_transforms.
pub fn register_core_passes(ctx: &mut IRContext) {
    crate::context_registry::ContextRegistry::exec_register_fn(
        ctx,
        "__sir/transforms/register_core_passes",
        |mut registry| {
            registry.register_pass(&CanonicalizePass::new);
            registry.register_pass(&CSEPass::new);
        },
    );
}

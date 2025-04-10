use sir_core::ir_context::IRContext;

pub mod interfaces;
pub mod legalize_pass;
pub mod low_level_types;
pub mod tags;

// Register all passes of sir_low_level.
pub fn register_low_level_passes(ctx: &mut IRContext) {
    sir_transform::context_registry::ContextRegistry::exec_register_fn(
        ctx,
        "__sir/transforms/register_low_level_passes",
        |mut registry| {
            registry.register_pass(&legalize_pass::LegalizeToLowLevelPass::new);
        },
    );
}

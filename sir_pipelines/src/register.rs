use sir_backend::{backend_ops::register_backend_ops, register::register_backend_passes};
use sir_backend_arm_v86a::backend_desc::register_armv86a_backend;
use sir_core::ir_context::IRContext;
use sir_func::{func_ops::register_func_ops, func_transforms::register_func_transforms};
use sir_interpreter::transforms::register_interpreter_transforms;
use sir_lir::lir_ops::register_lir_ops;
use sir_low_level::register_low_level_passes;
use sir_math::{math_ops::register_math_ops, math_transforms::register_math_transforms};
use sir_mem::{mem_ops::register_mem_ops, mem_transforms::register_mem_transforms};
use sir_transform::{
    register::register_core_passes, sir_backend::BackendsRegistry,
    unknown_backend_desc::register_unknown_backend,
};

/// Register all possible ops of the SIR libraries.
pub fn register_all_sir_ops(ctx: &mut IRContext) {
    register_func_ops(ctx);
    register_lir_ops(ctx);
    register_math_ops(ctx);
    register_mem_ops(ctx);

    register_backend_ops(ctx);
}

/// Register all possible passes of the SIR libraries.
pub fn register_all_sir_passes(ctx: &mut IRContext) {
    // Register passes.
    register_core_passes(ctx);
    register_low_level_passes(ctx);
    register_backend_passes(ctx);

    // Register transforms.
    register_func_transforms(ctx);
    register_interpreter_transforms(ctx);
    register_math_transforms(ctx);
    register_mem_transforms(ctx);
}

/// Register all the available SIR backends.
pub fn register_all_backends(registry: &mut BackendsRegistry) {
    register_unknown_backend(registry);
    register_armv86a_backend(registry);
}

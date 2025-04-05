use sir_core::{
    compiler_setup::CompilerSetup, ir_context::IRContext, pass_manager::register_core_passes,
};
use sir_func::{func_ops::register_func_ops, func_transforms::register_func_transforms};
use sir_interpreter::transforms::register_interpreter_transforms;
use sir_lir::lir_ops::register_lir_ops;
use sir_low_level::register_low_level_passes;
use sir_math::{math_ops::register_math_ops, math_transforms::register_math_transforms};
use sir_mem::{mem_ops::register_mem_ops, mem_transforms::register_mem_transforms};

/// Register all possible ops of the SIR libraries.
pub fn register_all_sir_ops(ctx: &mut IRContext) {
    // @TODO[I8][SIR-CORE]: Find consistent way to register ops / transforms which avoids duplicate registrations.

    register_func_ops(ctx);
    register_lir_ops(ctx);
    register_math_ops(ctx);
    register_mem_ops(ctx);
}

// Register all possible passes of the SIR libraries.
pub fn register_all_sir_passes(cs: &mut CompilerSetup) {
    // @TODO[I8][SIR-CORE]: Find consistent way to register ops / transforms which avoids duplicate registrations.

    // Register passes.
    register_core_passes(cs);
    register_low_level_passes(cs);

    // Register transforms.
    register_func_transforms(cs);
    register_interpreter_transforms(cs);
    register_math_transforms(cs);
    register_mem_transforms(cs);
}

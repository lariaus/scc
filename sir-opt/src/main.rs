use std::{env::args, process::exit};

use sir_core::{
    compiler_setup::CompilerSetup, ir_context::IRContext, pass_manager::register_core_passes,
    sir_opt_runner::SIROptRunner,
};
use sir_func::{func_ops::register_func_ops, func_transforms::register_func_transforms};
use sir_interpreter::transforms::register_interpreter_transforms;
use sir_lir::lir_ops::register_lir_ops;
use sir_low_level::register_low_level_passes;
use sir_math::{math_ops::register_math_ops, math_transforms::register_math_transforms};
use sir_mem::{mem_ops::register_mem_ops, mem_transforms::register_mem_transforms};

fn register_all_sir_ops(ctx: &mut IRContext) {
    register_func_ops(ctx);
    register_lir_ops(ctx);
    register_math_ops(ctx);
    register_mem_ops(ctx);
}

fn setup_compiler(cs: &mut CompilerSetup) {
    // Register passes.
    register_core_passes(cs);
    register_low_level_passes(cs);

    // Register transforms.
    register_func_transforms(cs);
    register_interpreter_transforms(cs);
    register_math_transforms(cs);
    register_mem_transforms(cs);
}

fn main() {
    let mut runner = SIROptRunner::new(
        "sir-opt".to_owned(),
        "SIR optimizer binary with support of the standard sir ops".to_owned(),
        "0.0.1".to_owned(),
    );

    // Setup the compiler.
    runner.register_setup_callback(setup_compiler);

    // Register all the operations.
    runner.register_setup_ctx_callback(register_all_sir_ops);

    // Setup args (drop the binary name).
    let args: Vec<_> = args().skip(1).collect();
    runner.setup(args);

    let ret_code = runner.run();
    exit(ret_code as i32);
}

use std::{env::args, process::exit};

use sir_opt::sir_opt_runner::SIROptRunner;
use sir_pipelines::register::{
    register_all_backends, register_all_sir_ops, register_all_sir_passes,
};

fn main() {
    let mut runner = SIROptRunner::new(
        "sir-opt".to_owned(),
        "SIR optimizer binary with support of the standard sir ops".to_owned(),
        "0.0.1".to_owned(),
    );

    // Setup the context.
    runner.register_setup_ctx_callback(register_all_sir_passes);
    runner.register_setup_ctx_callback(register_all_sir_ops);
    runner.register_setup_backend_callback(register_all_backends);

    // Setup args (drop the binary name).
    let args: Vec<_> = args().skip(1).collect();
    runner.setup(args);

    let ret_code = runner.run();
    exit(ret_code as i32);
}

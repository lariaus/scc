use sir_backend::register::register_backend_passes;
use sir_backend_arm_v86a::backend_desc::register_armv86a_backend;
use sir_core::ir_context::IRContext;
use sir_func::func_ops::register_func_ops;
use sir_lir::lir_ops::register_lir_ops;
use sir_opt::sir_opt_runner::SIROptRunner;
use sir_transform::sir_backend::BackendsRegistry;
use xtest::{
    test_driver::{OutputTestWriter, TestDriver, TestRunner},
    test_file::TestConfig,
};

fn setup_context(ctx: &mut IRContext) {
    register_func_ops(ctx);
    register_lir_ops(ctx);
    register_backend_passes(ctx);
}

fn setup_backends(registry: &mut BackendsRegistry) {
    register_armv86a_backend(registry);
}

#[derive(Default)]
struct SIROptTestRunner {}

impl TestRunner for SIROptTestRunner {
    fn get_runner_name(&self) -> &'static str {
        "sir-opt"
    }

    fn run_test(&self, cfg: &TestConfig, os: &mut OutputTestWriter) -> i64 {
        let mut runner = SIROptRunner::new(
            "sir-opt".to_owned(),
            "test version of sir-opt to test the armv86a backend library".to_owned(),
            "0.0.1".to_owned(),
        );
        runner.set_manually_pass_input_file(true);
        runner.set_backend_name("Armv8.6-A".to_owned());
        runner.set_input_path(cfg.path().to_owned());
        runner.register_setup_ctx_callback(setup_context);
        runner.register_setup_backend_callback(setup_backends);

        let args = cfg.command()[1..].iter().map(|x| x.to_owned()).collect();
        runner.setup(args);
        runner.run_with_stream(os)
    }
}

fn get_driver() -> TestDriver {
    let mut driver = TestDriver::new();
    driver.add_runner::<SIROptTestRunner>();
    driver
}

fn run_xtest(path: &str) {
    let path = std::env::current_dir()
        .unwrap()
        .as_path()
        .join("tests")
        .join(path)
        .to_str()
        .unwrap()
        .to_string();
    get_driver().run_test(&path);
}

#[test]
fn test_instruction_selection_basic_add_regs() {
    run_xtest("instruction_selection/basic_add_regs.sir");
}

#[test]
fn test_instruction_selection_basic_add_stack() {
    run_xtest("instruction_selection/basic_add_stack.sir");
}

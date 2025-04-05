use sir_core::{
    compiler_setup::CompilerSetup, ir_context::IRContext, pass_manager::register_core_passes,
    sir_opt_runner::SIROptRunner,
};
use sir_func::func_ops::register_func_ops;
use sir_interpreter::transforms::register_interpreter_transforms;
use sir_lir::lir_ops::register_lir_ops;
use sir_runner::runner_test::{SIRRunnerTestInterface, SIRRunnerTestRunner};
use xtest::{
    test_driver::{OutputTestWriter, TestDriver, TestRunner},
    test_file::TestConfig,
};

#[derive(Default)]
struct SIROptTestRunner {}

fn register_ops(ctx: &mut IRContext) {
    register_func_ops(ctx);
    register_lir_ops(ctx);
}

fn setup_compiler(cs: &mut CompilerSetup) {
    register_core_passes(cs);
    register_interpreter_transforms(cs);
}

impl TestRunner for SIROptTestRunner {
    fn get_runner_name(&self) -> &'static str {
        "sir-opt"
    }

    fn run_test(&self, cfg: &TestConfig, os: &mut OutputTestWriter) -> i64 {
        let mut runner = SIROptRunner::new(
            "sir-opt".to_owned(),
            "test version of sir-opt to test sir_lir library".to_owned(),
            "0.0.1".to_owned(),
        );
        runner.set_manually_pass_input_file(true);
        runner.set_input_path(cfg.path().to_owned());

        runner.register_setup_callback(setup_compiler);
        runner.register_setup_ctx_callback(register_ops);

        let args = cfg.command()[1..].iter().map(|x| x.to_owned()).collect();
        runner.setup(args);
        runner.run_with_stream(os)
    }
}

#[derive(Default)]
struct SIRRunnerTestImpl;

impl SIRRunnerTestInterface for SIRRunnerTestImpl {
    fn setup_context(&self, ctx: &mut IRContext) {
        register_ops(ctx);
    }
}

fn get_driver() -> TestDriver {
    let mut driver = TestDriver::new();
    driver.add_runner::<SIROptTestRunner>();
    driver.add_runner::<SIRRunnerTestRunner<SIRRunnerTestImpl>>();
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
fn test_ops_alloca_verifier() {
    run_xtest("ops/alloca_verifier.sir");
}

#[test]
fn test_ops_iadd_verifier() {
    run_xtest("ops/iadd_verifier.sir");
}

#[test]
fn test_ops_load_verifier() {
    run_xtest("ops/load_verifier.sir");
}

#[test]
fn test_ops_store_verifier() {
    run_xtest("ops/store_verifier.sir");
}

#[test]
fn test_transforms_fold_elementwise() {
    run_xtest("transforms/fold_elementwise.sir");
}

#[test]
fn test_runner_elementwise() {
    run_xtest("runner/elementwise.sir");
}

#[test]
fn test_runner_mem_ops() {
    run_xtest("runner/mem_ops.sir");
}

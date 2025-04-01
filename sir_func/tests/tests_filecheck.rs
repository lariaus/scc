use sir_core::{
    compiler_setup::CompilerSetup, ir_context::IRContext, pass_manager::register_core_passes,
    sir_opt_runner::SIROptRunner,
};
use sir_func::{func_ops::register_func_ops, func_transforms::register_func_transforms};
use sir_low_level::register_low_level_passes;
use xtest::{
    test_driver::{OutputTestWriter, TestDriver, TestRunner},
    test_file::TestConfig,
};

fn register_ops(ctx: &mut IRContext) {
    register_func_ops(ctx);
}

fn setup_compiler(cs: &mut CompilerSetup) {
    register_core_passes(cs);
    register_low_level_passes(cs);
    register_func_transforms(cs);
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
            "test version of sir-opt to test sir_func library".to_owned(),
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
fn test_function_verifier() {
    run_xtest("test_function_verifier.sir");
}

#[test]
fn test_conversion_func_to_lir() {
    run_xtest("conversion/func_to_lir.sir");
}

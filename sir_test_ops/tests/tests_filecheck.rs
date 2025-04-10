use sir_core::ir_context::IRContext;
use sir_opt::sir_opt_runner::SIROptRunner;
use sir_test_ops::test_ops::register_test_ops;
use sir_transform::register::register_core_passes;
use xtest::{
    test_driver::{OutputTestWriter, TestDriver, TestRunner},
    test_file::TestConfig,
};

#[derive(Default)]
struct SIROptTestRunner {}

fn setup_context(ctx: &mut IRContext) {
    register_test_ops(ctx);
    register_core_passes(ctx);
}

impl TestRunner for SIROptTestRunner {
    fn get_runner_name(&self) -> &'static str {
        "sir-opt"
    }

    fn run_test(&self, cfg: &TestConfig, os: &mut OutputTestWriter) -> i64 {
        let mut runner = SIROptRunner::new(
            "sir-opt".to_owned(),
            "test version of sir-opt to test sir_core library".to_owned(),
            "0.0.1".to_owned(),
        );
        runner.set_manually_pass_input_file(true);
        runner.set_input_path(cfg.path().to_owned());

        runner.register_setup_ctx_callback(setup_context);

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
fn test_ops() {
    run_xtest("test_ops.sir");
}

#[test]
fn test_add_verifier() {
    run_xtest("test_add_verifier.sir");
}

#[test]
fn test_fun_verifier() {
    run_xtest("test_fun_verifier.sir");
}

#[test]
fn test_cse() {
    run_xtest("test_cse.sir");
}

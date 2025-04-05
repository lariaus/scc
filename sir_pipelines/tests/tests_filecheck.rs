use sir_core::sir_opt_runner::SIROptRunner;
use sir_pipelines::{
    register::{register_all_sir_ops, register_all_sir_passes},
    sir_to_lir::create_sir_to_lir_pipeline,
};
use xtest::{
    test_driver::{OutputTestWriter, TestDriver, TestRunner},
    test_file::TestConfig,
};

#[derive(Default)]
struct SIROptTestRunner {}

impl TestRunner for SIROptTestRunner {
    fn get_runner_name(&self) -> &'static str {
        "sir-opt"
    }

    fn run_test(&self, cfg: &TestConfig, os: &mut OutputTestWriter) -> i64 {
        let args: Vec<_> = cfg.command()[1..].iter().map(|x| x.to_owned()).collect();
        assert!(args.len() == 1, "expected 1 arguments");
        let mode = &args[0];

        let mut runner = SIROptRunner::new(
            "sir-opt".to_owned(),
            "test version of sir-opt to test sir_pipelines".to_owned(),
            "0.0.1".to_owned(),
        );
        runner.set_manually_pass_input_file(true);
        runner.set_input_path(cfg.path().to_owned());

        runner.register_setup_callback(register_all_sir_passes);
        runner.register_setup_ctx_callback(register_all_sir_ops);

        if mode == "--sir-to-lir" {
            runner.set_pre_pipeline_builder(create_sir_to_lir_pipeline);
        } else {
            panic!("Unknown pipeline `{}`", mode);
        }

        runner.setup(vec![]);
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
fn test_sir_to_lir_add_si32() {
    run_xtest("sir_to_lir/add_si32.sir");
}

#[test]
fn test_sir_to_lir_add_i32_stack() {
    run_xtest("sir_to_lir/add_i32_stack.sir");
}

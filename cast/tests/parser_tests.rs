use cast::parser::CParser;
use diagnostics::CompilerDiagnosticsEmitter;
use iostreams::source_streams_set::SourceStreamsSet;
use xtest::{
    test_driver::{OutputTestWriter, TestDriver, TestRunner},
    test_file::TestConfig,
};

#[derive(Default)]
struct ParserTest {}

impl TestRunner for ParserTest {
    fn get_runner_name(&self) -> &'static str {
        "cparser"
    }

    fn run_test(&self, cfg: &TestConfig, os: &mut OutputTestWriter) -> i64 {
        // Prepare the args.
        let mut ss = SourceStreamsSet::new();
        let src_file = ss.add_source_file(cfg.path());

        // Parse the file
        let mut parser = CParser::new(ss.open_stream(src_file));
        let ast = parser.parse();
        let mut diagnostics = parser.take_diagnostics();
        if diagnostics.check_has_any_errors(&ss, os) {
            return 1;
        }

        // Dump the IR.
        ast.dump_to(os);
        return 0;
    }
}

fn get_driver() -> TestDriver {
    let mut driver = TestDriver::new();
    driver.add_runner::<ParserTest>();
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
fn test_fail_invalid_type() {
    run_xtest("parser/fail_invalid_type.c");
}

#[test]
fn test_return_statement() {
    run_xtest("parser/return_statement.c");
}

#[test]
fn test_var_decls() {
    run_xtest("parser/var_decls.c");
}

#[test]
fn test_c_operators() {
    run_xtest("parser/c_operators.c");
}

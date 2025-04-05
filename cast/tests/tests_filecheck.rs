use cast::{
    ast_binder::ASTBinder,
    ast_to_sir::{ASTToSIR, CASTToSIROptions},
    parser::CParser,
};
use diagnostics::diagnostics::CompilerInputs;
use iostreams::source_streams_set::SourceStreamsSet;
use scclib::scc_compiler::{SCCCompiler, SCCCompilerOptions};
use sir_core::{
    ir_context::IRContext,
    ir_data::OperationID,
    ir_printer::IRPrintableObject,
    ir_verifier::{IRVerifier, IRVerifierOptions},
};
use sir_runner::runner_test::{SIRRunnerTestInterface, SIRRunnerTestRunner};
use std::io::Write;
use xtest::{
    test_driver::{OutputTestWriter, TestDriver, TestRunner},
    test_file::TestConfig,
};

#[derive(Default)]
struct CASTTest;

impl TestRunner for CASTTest {
    fn get_runner_name(&self) -> &'static str {
        "cast"
    }

    fn run_test(&self, cfg: &TestConfig, os: &mut OutputTestWriter) -> i64 {
        // Prepare the args.
        let mut ss = SourceStreamsSet::new();
        let src_file = ss.add_source_file(cfg.path());

        // Setup config.
        let mut run_binder = false;
        let mut run_lower_to_sir = false;
        if cfg.command().contains(&"--bind-ast".to_string()) {
            run_binder = true;
        }
        if cfg.command().contains(&"--lower-to-sir".to_string()) {
            run_binder = true;
            run_lower_to_sir = true;
        }

        // Parse the file
        let parser = CParser::new(ss.open_stream(src_file));
        let ast = parser.parse();
        let mut ast = match ast.resolve_with_stream(CompilerInputs::Sources(&ss), os) {
            Some(ast) => ast,
            None => return 1,
        };

        if !run_binder {
            // Dump the IR and exit.
            ast.dump_to(os);
            return 0;
        }

        // Run the binder.
        let binder = ASTBinder::new();
        if binder
            .bind(&mut ast)
            .resolve_with_stream(CompilerInputs::Sources(&ss), os)
            .is_none()
        {
            return 1;
        }

        if !run_lower_to_sir {
            // Dump the IR and exit.
            ast.dump_to(os);
            return 0;
        }

        // Prepare the translater.
        let cast_to_sir_opts = CASTToSIROptions::new();
        let translater = ASTToSIR::new(cast_to_sir_opts);

        // Prepare the SIR context.
        let mut ctx = IRContext::new();
        translater.setup_context(&mut ctx);

        // Run the translation.
        let root = translater.convert_to_sir(&ast, &mut ctx);
        let root = match root.resolve_with_stream(CompilerInputs::Sources(&ss), os) {
            Some(root) => root,
            None => return 1,
        };

        // Run the IR verifier.
        let mut verifier_opts = IRVerifierOptions::new();
        verifier_opts.allow_unregistered_ops = false;
        let ir_verifier = IRVerifier::new(verifier_opts);
        if ir_verifier
            .verify(ctx.get_generic_operation(root))
            .resolve_with_stream(CompilerInputs::Sources(&ss), os)
            .is_none()
        {
            return 1;
        }

        // Dump the IR.
        write!(os, "{}\n", ctx.get_generic_operation(root).to_string_repr()).unwrap();

        return 0;
    }
}

#[derive(Default)]
struct SCCRunnerTestImpl;

impl SIRRunnerTestInterface for SCCRunnerTestImpl {
    fn build_ir(&self, cfg: &TestConfig, _os: &mut OutputTestWriter) -> (IRContext, OperationID) {
        // Prepare the compiler.
        let compiler_opts = SCCCompilerOptions::new();
        let mut compiler = SCCCompiler::new(compiler_opts);
        compiler.set_inputs_paths(&[cfg.path()]);

        // Lower the source C file to LIR.
        let root = match compiler.get_lir() {
            Ok(root) => root,
            Err(err) => panic!("Failed to generate LIR: {}", err),
        };

        let ctx = compiler.take_context();
        (ctx, root)
    }
}

fn get_driver() -> TestDriver {
    let mut driver = TestDriver::new();
    driver.add_runner::<CASTTest>();
    driver.add_runner::<SIRRunnerTestRunner<SCCRunnerTestImpl>>();
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
fn test_parser_fail_invalid_type() {
    run_xtest("parser/fail_invalid_type.c");
}

#[test]
fn test_parser_return_statement() {
    run_xtest("parser/return_statement.c");
}

#[test]
fn test_parser_var_decls() {
    run_xtest("parser/var_decls.c");
}

#[test]
fn test_parser_c_operators() {
    run_xtest("parser/c_operators.c");
}

#[test]
fn test_binder_c_operators() {
    run_xtest("binder/c_operators.c");
}

#[test]
fn test_lower_to_sir_c_operators() {
    run_xtest("lower_to_sir/c_operators.c");
}

#[test]
fn test_runner_c_operators() {
    run_xtest("runner/c_operators.c");
}

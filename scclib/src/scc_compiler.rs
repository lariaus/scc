use std::{fs::File, io::BufWriter, path::PathBuf};

use cast::{
    ast::ASTNode,
    ast_binder::ASTBinder,
    ast_to_sir::{ASTToSIR, CASTToSIROptions},
    parser::CParser,
};
use diagnostics::diagnostics::CompilerInputs;
use iostreams::source_streams_set::SourceStreamsSet;
use sir_core::{
    compiler_setup::CompilerSetup,
    ir_context::IRContext,
    ir_data::OperationID,
    ir_printer::{IRPrinter, IRPrinterOptions},
    pass_manager::{PassManager, PassManagerOptions},
};
use sir_pipelines::{
    register::{register_all_sir_ops, register_all_sir_passes},
    sir_to_lir::create_sir_to_lir_pipeline,
};

/// Struct containing all options to configure the SCCCompiler.
pub struct SCCCompilerOptions {
    // If true, the compiler will dump the generated CAST to stdout.
    dump_cast: bool,

    // If true, the compiler will dump the generated SIR to stdout.
    dump_sir: bool,
}

impl SCCCompilerOptions {
    /// Create a options object with the default config.
    pub fn new() -> Self {
        Self {
            dump_cast: false,
            dump_sir: false,
        }
    }
}

/// Result type used by SCCCompiler.
pub type SRCCResult<T> = Result<T, String>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SIRKind {
    Unready,
    SIR,
    LIR,
}

/// Class handling all compilation of SCC source files.
pub struct SCCCompiler {
    opts: SCCCompilerOptions,
    cs: CompilerSetup,
    ctx: IRContext,
    ss: SourceStreamsSet,
    cast: Option<SRCCResult<ASTNode>>,
    root: Option<SRCCResult<OperationID>>,
    sir_kind: SIRKind,
}

impl SCCCompiler {
    /// Create a new compiler with `opts`.
    pub fn new(opts: SCCCompilerOptions) -> Self {
        // Prepare the IRContetx
        let mut cs = CompilerSetup::new();
        register_all_sir_passes(&mut cs);
        let mut ctx = IRContext::new();
        register_all_sir_ops(&mut ctx);

        Self {
            opts,
            cs,
            ctx,
            ss: SourceStreamsSet::new(),
            cast: None,
            root: None,
            sir_kind: SIRKind::Unready,
        }
    }

    /// Set the inputs files of the compiler.
    pub fn set_inputs_paths(&mut self, paths: &[&str]) {
        assert!(self.ss.len() == 0, "inputs already configured");
        assert!(
            paths.len() == 1,
            "only a single input file is supported for now"
        );
        self.ss.add_source_file(paths[0]);
    }

    /// Get the SIR context of the compiler.
    pub fn get_sir_context(&self) -> &IRContext {
        &self.ctx
    }

    /// Try to get the CAST from the input source.
    /// Returns an error in case of compiler error.
    pub fn get_cast(&mut self) -> SRCCResult<&ASTNode> {
        if self.cast.is_none() {
            self.cast = Some(self._convert_source_to_cast());
        }

        match self.cast.as_ref().unwrap() {
            Ok(node) => Ok(node),
            Err(err) => Err(err.clone()),
        }
    }

    /// Try to get the SIR root op from the input source.
    /// Returns an error in case of compiler error.
    pub fn get_sir_root(&mut self) -> SRCCResult<OperationID> {
        if self.root.is_none() {
            self.root = Some(self._convert_cast_to_sir());
            self.sir_kind = SIRKind::SIR;
        }

        self.root.as_ref().unwrap().clone()
    }

    /// Try to get the SIR from the input source.
    /// Returns an error in case of compiler error.
    pub fn get_sir(&mut self) -> SRCCResult<OperationID> {
        let res = self.get_sir_root()?;
        assert!(
            self.sir_kind == SIRKind::SIR,
            "Cannot obtain SIR: IR already transformed"
        );
        Ok(res)
    }

    /// Try to get the LIR from the input source.
    /// Returns an error in case of compiler error.
    pub fn get_lir(&mut self) -> SRCCResult<OperationID> {
        if self.root.is_none() {
            self.root = Some(self._convert_sir_to_lir());
            self.sir_kind = SIRKind::LIR;
        }

        self.root.as_ref().unwrap().clone()
    }

    /// Try to emit the CAST IR output.
    pub fn emit_cast(&mut self, os: OutputTarget) -> SRCCResult<()> {
        let cast = self.get_cast()?;
        Self::_dump_cast(cast, os);
        Ok(())
    }

    /// Try to emit the SIR output.
    pub fn emit_sir(&mut self, os: OutputTarget) -> SRCCResult<()> {
        let root = self.get_sir()?;
        self._dump_sir(root, os);
        Ok(())
    }

    /// Try to emit the LIR output.
    pub fn emit_lir(&mut self, os: OutputTarget) -> SRCCResult<()> {
        let root = self.get_lir()?;
        self._dump_sir(root, os);
        Ok(())
    }

    /// Destroy the runner and take ownership of the context.
    pub fn take_context(self) -> IRContext {
        self.ctx
    }

    fn _convert_source_to_cast(&self) -> SRCCResult<ASTNode> {
        // Run the parser.
        let parser = CParser::new(self.ss.open_main_stream());
        let ast = parser.parse();
        let mut ast = match ast.resolve(CompilerInputs::Sources(&self.ss)) {
            Some(ast) => ast,
            None => return Err(format!("Failed to parse c input source")),
        };

        // Run the binder.
        let binder = ASTBinder::new();
        if binder
            .bind(&mut ast)
            .resolve(CompilerInputs::Sources(&self.ss))
            .is_none()
        {
            return Err(format!("Failed to typecheck c input file"));
        }

        if self.opts.dump_cast {
            Self::_dump_cast(&ast, OutputTarget::Stdout);
        }

        Ok(ast)
    }

    fn _convert_cast_to_sir(&mut self) -> SRCCResult<OperationID> {
        // Get the cast.
        self.get_cast()?;
        let cast = self.cast.as_ref().unwrap().as_ref().unwrap();

        let convert_opts = CASTToSIROptions::new();
        let converter = ASTToSIR::new(convert_opts);
        let root = match converter
            .convert_to_sir(cast, &mut self.ctx)
            .resolve(CompilerInputs::Sources(&self.ss))
        {
            Some(root) => root,
            None => return Err(format!("Failed to lower CAST to SIR")),
        };

        if self.opts.dump_sir {
            self._dump_sir(root, OutputTarget::Stdout);
        }

        Ok(root)
    }

    fn _convert_sir_to_lir(&mut self) -> SRCCResult<OperationID> {
        // Get the SIR.
        let root = self.get_sir_root()?;
        if self.sir_kind == SIRKind::LIR {
            // If it's already LIR nothing is required.
            return Ok(root);
        }
        assert!(
            self.sir_kind == SIRKind::SIR,
            "Cannot obtain LIR: IR already transformed"
        );

        // Prepare the passes.
        let pm_opts = PassManagerOptions::new();
        let mut pm = PassManager::new(pm_opts, &self.cs);
        create_sir_to_lir_pipeline(&mut pm);

        // Run the passes.
        let pm_runner = pm.make_runner();
        if pm_runner
            .run_all(&mut self.ctx, root)
            .resolve(CompilerInputs::Sources(&self.ss))
            .is_none()
        {
            return Err(format!("Failed to lower SIR to LIR"));
        };

        Ok(root)
    }

    fn _dump_cast(cast: &ASTNode, os: OutputTarget) {
        match os {
            OutputTarget::Stdout => cast.dump_to(&mut std::io::stdout()),
            OutputTarget::Stderr => cast.dump_to(&mut std::io::stderr()),
            OutputTarget::File(path) => {
                let os = File::create(&path)
                    .expect(&format!("Unable to create output file `{:?}`", &path));
                let mut os = BufWriter::new(os);
                cast.dump_to(&mut os)
            }
        };
    }

    fn _dump_sir(&self, root: OperationID, os: OutputTarget) {
        let op = self.ctx.get_generic_operation(root);
        let printer_opts = IRPrinterOptions::new();
        let mut printer = IRPrinter::new(printer_opts, os.to_dyn_stream());
        printer.print_root(&op).unwrap();
    }
}

/// Helper enum to indicate the compiler output (stream / file).
#[derive(Debug, Clone)]
pub enum OutputTarget {
    Stdout,
    Stderr,
    File(PathBuf),
}

impl OutputTarget {
    fn to_dyn_stream(self) -> Box<dyn std::io::Write> {
        match self {
            OutputTarget::Stdout => Box::new(std::io::stdout()),
            OutputTarget::Stderr => Box::new(std::io::stderr()),
            OutputTarget::File(path) => {
                let os = File::create(&path)
                    .expect(&format!("Unable to create output file `{:?}`", &path));
                Box::new(BufWriter::new(os))
            }
        }
    }
}

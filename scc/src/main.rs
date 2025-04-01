use std::env;

use cast::parser::CParser;
use diagnostics::CompilerDiagnosticsEmitter;
use iostreams::source_streams_set::SourceStreamsSet;

fn main() {
    let args: Vec<_> = env::args().collect();
    assert!(args.len() == 3);

    let src_file = args[2].clone();

    // Prepare the args.
    let mut ss = SourceStreamsSet::new();
    let src_file = ss.add_source_file(&src_file);

    // Parse the file
    let mut parser = CParser::new(ss.open_stream(src_file));
    let ast = parser.parse();
    let mut diagnostics = parser.take_diagnostics();
    diagnostics.resolve(&ss);

    // Dump the IR.
    ast.dump();
}

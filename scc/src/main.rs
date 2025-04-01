use std::{env, process::exit};

use cast::parser::CParser;
use diagnostics::diagnostics::CompilerInputs;
use iostreams::source_streams_set::SourceStreamsSet;

fn main() {
    let args: Vec<_> = env::args().collect();
    assert!(args.len() == 3);

    let src_file = args[2].clone();

    // Prepare the args.
    let mut ss = SourceStreamsSet::new();
    let src_file = ss.add_source_file(&src_file);

    // Parse the file
    let parser = CParser::new(ss.open_stream(src_file));
    let ast = parser.parse();
    let ast = match ast.resolve(CompilerInputs::Sources(&ss)) {
        Some(ast) => ast,
        None => exit(1),
    };

    // Dump the IR.
    ast.dump();
}

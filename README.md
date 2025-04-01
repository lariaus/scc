# SCC

SCC is a Compiler Framework written in Rust, and inspired by LLVM / MLIR.
This is designed for education purposes, and is far from reaching the quality of a real world compiler.
It can compile a subset of the C language.

## File organization

### cast (library)

CAST is the C Parser library, it takes a C input file and outputs SIR.

### diagnostics (library)

Helper library to properly emit and handle compiler diagnostics (log / warning / error).

### iostreams (library)

Helper library to easily manipulate Input / Output source code streams.

### parse (library)

Helper library to implement a source code parser.

### scc (binary)

C Compiler binary. 
Does nothing today.
In the end it should mimic the behavior of GCC.

### sir_core

Core part of the SIR library.
SIR is a Multi Level IR library, similar to LLVM
Define all basic systems behind the IR / Context.

### sir_arith

Not done yet.
Define all arith ops.

### xtest

Helper library to write unit tests.
This is inspired from LLVM FileCheck tool, but integrated directly with the rust testing system.
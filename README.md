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
SIR is a Multi Level IR library, similar to LLVM / MLIR
Define all basic systems behind the IR / Context.

### sir_func

Part of the SIR library.
Define Function related ops.

### sir_lir

Part of the SIR Library.
LIR Or Low IR defines Low Level operations.

### sir_math

Part of the SIR Library.
Define mathematic-like ops.

### sir-opt (binary)

Test the SIR compiler optimization passes manually.
Usage:

```shell
cargo run --bin sir-opt -- -i sir_math/tests/test_iadd_i32.sir
```

### xgen

XGen is a python library used to generate rust code for all the op defs.
It's the equivalent of TableGEN in MLIR / LLVM.
All definitions are written using special comments in the source file.
ALL XGEN is doing is reading these comments and transforming them to a "JSON" representation.
Then special hooks for SIR transform those JSON representation into actual rust code.

### xtest

Helper library to write unit tests.
This is inspired from LLVM FileCheck tool, but integrated directly with the rust testing system.

## Experiments / Tests

### Generate LLVM IR from C

clang -O3 tests/test_add.c -S -emit-llvm -o tests/out.ll && cat tests/out.ll
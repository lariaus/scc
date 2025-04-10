# SCC

SCC is a Compiler Framework written in Rust, and inspired by LLVM / MLIR.
This is designed for education purposes, and is far from reaching the quality of a real world compiler.
It can compile a subset of the C language.

## File organization

### cast

CAST is the C Parser library, it takes a C input file and outputs SIR.

### diagnostics

Helper library to properly emit and handle compiler diagnostics (log / warning / error).

### iostreams

Helper library to easily manipulate Input / Output source code streams.

### parse

Helper library to implement a source code parser.

### scclib

Library using cast / sir to compile c source files to IR / ASM / binary.
Today it only support emitting IR.

### scc (binary)

C Compiler binary.
Today it can only emit CAST / SIR / LIR.
In the end it should mimic the behavior of GCC.

Example: emit `LIR` from C source file:
```bash
cargo run --bin scc -- tests/test_basic_add.c --emit-lir
```

### scc-runner (binary)

Binary to directly run C programs from the source file (for now only the interpreter is supported).

Example: run a C function using the interpreter:
```bash
cargo run --bin scc-runner -- tests/test_basic_add.c --function my_add --inputs 8 13 --mode interpreter
```

### sir_backend

Library that handles all compiler steps to go from SIR to assembly code.
This library only defines the backend primitives and algorithms.
Each backend is implemented in their independant library.

### sir_backend_arm_v86a

SIR backend for Armv8.6-A architecture.
This can be used to compile files to run on macos.

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

### sir_mem

Part of the SIR Library.
Define high-level ops related to memory (load / store / alloc / pointer ops)

### sir_opt

Library to easily run SIR transforms / passes through the 

### sir-opt (binary)

Test the SIR compiler optimization passes manually.

Example: 
```bash
cargo run --bin sir-opt -- -i sir_math/tests/test_iadd_i32.sir --legalize-to-low-level --print-ir-before-all
```

### sir_pipelines

Library with several SIR pipelines to control general lowering / backend of IR.

### sir_runner

Test Library to run SIR programs (for now only the interpreter is supported).

### sir-runner (binary)

Binary to run SIR programs (for now only the interpreter is supported).

Example: run a SIR function using the interpreter:
```bash
cargo run --bin sir-runner -- "tests/test_interpret_iadd.sir" --function "foo" --inputs 8 13 --mode interpreter
```

### sir_test_ops

Library that contains several SIR Ops / passes / transforms for testing purposes.
This lives in its own directory as it tests different sir libraries.
It also contains good examples of how to work with sir.

### sir_transforms

Library containing many tools needed to translate / transform / optimize SIR code.
Defines also the whole system of passes / PassManager.


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

### Generate LLVM IR from C (clang)

clang -O3 tests/test_add.c -S -emit-llvm -o tests/out.ll

### Generate ASM from C (clang)

clang -O0 tests/test_basic_add.c -S -o tests/out.s

// Compile
// cargo run --bin scc -- tests/test_basic_add.c --emit-cast
// cargo run --bin scc -- tests/test_basic_add.c --emit-sir
// cargo run --bin scc -- tests/test_basic_add.c --emit-lir

// Execute
// cargo run --bin scc-runner -- tests/test_basic_add.c --function my_add --inputs 8 13 --mode interpreter

int my_add(int x, int y) {
    return x + y;
}
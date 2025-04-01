// XTEST: cparser
// XTEST-OUTPUT-CHECK

// CHECK: Parser: Error: Expected identifier but got `return` at {{.*}}:5:20-25:
int foo(int x, int return y) {
    return 5;
} 
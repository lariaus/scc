// XTEST: cparser
// XTEST-OUTPUT-CHECK

// CHECK: function @foo(x: int, y: int) -> int
// CHECK-NEXT: {
// CHECK-NEXT: return 5;
// CHECK-MEXT: }
int foo(int x, int y) {
    return 5;
} 
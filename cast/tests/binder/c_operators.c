// XTEST: cast --bind-ast
// XTEST-OUTPUT-CHECK

// CHECK: function @binop_add(x: int, y: int) -> int
// CHECK-NEXT: {
// CHECK-NEXT: return OP<Add>(x, y);
// CHECK-MEXT: }
int binop_add(int x, int y) {
    return x + y;
} 
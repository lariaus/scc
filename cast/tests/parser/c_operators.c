// XTEST: cast
// XTEST-OUTPUT-CHECK

// CHECK: function @binop_add(x: int, y: int) -> int
// CHECK-NEXT: {
// CHECK-NEXT: return OP<Add>(x, y);
// CHECK-MEXT: }
int binop_add(int x, int y) {
    return x + y;
} 

// CHECK: function @add3(x: int, y: int, z: int) -> int
// CHECK: return OP<Add>(OP<Add>(x, y), z);
int add3(int x, int y, int z) {
    return x + y + z;
} 
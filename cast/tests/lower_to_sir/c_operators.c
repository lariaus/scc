// XTEST: cast --lower-to-sir
// XTEST-OUTPUT-CHECK

// CHECK: function @binop_add(%arg0: i32, %arg1: i32) -> (i32) {
// CHECK-NEXT: %0 = "mem.alloca"() {"align" = 4: i64} : () -> (ptr<i32>)
// CHECK-NEXT: "mem.store"(%arg0, %0) : (i32, ptr<i32>) -> ()
// CHECK-NEXT: %1 = "mem.alloca"() {"align" = 4: i64} : () -> (ptr<i32>)
// CHECK-NEXT: "mem.store"(%arg1, %1) : (i32, ptr<i32>) -> ()
// CHECK-NEXT: %2 = "mem.load"(%0) : (ptr<i32>) -> (i32)
// CHECK-NEXT: %3 = "mem.load"(%1) : (ptr<i32>) -> (i32)
// CHECK-NEXT: %4 = math.iadd %2, %3 : i32
// CHECK-NEXT: return %4 : i32
int binop_add(int x, int y) {
    return x + y;
} 
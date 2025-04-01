// XTEST: cparser
// XTEST-OUTPUT-CHECK

// CHECK: function @var_no_init
// CHECK: var x: int;
void var_no_init() {
    int x;
} 

// CHECK: function @var_with_init
// CHECK: var x: int = 3;
void var_with_init() {
    int x = 3;
} 

// CHECK: function @multi_vars_def
// CHECK: var x: int;
// CHECK-NEXT: var y: int;
// CHECK-NEXT: var z: int;
void multi_vars_def() {
    int x, y, z;
}

// CHECK: function @multi_vars_defs_with_inits
// CHECK: var x: int = 45;
// CHECK-NEXT: var y: int;
// CHECK-NEXT: var z: int = 12;
void multi_vars_defs_with_inits() {
    int x = 45, y, z = 12;
}

// CHECK: function @multi_arrs_vars_defs
// CHECK: var arr0: int[4];
// CHECK-NEXT: var arr1: int;
// CHECK-NEXT: var arr2: int[3][6];
void multi_arrs_vars_defs() {
    int arr0[4], arr1, arr2[3][6];
}

// CHECK: function @var_defs_all_types
void var_defs_all_types() {
    // Integer types.
    // CHECK: var v0: char;
    char v0;
    // CHECK-NEXT: var v1: unsigned char;
    unsigned char v1;
    // CHECK-NEXT: var v2: signed char;
    signed char v2;
    // CHECK: var v3: short;
    short v3;
    // CHECK-NEXT: var v4: unsigned short;
    unsigned short v4;
    // CHECK-NEXT: var v5: signed short;
    signed short v5;
    // CHECK: var v6: int;
    int v6;
    // CHECK-NEXT: var v7: unsigned int;
    unsigned int v7;
    // CHECK-NEXT: var v8: signed int;
    signed int v8;
    // CHECK: var v9: long;
    long v9;
    // CHECK-NEXT: var v10: unsigned long;
    unsigned long v10;
    // CHECK-NEXT: var v11: signed long;
    signed long v11;

    // Float types.
    // CHECK: var v12: float;
    float v12;
    // CHECK: var v13: double;
    double v13;

    // TODO: Support Const and Pointer types
    // const float v14;
    // float* v15;
    // float const* v16;

}
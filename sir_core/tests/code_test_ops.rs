use std::{cell::RefCell, ops::AddAssign};

use diagnostics::diagnostics::CompilerInputs;
use iostreams::location::Location;
use sir_core::{
    ir_builder::{IRBuilder, InsertionPoint},
    ir_context::IRContext,
    ir_data::OperationID,
    ir_parser::IRParsableObjectWithContext,
    ir_printer::IRPrintableObject,
    ir_verifier::verify_op,
    ir_visitor::{make_ir_visitor_mut, IRVisitor, IRVisitorMut, WalkOlder},
    operation::{GenericOperation, OperationImpl},
    types::{FloatType, FunctionType, Type},
};
use test_ops::{register_test_ops, TestAddOp, TestFunOp, TestReturnOp};

mod test_ops;

#[test]
fn test_fun_op() {
    let loc = Location::unknown_test_loc();
    let val_ty = Type::make_int(32, None);
    let mut ctx = IRContext::new();
    register_test_ops(&mut ctx);
    let mut b = IRBuilder::new(&mut ctx);

    let fun_ty = FunctionType::new(
        vec![val_ty.clone(), val_ty.clone(), val_ty.clone()],
        vec![val_ty.clone()],
    );
    let fun_op = b.create_op(loc, TestFunOp::_build("foo", fun_ty)).as_id();
    let block = b.create_block_at_begin_of(
        fun_op,
        loc,
        [val_ty.clone(), val_ty.clone(), val_ty.clone()],
    );
    let x = block.get_operand(0).as_id();
    let y = block.get_operand(1).as_id();
    let z = block.get_operand(2).as_id();
    let block = block.as_id();
    b.set_insertion_point(InsertionPoint::AtEndOf(block));

    let add0 = b.create_op(loc, TestAddOp::_build(x, y, val_ty.clone()));
    let add0_id = add0.as_id();
    let tmp0 = add0.get_result().as_id();

    let add1 = b.create_op(loc, TestAddOp::_build(tmp0, z, val_ty.clone()));
    let out = add1.get_result().as_id();
    assert_eq!(add1.get_lhs().as_id(), tmp0);
    assert_eq!(add1.get_rhs().as_id(), z);

    b.create_op(loc, TestReturnOp::_build(vec![out]));

    // We do this dance of convert to id then cast back just for testing cast.
    let add0 = ctx.get_generic_operation(add0_id);
    let add0 = add0.cast::<TestAddOp>().unwrap();
    assert_eq!(add0.get_lhs().as_id(), x);
    assert_eq!(add0.get_rhs().as_id(), y);

    let fun_op = ctx.get_generic_operation(fun_op);
    assert_eq!(fun_op.to_generic_form_string_repr(), "\"test.fun\"() {\"symbol_name\" = \"foo\", \"function_type\" = function<(i32, i32, i32) -> (i32)>} : () -> () {\n    ^(%arg0: i32, %arg1: i32, %arg2: i32) {\n        %0 = \"test.add\"(%arg0, %arg1) : (i32, i32) -> (i32)\n        %1 = \"test.add\"(%0, %arg2) : (i32, i32) -> (i32)\n        \"test.return\"(%1) : (i32) -> ()\n    }\n    \n}");
    assert_eq!(fun_op.to_string_repr(), "test.fun @foo(%arg0: i32, %arg1: i32, %arg2: i32) -> (i32) {\n    %0 = test.add %arg0, %arg1 : i32\n    %1 = test.add %0, %arg2 : i32\n    test.return %1 : i32\n}");

    // assert_eq!(3, 5);
}

#[test]
fn test_fun_op_parser() {
    let mut ctx = IRContext::new();
    register_test_ops(&mut ctx);
    let op = OperationID::from_string_repr_with_context(
        "test.fun @foo(%arg0: i32, %arg1: i32, %arg2: i32) -> (i32) {\n    %0 = test.add %arg0, %arg1 : i32\n    %1 = test.add %0, %arg2 : i32\n    test.return %1 : i32\n}".to_string(),
        &mut ctx,
    );
    let fun_op = ctx.get_generic_operation(op).cast::<TestFunOp>().unwrap();

    assert_eq!(fun_op.to_generic_form_string_repr(), "\"test.fun\"() {\"symbol_name\" = \"foo\", \"function_type\" = function<(i32, i32, i32) -> (i32)>} : () -> () {\n    ^(%arg0: i32, %arg1: i32, %arg2: i32) {\n        %0 = \"test.add\"(%arg0, %arg1) : (i32, i32) -> (i32)\n        %1 = \"test.add\"(%0, %arg2) : (i32, i32) -> (i32)\n        \"test.return\"(%1) : (i32) -> ()\n    }\n    \n}");
    assert_eq!(fun_op.to_string_repr(), "test.fun @foo(%arg0: i32, %arg1: i32, %arg2: i32) -> (i32) {\n    %0 = test.add %arg0, %arg1 : i32\n    %1 = test.add %0, %arg2 : i32\n    test.return %1 : i32\n}");

    // assert_eq!(3, 5);
}

struct AddOpVisitorCounter {
    // Use RefCell just to make it work with unmutable.
    // it's just for testing purposes.
    count: RefCell<usize>,
}

impl AddOpVisitorCounter {
    fn new() -> Self {
        Self {
            count: RefCell::new(0),
        }
    }
}

impl<'a> IRVisitor<'a, TestAddOp<'a>> for AddOpVisitorCounter {
    fn visit_op(&self, _op: TestAddOp<'a>) {
        self.count.borrow_mut().add_assign(1);
    }
}

struct GenericOpVisitorCounter {
    count: usize,
}

impl GenericOpVisitorCounter {
    fn new() -> Self {
        Self { count: 0 }
    }
}

impl<'a> IRVisitorMut<'a, GenericOperation<'a>> for GenericOpVisitorCounter {
    fn visit_op(&mut self, _op: GenericOperation<'a>) {
        self.count += 1;
    }
}

#[test]
fn test_ir_visitor() {
    let mut ctx = IRContext::new();
    register_test_ops(&mut ctx);
    let op = OperationID::from_string_repr_with_context(
        "test.fun @foo(%arg0: i32, %arg1: i32, %arg2: i32) {\n    %0 = test.add %arg0, %arg1 : i32\n    %1 = test.add %0, %arg2 : i32\n    test.return %1 : i32\n}".to_string(),
        &mut ctx,
    );
    let fun_op = ctx.get_generic_operation(op).cast::<TestFunOp>().unwrap();

    // Check with struct.
    let add_counter = AddOpVisitorCounter::new();
    fun_op.walk(WalkOlder::PreOrder, &add_counter);
    assert_eq!(*add_counter.count.borrow(), 2);

    // Check with struct and mut visior.
    let mut generic_counter = GenericOpVisitorCounter::new();
    fun_op.walk_mut(WalkOlder::PreOrder, &mut generic_counter);
    assert_eq!(generic_counter.count, 4);

    // Check with lambda and mut visitor.
    let mut add_count = 0;
    fun_op.walk_mut(
        WalkOlder::PreOrder,
        &mut make_ir_visitor_mut(|_op: TestAddOp| add_count += 1),
    );
    assert_eq!(add_count, 2);
}

#[test]
fn test_verify_use_before_def() {
    let mut ctx = IRContext::new();
    register_test_ops(&mut ctx);
    let loc = Location::unknown_test_loc();
    let val_ty = Type::make_int(32, None);
    let mut ctx = IRContext::new();
    register_test_ops(&mut ctx);
    let mut b = IRBuilder::new(&mut ctx);

    let fun_ty = FunctionType::new(
        vec![val_ty.clone(), val_ty.clone(), val_ty.clone()],
        vec![val_ty.clone()],
    );
    let fun_op = b.create_op(loc, TestFunOp::_build("foo", fun_ty)).as_id();
    let block = b.create_block_at_begin_of(
        fun_op,
        loc,
        [val_ty.clone(), val_ty.clone(), val_ty.clone()],
    );
    let x = block.get_operand(0).as_id();
    let y = block.get_operand(1).as_id();
    let block = block.as_id();
    b.set_insertion_point(InsertionPoint::AtBeginOf(block));

    let add0 = b.create_op(loc, TestAddOp::_build(x, y, val_ty.clone()));
    let tmp0 = add0.get_result().as_id();

    // Second add is inserted before first.
    let add1 = b.create_op(loc, TestAddOp::_build(x, tmp0, val_ty.clone()));
    let out = add1.get_result().as_id();

    b.set_insertion_point(InsertionPoint::AtEndOf(block));
    b.create_op(loc, TestReturnOp::_build(vec![out]));

    assert_eq!(verify_op(ctx.get_generic_operation(fun_op)).diagnostics_to_string(CompilerInputs::Unknown),
    "IR Verifier: Error: Value definition of operation input #1 doesn't dominate its use at I#0:0:0\n  See %0 = \"test.add\"(%arg0, <<UNKNOWN>) : (i32, i32) -> (i32).\n  Value definition is at I#0:0:0\n    See %1 = \"test.add\"(%arg0, %arg1) : (i32, i32) -> (i32).\n");
}

#[test]
fn test_verify_invalid_type() {
    let mut ctx = IRContext::new();
    register_test_ops(&mut ctx);
    let loc = Location::unknown_test_loc();
    let val_ty = Type::Float(FloatType::F32);
    let mut ctx = IRContext::new();
    register_test_ops(&mut ctx);
    let mut b = IRBuilder::new(&mut ctx);

    let fun_ty = FunctionType::new(
        vec![val_ty.clone(), val_ty.clone(), val_ty.clone()],
        vec![val_ty.clone()],
    );
    let fun_op = b.create_op(loc, TestFunOp::_build("foo", fun_ty)).as_id();
    let block = b.create_block_at_begin_of(
        fun_op,
        loc,
        [val_ty.clone(), val_ty.clone(), val_ty.clone()],
    );
    let x = block.get_operand(0).as_id();
    let y = block.get_operand(1).as_id();
    let block = block.as_id();
    b.set_insertion_point(InsertionPoint::AtEndOf(block));

    let res = b
        .create_op(loc, TestAddOp::_build(x, y, val_ty.clone()))
        .get_result()
        .as_id();
    b.create_op(loc, TestReturnOp::_build(vec![res]));

    assert_eq!(verify_op(ctx.get_generic_operation(fun_op)).diagnostics_to_string(CompilerInputs::Unknown),
    "IRVerifier: Error: Input #0 (lhs) must be of type `Integer`, but got f32 at I#0:0:0\n  See %0 = \"test.add\"(%arg0, %arg1) : (f32, f32) -> (f32).\nIRVerifier: Error: Input #1 (rhs) must be of type `Integer`, but got f32 at I#0:0:0\n  See %0 = \"test.add\"(%arg0, %arg1) : (f32, f32) -> (f32).\nIRVerifier: Error: Output #0 (result) must be of type `Integer`, but got f32 at I#0:0:0\n  See %0 = \"test.add\"(%arg0, %arg1) : (f32, f32) -> (f32).\n");
}

#[test]
fn test_verify_fun_invalid_operand_tye() {
    let mut ctx = IRContext::new();
    register_test_ops(&mut ctx);
    let loc = Location::unknown_test_loc();
    let val_fp_ty = Type::Float(FloatType::F32);
    let val_ty = Type::make_int(32, None);
    let mut ctx = IRContext::new();
    register_test_ops(&mut ctx);
    let mut b = IRBuilder::new(&mut ctx);

    let fun_ty = FunctionType::new(vec![val_fp_ty], vec![val_ty.clone()]);
    let fun_op = b.create_op(loc, TestFunOp::_build("foo", fun_ty)).as_id();
    let block = b.create_block_at_begin_of(fun_op, loc, [val_ty.clone()]);
    let x = block.get_operand(0).as_id();
    let block = block.as_id();
    b.set_insertion_point(InsertionPoint::AtEndOf(block));

    let res = b
        .create_op(loc, TestAddOp::_build(x, x, val_ty.clone()))
        .get_result()
        .as_id();
    b.create_op(loc, TestReturnOp::_build(vec![res]));

    assert_eq!(verify_op(ctx.get_generic_operation(fun_op)).diagnostics_to_string(CompilerInputs::Unknown),
    "IRVerifier: Error: Signature type mismatch: block operand #0 has type i32 but function signature operand type is f32 at I#0:0:0\n  See \"test.fun\"() {\"symbol_name\" = \"foo\", \"function_type\" = function<(f32) -> (i32)>} : () -> () {\n    ^(%arg0: i32) {\n        %0 = \"test.add\"(%arg0, %arg0) : (i32, i32) -> (i32)\n        \"test.return\"(%0) : (i32) -> ()\n    }\n    \n}.\n");
}

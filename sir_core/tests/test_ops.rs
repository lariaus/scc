use std::{cell::RefCell, ops::AddAssign};

use iostreams::location::Location;
use parse::lexer::TokenValue;
use parse::parser::Parser;
use sir::{
    block::Block,
    ir_builder::{IRBuilder, InsertionPoint, OpImplBuilderState},
    ir_context::IRContext,
    ir_data::{OperationData, OperationID, ValueID},
    ir_parser::{
        BlockParserState, IRParsableObject, IRParsableObjectWithContext, IRParser,
        OperationParserState,
    },
    ir_printer::{IRPrintableObject, IRPrinter},
    ir_visitor::{IRVisitor, WalkOlder},
    op_interfaces::BuiltinOpInterfaceWrapper,
    operation::{print_op_dispatch, GenericOperation, OperationImpl},
    operation_type::{OperationTypeBuilder, OperationTypeUID},
    types::Type,
    value::Value,
};

/////////////////////////////////////////////////////////////////////////
// TestFunOp implementation
/////////////////////////////////////////////////////////////////////////

pub struct TestFunOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl TestFunOp<'_> {
    pub fn build() -> OpImplBuilderState<Self> {
        let st = OpImplBuilderState::make();
        st
    }
}

const TEST_FUN_OPNAME: &'static str = "test.fun";
const TEST_FUN_TYPE_UID: OperationTypeUID = OperationTypeUID::make_from_opname(TEST_FUN_OPNAME);

impl<'a> OperationImpl<'a> for TestFunOp<'a> {
    fn make_from_data(ctx: &'a IRContext, data: &'a OperationData) -> Self {
        Self { ctx, data }
    }

    fn get_op_data(&self) -> &'a OperationData {
        self.data
    }

    fn get_context(&self) -> &'a IRContext {
        self.ctx
    }

    fn get_op_type_uid() -> OperationTypeUID {
        TEST_FUN_TYPE_UID
    }

    fn verify(&self) {
        todo!("verify TestFunOp");
    }

    fn custom_print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        let body = self.get_body();
        // Print the arguments
        write!(printer.os(), "(")?;
        for (idx, arg) in body.get_operands().enumerate() {
            printer.assign_and_print_value_label(arg.as_id(), Some("arg"))?;
            write!(printer.os(), ": ")?;
            printer.print(arg.get_type())?;
            if idx + 1 < body.get_num_operands() {
                write!(printer.os(), ", ")?;
            }
        }
        write!(printer.os(), ") {{")?;

        // Print the body
        printer.print_block_content(body)?;
        write!(printer.os(), "}}")
    }

    fn custom_parse(
        parser: &mut IRParser,
        ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        let mut block_st = BlockParserState::new();
        // Parse the arguments.
        let loc_beg = parser.get_next_token_loc();
        let mut operands_names = vec![];
        let mut operands_types = vec![];
        parser.consume_sym_or_error(TokenValue::sym_lparen())?;
        if !parser.next_token_is_sym(TokenValue::sym_rparen()) {
            loop {
                operands_names.push(parser.parse_value_ref()?);
                parser.consume_sym_or_error(TokenValue::sym_colon())?;
                operands_types.push(Type::parse(parser)?);
                if !parser.try_consume_sym(TokenValue::sym_comma()) {
                    break;
                }
            }
        }
        parser.consume_sym_or_error(TokenValue::sym_rparen())?;
        block_st.set_operands_names(operands_names);
        block_st.set_operands_types(operands_types);

        // Parse the body
        let loc_end = parser
            .consume_sym_or_error(TokenValue::sym_lcbracket())?
            .loc();
        block_st.set_loc(Location::join(loc_beg, loc_end));
        st.set_end_loc(loc_end);
        let block = parser.make_block_from_state(block_st, ctx, TokenValue::sym_rcbracket())?;

        // Set the block
        st.set_blocks(vec![block]);
        Some(())
    }
}

impl<'a> TestFunOp<'a> {
    fn get_body(&self) -> Block<'a> {
        self.get_block(0)
    }
}

// TODO: Find a way to get rid of this / generate this.
#[derive(Default)]
pub struct TestFunOpBuiltinInterfaceWrapperImpl;
impl BuiltinOpInterfaceWrapper for TestFunOpBuiltinInterfaceWrapperImpl {
    fn verify(&self, ctx: &IRContext, op: OperationID) {
        let op = TestFunOp::make_from_data(ctx, ctx.get_operation_data(op));
        op.verify()
    }

    fn custom_print(
        &self,
        printer: &mut sir::ir_printer::IRPrinter,
        ctx: &IRContext,
        op: OperationID,
    ) -> Result<(), std::io::Error> {
        let use_custom_printer = true;
        print_op_dispatch(
            TestFunOp::make_from_data(ctx, ctx.get_operation_data(op)),
            printer,
            use_custom_printer,
        )
    }

    fn custom_parse(
        &self,
        parser: &mut sir::ir_parser::IRParser,
        ctx: &mut IRContext,
        st: &mut sir::ir_parser::OperationParserState,
    ) -> Option<()> {
        TestFunOp::custom_parse(parser, ctx, st)
    }

    fn clone(&self) -> Box<dyn BuiltinOpInterfaceWrapper> {
        Box::new(TestFunOpBuiltinInterfaceWrapperImpl {})
    }
}

fn register_test_fun_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(TEST_FUN_OPNAME);
    infos.set_impl::<TestFunOp>();
    infos.set_builtin_interface::<TestFunOpBuiltinInterfaceWrapperImpl>();
    ctx.register_operation(infos.build());
}

/////////////////////////////////////////////////////////////////////////
// TestAddOp implementation
/////////////////////////////////////////////////////////////////////////

pub struct TestAddOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl TestAddOp<'_> {
    pub fn build<T0: Into<ValueID>, T1: Into<ValueID>>(
        lhs: T0,
        rhs: T1,
        out: Type,
    ) -> OpImplBuilderState<Self> {
        let mut st = OpImplBuilderState::make();
        st.set_inputs(vec![lhs.into(), rhs.into()]);
        st.set_outputs_types(vec![out]);
        st
    }
}

impl<'a> TestAddOp<'a> {
    pub fn lhs(&self) -> Value<'a> {
        self.get_input(0)
    }
    pub fn rhs(&self) -> Value<'a> {
        self.get_input(1)
    }
    pub fn result(&self) -> Value<'a> {
        self.get_output(0)
    }
}

const TEST_ADD_OPNAME: &'static str = "test.add";
const TEST_ADD_TYPE_UID: OperationTypeUID = OperationTypeUID::make_from_opname(TEST_ADD_OPNAME);

impl<'a> OperationImpl<'a> for TestAddOp<'a> {
    fn make_from_data(ctx: &'a IRContext, data: &'a OperationData) -> Self {
        Self { ctx, data }
    }

    fn get_op_type_uid() -> OperationTypeUID {
        TEST_ADD_TYPE_UID
    }

    fn get_op_data(&self) -> &'a OperationData {
        self.data
    }

    fn get_context(&self) -> &'a IRContext {
        self.ctx
    }

    fn verify(&self) {
        todo!("verify TestAdd");
    }

    fn custom_print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        printer.print_value_label_or_unknown(self.lhs().into())?;
        write!(printer.os(), ", ")?;
        printer.print_value_label_or_unknown(self.rhs().into())?;
        write!(printer.os(), " : ")?;
        printer.print(self.result().get_type())
    }

    fn custom_parse(
        parser: &mut IRParser,
        _ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        // Parse the two op inputs.
        let mut inputs_names = vec![];
        inputs_names.push(parser.parse_value_ref()?);
        parser.consume_sym_or_error(TokenValue::sym_comma())?;
        inputs_names.push(parser.parse_value_ref()?);

        // Parse the type (same for I/Os).
        let mut inputs_types = vec![];
        let mut outputs_types = vec![];
        parser.consume_sym_or_error(TokenValue::sym_colon())?;
        let ty = Type::parse(parser)?;
        st.set_end_loc(parser.get_next_token_loc());
        inputs_types.push(ty.clone());
        inputs_types.push(ty.clone());
        outputs_types.push(ty);
        st.set_inputs_names(inputs_names);
        st.set_inputs_types(inputs_types);
        st.set_outputs_types(outputs_types);
        Some(())
    }
}

// TODO: Find a way to get rid of this / generate this.
#[derive(Default)]
pub struct TestAddOpBuiltinInterfaceWrapperImpl;
impl BuiltinOpInterfaceWrapper for TestAddOpBuiltinInterfaceWrapperImpl {
    fn verify(&self, ctx: &IRContext, op: OperationID) {
        let op = TestAddOp::make_from_data(ctx, ctx.get_operation_data(op));
        op.verify()
    }

    fn custom_print(
        &self,
        printer: &mut sir::ir_printer::IRPrinter,
        ctx: &IRContext,
        op: OperationID,
    ) -> Result<(), std::io::Error> {
        let use_custom_printer = true;
        print_op_dispatch(
            TestAddOp::make_from_data(ctx, ctx.get_operation_data(op)),
            printer,
            use_custom_printer,
        )
    }

    fn custom_parse(
        &self,
        parser: &mut sir::ir_parser::IRParser,
        ctx: &mut IRContext,
        st: &mut sir::ir_parser::OperationParserState,
    ) -> Option<()> {
        TestAddOp::custom_parse(parser, ctx, st)
    }

    fn clone(&self) -> Box<dyn BuiltinOpInterfaceWrapper> {
        Box::new(TestAddOpBuiltinInterfaceWrapperImpl {})
    }
}

fn register_test_add_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(TEST_ADD_OPNAME);
    infos.set_impl::<TestAddOp>();
    infos.set_builtin_interface::<TestAddOpBuiltinInterfaceWrapperImpl>();
    ctx.register_operation(infos.build());
}

/////////////////////////////////////////////////////////////////////////
// TestReturnOp implementation
/////////////////////////////////////////////////////////////////////////

pub struct TestReturnOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl TestReturnOp<'_> {
    pub fn build<T: Into<Vec<ValueID>>>(operands: T) -> OpImplBuilderState<Self> {
        let mut st = OpImplBuilderState::make();
        st.set_inputs(operands);
        st
    }
}

const TEST_RETURN_OPNAME: &'static str = "test.return";
const TEST_RETURN_TYPE_UID: OperationTypeUID =
    OperationTypeUID::make_from_opname(TEST_RETURN_OPNAME);

impl<'a> OperationImpl<'a> for TestReturnOp<'a> {
    fn make_from_data(ctx: &'a IRContext, data: &'a OperationData) -> Self {
        Self { ctx, data }
    }

    fn get_op_type_uid() -> OperationTypeUID {
        TEST_RETURN_TYPE_UID
    }

    fn get_op_data(&self) -> &'a OperationData {
        self.data
    }

    fn get_context(&self) -> &'a IRContext {
        self.ctx
    }

    fn verify(&self) {
        todo!("verify TestReturn");
    }

    fn custom_print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        // Print the return names.
        for (idx, val) in self.get_inputs().enumerate() {
            printer.print_value_label_or_unknown(val.into())?;
            if idx + 1 < self.get_num_outputs() {
                write!(printer.os(), ", ")?;
            }
        }
        write!(printer.os(), " : ")?;

        // Print the return types.
        for (idx, ty) in self.get_inputs_types().enumerate() {
            printer.print(ty)?;
            if idx + 1 < self.get_num_outputs() {
                write!(printer.os(), ", ")?;
            }
        }

        Ok(())
    }

    fn custom_parse(
        parser: &mut IRParser,
        _ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        // TODO: Support empty return.

        // Parse the operands names
        let mut inputs_names = vec![];
        loop {
            inputs_names.push(parser.parse_value_ref()?);
            if !parser.try_consume_sym(TokenValue::sym_comma()) {
                break;
            }
        }
        parser.consume_sym_or_error(TokenValue::sym_colon())?;

        // Parse operands types.
        let mut inputs_types = vec![];
        for idx in 0..inputs_names.len() {
            inputs_types.push(Type::parse(parser)?);
            if idx + 1 < inputs_names.len() {
                parser.consume_sym_or_error(TokenValue::sym_comma())?;
            }
        }

        st.set_end_loc(parser.get_next_token_loc());
        st.set_inputs_names(inputs_names);
        st.set_inputs_types(inputs_types);
        Some(())
    }
}

// TODO: Find a way to get rid of this / generate this.
#[derive(Default)]
pub struct TestReturnOpBuiltinInterfaceWrapperImpl;
impl BuiltinOpInterfaceWrapper for TestReturnOpBuiltinInterfaceWrapperImpl {
    fn verify(&self, ctx: &IRContext, op: OperationID) {
        let op = TestReturnOp::make_from_data(ctx, ctx.get_operation_data(op));
        op.verify()
    }

    fn custom_print(
        &self,
        printer: &mut sir::ir_printer::IRPrinter,
        ctx: &IRContext,
        op: OperationID,
    ) -> Result<(), std::io::Error> {
        let use_custom_printer = true;
        print_op_dispatch(
            TestReturnOp::make_from_data(ctx, ctx.get_operation_data(op)),
            printer,
            use_custom_printer,
        )
    }

    fn custom_parse(
        &self,
        parser: &mut sir::ir_parser::IRParser,
        ctx: &mut IRContext,
        st: &mut sir::ir_parser::OperationParserState,
    ) -> Option<()> {
        TestReturnOp::custom_parse(parser, ctx, st)
    }

    fn clone(&self) -> Box<dyn BuiltinOpInterfaceWrapper> {
        Box::new(TestReturnOpBuiltinInterfaceWrapperImpl {})
    }
}

fn register_test_return_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(TEST_RETURN_OPNAME);
    infos.set_impl::<TestReturnOp>();
    infos.set_builtin_interface::<TestReturnOpBuiltinInterfaceWrapperImpl>();
    ctx.register_operation(infos.build());
}

/////////////////////////////////////////////////////////////////////////
// TestOps implementations
/////////////////////////////////////////////////////////////////////////

pub fn register_test_ops(ctx: &mut IRContext) {
    register_test_fun_op(ctx);
    register_test_add_op(ctx);
    register_test_return_op(ctx);
}

#[test]
fn test_fun_op() {
    let loc = Location::unknown_test_loc();
    let val_ty = Type::make_int(32, None);
    let mut ctx = IRContext::new();
    register_test_ops(&mut ctx);
    let mut b = IRBuilder::new(&mut ctx);

    // let fun_op = b.create_op(loc, TestFunOp::build()).as_id();
    let fun_op = b.create_op(loc, TestFunOp::build()).as_id();
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

    let add0 = b.create_op(loc, TestAddOp::build(x, y, val_ty.clone()));
    let add0_id = add0.as_id();
    let tmp0 = add0.result().as_id();

    let add1 = b.create_op(loc, TestAddOp::build(tmp0, z, val_ty.clone()));
    let out = add1.result().as_id();
    assert_eq!(add1.lhs().as_id(), tmp0);
    assert_eq!(add1.rhs().as_id(), z);

    b.create_op(loc, TestReturnOp::build(vec![out]));

    // We do this dance of convert to id then cast back just for testing cast.
    let add0 = ctx.get_generic_operation(add0_id);
    let add0 = add0.cast::<TestAddOp>().unwrap();
    assert_eq!(add0.lhs().as_id(), x);
    assert_eq!(add0.rhs().as_id(), y);

    let fun_op = ctx.get_generic_operation(fun_op);
    assert_eq!(fun_op.to_generic_form_string_repr(), "\"test.fun\"() : () -> () {\n    ^(%arg0: i32, %arg1: i32, %arg2: i32) {\n        %0 = \"test.add\"(%arg0, %arg1) : (i32, i32) -> (i32)\n        %1 = \"test.add\"(%0, %arg2) : (i32, i32) -> (i32)\n        \"test.return\"(%1) : (i32) -> ()\n    }\n    \n}");
    assert_eq!(fun_op.to_string_repr(), "test.fun (%arg0: i32, %arg1: i32, %arg2: i32) {\n    %0 = test.add %arg0, %arg1 : i32\n    %1 = test.add %0, %arg2 : i32\n    test.return %1 : i32\n}");

    // assert_eq!(3, 5);
}

#[test]
fn test_fun_op_parser() {
    let mut ctx = IRContext::new();
    register_test_ops(&mut ctx);
    let op = OperationID::from_string_repr_with_context(
        "test.fun (%arg0: i32, %arg1: i32, %arg2: i32) {\n    %0 = test.add %arg0, %arg1 : i32\n    %1 = test.add %0, %arg2 : i32\n    test.return %1 : i32\n}".to_string(),
        &mut ctx,
    );
    let fun_op = ctx.get_generic_operation(op).cast::<TestFunOp>().unwrap();

    assert_eq!(fun_op.to_generic_form_string_repr(), "\"test.fun\"() : () -> () {\n    ^(%arg0: i32, %arg1: i32, %arg2: i32) {\n        %0 = \"test.add\"(%arg0, %arg1) : (i32, i32) -> (i32)\n        %1 = \"test.add\"(%0, %arg2) : (i32, i32) -> (i32)\n        \"test.return\"(%1) : (i32) -> ()\n    }\n    \n}");
    assert_eq!(fun_op.to_string_repr(), "test.fun (%arg0: i32, %arg1: i32, %arg2: i32) {\n    %0 = test.add %arg0, %arg1 : i32\n    %1 = test.add %0, %arg2 : i32\n    test.return %1 : i32\n}");

    // assert_eq!(3, 5);
}

struct GenericOpVisitorCounter {
    // Use RefCell just to make it work with unmutable.
    // it's just for testing purposes.
    count: RefCell<usize>,
}

impl GenericOpVisitorCounter {
    fn new() -> Self {
        Self {
            count: RefCell::new(0),
        }
    }
}

impl<'a> IRVisitor<'a, GenericOperation<'a>> for GenericOpVisitorCounter {
    fn visit_op(&self, _op: GenericOperation<'a>) {
        self.count.borrow_mut().add_assign(1);
    }
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

#[test]
fn test_ir_visitor() {
    let mut ctx = IRContext::new();
    register_test_ops(&mut ctx);
    let op = OperationID::from_string_repr_with_context(
        "test.fun (%arg0: i32, %arg1: i32, %arg2: i32) {\n    %0 = test.add %arg0, %arg1 : i32\n    %1 = test.add %0, %arg2 : i32\n    test.return %1 : i32\n}".to_string(),
        &mut ctx,
    );
    let fun_op = ctx.get_generic_operation(op).cast::<TestFunOp>().unwrap();

    let generic_counter = GenericOpVisitorCounter::new();
    fun_op.walk(WalkOlder::PreOrder, &generic_counter);
    assert_eq!(*generic_counter.count.borrow(), 4);

    let add_counter = AddOpVisitorCounter::new();
    fun_op.walk(WalkOlder::PreOrder, &add_counter);
    assert_eq!(*add_counter.count.borrow(), 2);

    // assert_eq!(3, 5);
}

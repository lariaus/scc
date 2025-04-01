use diagnostics::diagnostics::{emit_error, DiagnosticsEmitter};
use iostreams::location::Location;
use parse::lexer::TokenValue;
use parse::parser::Parser;
use sir_core::{
    attributes::{Attribute, StringAttr, TypeAttr},
    block::Block,
    ir_builder::OpImplBuilderState,
    ir_context::IRContext,
    ir_data::{OperationData, OperationID, ValueID},
    ir_parser::{BlockParserState, IRParsableObject, IRParser, OperationParserState},
    ir_printer::{IRPrintableObject, IRPrinter},
    ir_verifier::ir_checks,
    op_interfaces::BuiltinOpInterfaceWrapper,
    op_tags::{TAG_DECLS_BLOCK_OP, TAG_TERMINATOR_OP},
    operation::{print_op_dispatch, OperationImpl},
    operation_type::{OperationTypeBuilder, OperationTypeUID},
    types::{FunctionType, Type},
    value::Value,
};

/////////////////////////////////////////////////////////////////////////
// TestModOp implementation
/////////////////////////////////////////////////////////////////////////

pub struct TestModOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl TestModOp<'_> {
    pub fn _build() -> OpImplBuilderState<Self> {
        let st = OpImplBuilderState::make();
        st
    }
}

const TEST_MOD_OPNAME: &'static str = "test.mod";
const TEST_MOD_TYPE_UID: OperationTypeUID = OperationTypeUID::make_from_opname(TEST_MOD_OPNAME);

impl<'a> OperationImpl<'a> for TestModOp<'a> {
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
        TEST_MOD_TYPE_UID
    }

    fn verify(&self, diagnostics: &mut DiagnosticsEmitter) {
        ir_checks::verif_inputs_count(diagnostics, self.generic(), 0);
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 0);
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 1);
    }

    fn custom_print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        let body = self.get_body();
        write!(printer.os(), "{{")?;

        // Print the body
        printer.print_block_content(body)?;
        write!(printer.os(), "}}")
    }

    fn custom_parse(
        parser: &mut IRParser,
        ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        let loc_beg = parser.get_next_token_loc();
        parser.consume_sym_or_error(TokenValue::sym_lcbracket())?;

        // Parse the body
        let mut block_st = BlockParserState::new();
        block_st.set_loc(loc_beg);
        let block = parser.make_block_from_state(block_st, ctx, TokenValue::sym_rcbracket())?;

        // Set the block
        st.set_blocks(vec![block]);
        Some(())
    }
}

impl<'a> TestModOp<'a> {
    fn get_body(&self) -> Block<'a> {
        self.get_block(0)
    }
}

// TODO: Find a way to get rid of this / generate this.
#[derive(Default)]
pub struct TestModOpBuiltinInterfaceWrapperImpl;
impl BuiltinOpInterfaceWrapper for TestModOpBuiltinInterfaceWrapperImpl {
    fn verify(&self, ctx: &IRContext, op: OperationID, diagnostics: &mut DiagnosticsEmitter) {
        let op = TestModOp::make_from_data(ctx, ctx.get_operation_data(op));
        op.verify(diagnostics)
    }

    fn custom_print(
        &self,
        printer: &mut sir_core::ir_printer::IRPrinter,
        ctx: &IRContext,
        op: OperationID,
    ) -> Result<(), std::io::Error> {
        let use_custom_printer = true;
        print_op_dispatch(
            TestModOp::make_from_data(ctx, ctx.get_operation_data(op)),
            printer,
            use_custom_printer,
        )
    }

    fn custom_parse(
        &self,
        parser: &mut sir_core::ir_parser::IRParser,
        ctx: &mut IRContext,
        st: &mut sir_core::ir_parser::OperationParserState,
    ) -> Option<()> {
        TestModOp::custom_parse(parser, ctx, st)
    }

    fn clone(&self) -> Box<dyn BuiltinOpInterfaceWrapper> {
        Box::new(TestModOpBuiltinInterfaceWrapperImpl {})
    }
}

fn register_test_mod_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(TEST_MOD_OPNAME);
    infos.set_impl::<TestModOp>();
    infos.set_builtin_interface::<TestModOpBuiltinInterfaceWrapperImpl>();
    infos.set_tags(&[TAG_DECLS_BLOCK_OP]);
    ctx.register_operation(infos.build());
}

/////////////////////////////////////////////////////////////////////////
// TestFunOp implementation
/////////////////////////////////////////////////////////////////////////

pub struct TestFunOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl TestFunOp<'_> {
    // Need to use `_build` somehow to remove the warning because the compiler can't detect it's used.
    pub fn _build<S: Into<StringAttr>>(
        symbol_name: S,
        function_type: Type,
    ) -> OpImplBuilderState<Self> {
        let symbol_name = Attribute::Str(symbol_name.into());
        assert!(function_type.isa::<FunctionType>());
        let function_type = TypeAttr::new(function_type);
        let mut st = OpImplBuilderState::make();
        st.set_attr(TEST_FUN_ATTR_SYMBOL_NAME, symbol_name);
        st.set_attr(TEST_FUN_ATTR_FUNCTION_TYPE, function_type);
        st
    }
}

const TEST_FUN_OPNAME: &'static str = "test.fun";
const TEST_FUN_TYPE_UID: OperationTypeUID = OperationTypeUID::make_from_opname(TEST_FUN_OPNAME);
const TEST_FUN_ATTR_SYMBOL_NAME: &'static str = "symbol_name";
const TEST_FUN_ATTR_FUNCTION_TYPE: &'static str = "function_type";

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

    fn verify(&self, diagnostics: &mut DiagnosticsEmitter) {
        ir_checks::verif_inputs_count(diagnostics, self.generic(), 0);
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 0);
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 1);
        ir_checks::verif_has_attr_of_type::<StringAttr>(
            diagnostics,
            self.generic(),
            &TEST_FUN_ATTR_SYMBOL_NAME,
        );
        ir_checks::verif_has_attr_of_type::<TypeAttr>(
            diagnostics,
            self.generic(),
            &TEST_FUN_ATTR_FUNCTION_TYPE,
        );

        // Check the block operands types
        let fun_type = self.get_function_type();
        let body = self.get_body();
        if fun_type.num_arguments() != body.get_num_operands() {
            emit_error(diagnostics, &self.generic(), format!("Signature type mismatch: signature has {} operands but body block has {} operands", fun_type.num_arguments(), body.get_num_operands()));
            return;
        }

        for (idx, (fun_ty, block_ty)) in fun_type
            .arguments()
            .iter()
            .zip(body.get_operands_types())
            .enumerate()
        {
            if fun_ty != block_ty {
                emit_error(diagnostics, &self.generic(), format!("Signature type mismatch: block operand #{} has type {} but function signature operand type is {}", idx, block_ty.to_string_repr(), fun_ty.to_string_repr()));
                return;
            }
        }
    }

    fn custom_print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        // Print the symbol name.
        write!(printer.os(), "@{}", self.get_symbol_name())?;

        let body = self.get_body();
        // Print the arguments
        write!(printer.os(), "(")?;
        printer.start_printing_block();
        for (idx, arg) in body.get_operands().enumerate() {
            printer.assign_and_print_value_label(arg.as_id(), true)?;
            write!(printer.os(), ": ")?;
            printer.print(arg.get_type())?;
            if idx + 1 < body.get_num_operands() {
                write!(printer.os(), ", ")?;
            }
        }
        write!(printer.os(), ")")?;

        // Print the optional results.
        let fun_type = self.get_function_type();
        if fun_type.num_results() > 0 {
            write!(printer.os(), " -> (")?;
            for (idx, ty) in fun_type.results().iter().enumerate() {
                printer.print(ty)?;
                if idx + 1 < fun_type.num_results() {
                    write!(printer.os(), ", ")?;
                }
            }
            write!(printer.os(), ")")?;
        }

        // Print the body
        write!(printer.os(), " {{")?;
        printer.print_block_content(body)?;
        printer.end_printing_block();
        write!(printer.os(), "}}")
    }

    fn custom_parse(
        parser: &mut IRParser,
        ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        // Parse the symbol name.
        parser.consume_sym_or_error(TokenValue::sym_at())?;
        let symbol_name = parser
            .consume_identifier_or_error()?
            .take_identifier()
            .unwrap();
        st.set_attr(TEST_FUN_ATTR_SYMBOL_NAME, StringAttr::new(symbol_name));

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
        block_st.set_operands_types(operands_types.clone());

        // Parse the optional result types.
        // '->' '(' <type> ',' <type> ',' ... ')'
        let mut results_types = Vec::new();
        if parser.try_consume_sym(TokenValue::sym_deref()) {
            parser.consume_sym_or_error(TokenValue::sym_lparen())?;
            if !parser.next_token_is_sym(TokenValue::sym_rparen()) {
                loop {
                    results_types.push(Type::parse(parser)?);
                    if !parser.try_consume_sym(TokenValue::sym_comma()) {
                        break;
                    }
                }
            }
            parser.consume_sym_or_error(TokenValue::sym_rparen())?;
        }

        // Build the function type.
        let fun_type = FunctionType::new(operands_types, results_types);
        st.set_attr(TEST_FUN_ATTR_FUNCTION_TYPE, TypeAttr::new(fun_type));

        // Parse the body
        let loc_end = parser
            .consume_sym_or_error(TokenValue::sym_lcbracket())?
            .loc();
        block_st.set_loc(Location::join(loc_beg, loc_end));
        let block = parser.make_block_from_state(block_st, ctx, TokenValue::sym_rcbracket())?;

        // Set the block
        st.set_blocks(vec![block]);
        Some(())
    }
}

impl<'a> TestFunOp<'a> {
    // Returns the body implementation of the function.
    pub fn get_body(&self) -> Block<'a> {
        self.get_block(0)
    }

    // Returns the symbol name
    pub fn get_symbol_name(&self) -> &'a str {
        self.get_attr(&TEST_FUN_ATTR_SYMBOL_NAME)
            .expect("Missing `symbol_name` attribute")
            .cast::<StringAttr>()
            .expect("symbol_name attr must be a StringAttr")
            .val()
    }

    // Returns the function type.
    pub fn get_function_type(&self) -> &'a FunctionType {
        self.get_attr(TEST_FUN_ATTR_FUNCTION_TYPE)
            .expect("Missing `function_type` attribute")
            .cast::<TypeAttr>()
            .expect("function_type attr must be a typeattr")
            .val()
            .cast()
            .expect("function_type attr value must be a function type")
    }
}

// TODO: Find a way to get rid of this / generate this.
#[derive(Default)]
pub struct TestFunOpBuiltinInterfaceWrapperImpl;
impl BuiltinOpInterfaceWrapper for TestFunOpBuiltinInterfaceWrapperImpl {
    fn verify(&self, ctx: &IRContext, op: OperationID, diagnostics: &mut DiagnosticsEmitter) {
        let op = TestFunOp::make_from_data(ctx, ctx.get_operation_data(op));
        op.verify(diagnostics)
    }

    fn custom_print(
        &self,
        printer: &mut sir_core::ir_printer::IRPrinter,
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
        parser: &mut sir_core::ir_parser::IRParser,
        ctx: &mut IRContext,
        st: &mut sir_core::ir_parser::OperationParserState,
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
    // Need to use `_build` somehow to remove the warning because the compiler can't detect it's used.
    pub fn _build<T0: Into<ValueID>, T1: Into<ValueID>>(
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

    fn verify(&self, diagnostics: &mut DiagnosticsEmitter) {
        ir_checks::verif_inputs_count(diagnostics, self.generic(), 2);
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 1);
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 0);
        ir_checks::verif_same_input_output_types(diagnostics, self.generic());
        if !self.lhs().get_type().is_int() {
            emit_error(
                diagnostics,
                &self.generic(),
                format!("TestAdd must have integer type inputs"),
            );
        }
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
    fn verify(&self, ctx: &IRContext, op: OperationID, diagnostics: &mut DiagnosticsEmitter) {
        let op = TestAddOp::make_from_data(ctx, ctx.get_operation_data(op));
        op.verify(diagnostics)
    }

    fn custom_print(
        &self,
        printer: &mut sir_core::ir_printer::IRPrinter,
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
        parser: &mut sir_core::ir_parser::IRParser,
        ctx: &mut IRContext,
        st: &mut sir_core::ir_parser::OperationParserState,
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
    // Need to use `_build` somehow to remove the warning because the compiler can't detect it's used.
    pub fn _build<T: Into<Vec<ValueID>>>(operands: T) -> OpImplBuilderState<Self> {
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

    fn verify(&self, diagnostics: &mut DiagnosticsEmitter) {
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 0);
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 0);

        // Get the function signature of the parent.
        let fun_op = match self.parent_op() {
            Some(x) => x,
            None => return,
        };
        let fun_type = match fun_op.cast::<TestFunOp>() {
            Some(fun_op) => fun_op.get_function_type(),
            None => {
                emit_error(
                    diagnostics,
                    &self.generic(),
                    format!("ReturnOp parent must be a TestFunOp"),
                );
                return;
            }
        };

        // Check the number and types of the arguments.
        if fun_type.num_results() != self.get_num_inputs() {
            emit_error(
                diagnostics,
                &self.generic(),
                format!(
                    "Function signature has {} results, but the function returns {} values",
                    fun_type.num_results(),
                    self.get_num_inputs()
                ),
            );
            return;
        }
        for (idx, (fun_ty, ret_ty)) in fun_type
            .results()
            .iter()
            .zip(self.get_inputs_types())
            .enumerate()
        {
            if fun_ty != ret_ty {
                emit_error(diagnostics, &self.generic(), format!("Function signature result type #{} is {}, but the return value #{} is of type {}", 
                    idx, fun_ty.to_string_repr(), idx, ret_ty.to_string_repr()));
            }
        }
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

        // Parse the operands names.
        let mut inputs_names = vec![];
        if parser.next_token_is_sym(TokenValue::sym_percent()) {
            loop {
                inputs_names.push(parser.parse_value_ref()?);
                if !parser.try_consume_sym(TokenValue::sym_comma()) {
                    break;
                }
            }
            parser.consume_sym_or_error(TokenValue::sym_colon())?;
        }

        // Parse operands types.
        let mut inputs_types = vec![];
        for idx in 0..inputs_names.len() {
            inputs_types.push(Type::parse(parser)?);
            if idx + 1 < inputs_names.len() {
                parser.consume_sym_or_error(TokenValue::sym_comma())?;
            }
        }

        st.set_inputs_names(inputs_names);
        st.set_inputs_types(inputs_types);
        Some(())
    }
}

// TODO: Find a way to get rid of this / generate this.
#[derive(Default)]
pub struct TestReturnOpBuiltinInterfaceWrapperImpl;
impl BuiltinOpInterfaceWrapper for TestReturnOpBuiltinInterfaceWrapperImpl {
    fn verify(&self, ctx: &IRContext, op: OperationID, diagnostics: &mut DiagnosticsEmitter) {
        let op = TestReturnOp::make_from_data(ctx, ctx.get_operation_data(op));
        op.verify(diagnostics)
    }

    fn custom_print(
        &self,
        printer: &mut sir_core::ir_printer::IRPrinter,
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
        parser: &mut sir_core::ir_parser::IRParser,
        ctx: &mut IRContext,
        st: &mut sir_core::ir_parser::OperationParserState,
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
    infos.set_tags(&[TAG_TERMINATOR_OP]);
    ctx.register_operation(infos.build());
}

/////////////////////////////////////////////////////////////////////////
// TestOps implementations
/////////////////////////////////////////////////////////////////////////

pub fn register_test_ops(ctx: &mut IRContext) {
    register_test_mod_op(ctx);
    register_test_fun_op(ctx);
    register_test_add_op(ctx);
    register_test_return_op(ctx);
}

use diagnostics::diagnostics::{emit_error, DiagnosticsEmitter, ErrorOrSuccess};
use iostreams::location::Location;
use parse::lexer::TokenValue;
use parse::parser::Parser;
use sir_core::{
    attributes::{Attribute, StringAttr, TypeAttr},
    block::Block,
    ir_builder::OpImplBuilderState,
    ir_context::IRContext,
    ir_data::{OperationData, ValueID},
    ir_parser::{BlockParserState, IRParsableObject, IRParser, OperationParserState},
    ir_printer::{IRPrintableObject, IRPrinter},
    ir_verifier::ir_checks,
    op_interfaces::{
        BuiltinOp, BuiltinOpInterfaceImpl, BuiltinOpInterfaceImplWrapper, OpInterfaceBuilder,
        OpInterfaceWrapper,
    },
    op_tags::{TAG_DECLS_BLOCK_OP, TAG_PURE_OP, TAG_TERMINATOR_OP},
    operation::{GenericOperation, OperationImpl},
    operation_type::{OperationTypeBuilder, OperationTypeUID},
    types::{FunctionType, IntegerType, Type},
    value::Value,
};

/////////////////////////////////////////////////////////////////////////
// TestModOp implementation
/////////////////////////////////////////////////////////////////////////

// @XGENDEF:SIROp TestModOp
// @opname "test.mod"
// @tags ["TAG_DECLS_BLOCK_OP"]
// @+block "body"
// @custom_print_parse
// @disable_default_builder

// @XGENBEGIN

// Code automatically generated by sir_core/scripts/xgen.py

const TEST_MOD_OPNAME: &'static str = "test.mod";
const TEST_MOD_TYPE_UID: OperationTypeUID = OperationTypeUID::make_from_opname(TEST_MOD_OPNAME);

pub struct TestModOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

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
}

impl<'a> TestModOp<'a> {
    pub fn get_body(&self) -> Block<'a> {
        self.get_block(0)
    }
}

// Wrapper struct for the BuiltinOp interface implementation.
#[derive(Default)]
pub struct TestModOpBuiltinOpInterfaceImpl;

impl BuiltinOpInterfaceImpl for TestModOpBuiltinOpInterfaceImpl {
    fn verify<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        diagnostics: &mut DiagnosticsEmitter,
    ) -> ErrorOrSuccess {
        GenericOperation::make_from_data(ctx, data)
            .cast::<TestModOp>()
            .unwrap()
            .verify(diagnostics)
    }
    fn custom_print<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        printer: &mut IRPrinter,
    ) -> Result<(), std::io::Error> {
        GenericOperation::make_from_data(ctx, data)
            .cast::<TestModOp>()
            .unwrap()
            .custom_print(printer)
    }
    fn custom_parse(
        &self,
        parser: &mut IRParser,
        ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        TestModOp::custom_parse(parser, ctx, st)
    }
    fn clone(&self) -> BuiltinOpInterfaceImplWrapper {
        TestModOp::clone()
    }
}

// Interface builder for BuiltinOp interface.
impl OpInterfaceBuilder for TestModOpBuiltinOpInterfaceImpl {
    type InterfaceObjectType = BuiltinOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = BuiltinOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

impl<'a> TestModOp<'a> {
    pub fn verify(&self, diagnostics: &mut DiagnosticsEmitter) -> ErrorOrSuccess {
        ir_checks::verif_inputs_count(diagnostics, self.generic(), 0)?;
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 0)?;
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 1)?;
        Ok(())
    }
    pub fn clone() -> BuiltinOpInterfaceImplWrapper {
        BuiltinOpInterfaceImplWrapper::new(Box::new(TestModOpBuiltinOpInterfaceImpl))
    }
}

fn register_test_mod_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(TEST_MOD_OPNAME);
    infos.set_impl::<TestModOp>();
    infos.set_builtin_interface::<TestModOpBuiltinOpInterfaceImpl>();
    infos.set_tags(&[TAG_DECLS_BLOCK_OP]);
    infos.add_interface::<TestModOpBuiltinOpInterfaceImpl>();
    ctx.register_operation(infos.build());
}

// @XGENEND

impl TestModOp<'_> {
    pub fn _build() -> OpImplBuilderState<Self> {
        let st = OpImplBuilderState::make();
        st
    }
}

// BuiltinOp interface implementation.
impl<'a> TestModOp<'a> {
    pub fn custom_print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        printer.print_op_results_and_opname(self.generic(), true)?;
        let body = self.get_body();
        write!(printer.os(), "{{")?;

        // Print the body
        printer.print_block_content(body)?;
        write!(printer.os(), "}}")
    }

    pub fn custom_parse(
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

/////////////////////////////////////////////////////////////////////////
// TestFunOp implementation
/////////////////////////////////////////////////////////////////////////

// @XGENDEF:SIROp TestFunOp
// @opname "test.fun"
// @+attr StringAttr<"symbol_name">
// @+attr FunctionTypeAttr<"function_type">
// @+block "body"
// @verifier
// @custom_print_parse
// @disable_default_builder

// @XGENBEGIN

// Code automatically generated by sir_core/scripts/xgen.py

const TEST_FUN_OPNAME: &'static str = "test.fun";
const TEST_FUN_TYPE_UID: OperationTypeUID = OperationTypeUID::make_from_opname(TEST_FUN_OPNAME);

pub struct TestFunOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

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
}

const TEST_FUN_ATTR_SYMBOL_NAME: &'static str = "symbol_name";
const TEST_FUN_ATTR_FUNCTION_TYPE: &'static str = "function_type";

impl<'a> TestFunOp<'a> {
    pub fn get_symbol_name(&self) -> &'a str {
        self.get_attr(TEST_FUN_ATTR_SYMBOL_NAME)
            .expect("Missing `symbol_name` attribute")
            .cast::<StringAttr>()
            .expect("`symbol_name` attribute must be a StringAttr")
            .val()
    }

    pub fn get_function_type(&self) -> &'a FunctionType {
        self.get_attr(TEST_FUN_ATTR_FUNCTION_TYPE)
            .expect("Missing `function_type` attribute")
            .cast::<TypeAttr>()
            .expect("`function_type` attribute must be a Type")
            .val()
            .cast::<FunctionType>()
            .expect("`function_type` type attribute must be a FunctionType")
    }

    pub fn get_body(&self) -> Block<'a> {
        self.get_block(0)
    }
}

// Wrapper struct for the BuiltinOp interface implementation.
#[derive(Default)]
pub struct TestFunOpBuiltinOpInterfaceImpl;

impl BuiltinOpInterfaceImpl for TestFunOpBuiltinOpInterfaceImpl {
    fn verify<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        diagnostics: &mut DiagnosticsEmitter,
    ) -> ErrorOrSuccess {
        GenericOperation::make_from_data(ctx, data)
            .cast::<TestFunOp>()
            .unwrap()
            .verify(diagnostics)
    }
    fn custom_print<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        printer: &mut IRPrinter,
    ) -> Result<(), std::io::Error> {
        GenericOperation::make_from_data(ctx, data)
            .cast::<TestFunOp>()
            .unwrap()
            .custom_print(printer)
    }
    fn custom_parse(
        &self,
        parser: &mut IRParser,
        ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        TestFunOp::custom_parse(parser, ctx, st)
    }
    fn clone(&self) -> BuiltinOpInterfaceImplWrapper {
        TestFunOp::clone()
    }
}

// Interface builder for BuiltinOp interface.
impl OpInterfaceBuilder for TestFunOpBuiltinOpInterfaceImpl {
    type InterfaceObjectType = BuiltinOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = BuiltinOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

impl<'a> TestFunOp<'a> {
    pub fn verify(&self, diagnostics: &mut DiagnosticsEmitter) -> ErrorOrSuccess {
        ir_checks::verif_inputs_count(diagnostics, self.generic(), 0)?;
        ir_checks::verif_has_attr_of_type::<StringAttr>(
            diagnostics,
            self.generic(),
            TEST_FUN_ATTR_SYMBOL_NAME,
        )?;
        ir_checks::verif_has_type_attr_of_type::<FunctionType>(
            diagnostics,
            self.generic(),
            TEST_FUN_ATTR_FUNCTION_TYPE,
        )?;
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 0)?;
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 1)?;
        self.verify_op(diagnostics)?;
        Ok(())
    }
    pub fn clone() -> BuiltinOpInterfaceImplWrapper {
        BuiltinOpInterfaceImplWrapper::new(Box::new(TestFunOpBuiltinOpInterfaceImpl))
    }
}

fn register_test_fun_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(TEST_FUN_OPNAME);
    infos.set_impl::<TestFunOp>();
    infos.set_builtin_interface::<TestFunOpBuiltinOpInterfaceImpl>();
    infos.add_interface::<TestFunOpBuiltinOpInterfaceImpl>();
    ctx.register_operation(infos.build());
}

// @XGENEND

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

// BuiltinOp interface implementation.
impl<'a> TestFunOp<'a> {
    pub fn verify_op(&self, diagnostics: &mut DiagnosticsEmitter) -> ErrorOrSuccess {
        // Check the block operands types
        let fun_type = self.get_function_type();
        let body = self.get_body();
        if fun_type.num_arguments() != body.get_num_operands() {
            emit_error(diagnostics, &self.generic(), format!("Signature type mismatch: signature has {} operands but body block has {} operands", fun_type.num_arguments(), body.get_num_operands()));
            return Err(());
        }

        for (idx, (fun_ty, block_ty)) in fun_type
            .arguments()
            .iter()
            .zip(body.get_operands_types())
            .enumerate()
        {
            if fun_ty != block_ty {
                emit_error(diagnostics, &self.generic(), format!("Signature type mismatch: block operand #{} has type {} but function signature operand type is {}", idx, block_ty.to_string_repr(), fun_ty.to_string_repr()));
                return Err(());
            }
        }

        Ok(())
    }

    pub fn custom_print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        printer.print_op_results_and_opname(self.generic(), true)?;

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

    pub fn custom_parse(
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

/////////////////////////////////////////////////////////////////////////
// TestAddOp implementation
/////////////////////////////////////////////////////////////////////////

// @XGENDEF:SIROp TestAddOp
// @opname "test.add"
// @+input IntegerValue<"lhs">
// @+input IntegerValue<"rhs">
// @+output IntegerValue<"result">
// @tags ["TAG_PURE_OP"]
// @+mod SameInputsAndOutputsTypes
// @custom_print_parse
// @disable_default_builder

// @XGENBEGIN

// Code automatically generated by sir_core/scripts/xgen.py

const TEST_ADD_OPNAME: &'static str = "test.add";
const TEST_ADD_TYPE_UID: OperationTypeUID = OperationTypeUID::make_from_opname(TEST_ADD_OPNAME);

pub struct TestAddOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl<'a> OperationImpl<'a> for TestAddOp<'a> {
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
        TEST_ADD_TYPE_UID
    }
}

impl<'a> TestAddOp<'a> {
    pub fn get_lhs(&self) -> Value<'a> {
        self.get_input(0)
    }

    pub fn get_rhs(&self) -> Value<'a> {
        self.get_input(1)
    }

    pub fn get_result(&self) -> Value<'a> {
        self.get_output(0)
    }
}

// Wrapper struct for the BuiltinOp interface implementation.
#[derive(Default)]
pub struct TestAddOpBuiltinOpInterfaceImpl;

impl BuiltinOpInterfaceImpl for TestAddOpBuiltinOpInterfaceImpl {
    fn verify<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        diagnostics: &mut DiagnosticsEmitter,
    ) -> ErrorOrSuccess {
        GenericOperation::make_from_data(ctx, data)
            .cast::<TestAddOp>()
            .unwrap()
            .verify(diagnostics)
    }
    fn custom_print<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        printer: &mut IRPrinter,
    ) -> Result<(), std::io::Error> {
        GenericOperation::make_from_data(ctx, data)
            .cast::<TestAddOp>()
            .unwrap()
            .custom_print(printer)
    }
    fn custom_parse(
        &self,
        parser: &mut IRParser,
        ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        TestAddOp::custom_parse(parser, ctx, st)
    }
    fn clone(&self) -> BuiltinOpInterfaceImplWrapper {
        TestAddOp::clone()
    }
}

// Interface builder for BuiltinOp interface.
impl OpInterfaceBuilder for TestAddOpBuiltinOpInterfaceImpl {
    type InterfaceObjectType = BuiltinOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = BuiltinOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

impl<'a> TestAddOp<'a> {
    pub fn verify(&self, diagnostics: &mut DiagnosticsEmitter) -> ErrorOrSuccess {
        ir_checks::verif_inputs_count(diagnostics, self.generic(), 2)?;
        ir_checks::verif_io_is_of_type::<IntegerType>(diagnostics, self.generic(), true, 0, "lhs")?;
        ir_checks::verif_io_is_of_type::<IntegerType>(diagnostics, self.generic(), true, 1, "rhs")?;
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 1)?;
        ir_checks::verif_io_is_of_type::<IntegerType>(
            diagnostics,
            self.generic(),
            false,
            0,
            "result",
        )?;
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 0)?;
        ir_checks::verif_same_input_output_types(diagnostics, self.generic())?;
        Ok(())
    }
    pub fn clone() -> BuiltinOpInterfaceImplWrapper {
        BuiltinOpInterfaceImplWrapper::new(Box::new(TestAddOpBuiltinOpInterfaceImpl))
    }
}

fn register_test_add_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(TEST_ADD_OPNAME);
    infos.set_impl::<TestAddOp>();
    infos.set_builtin_interface::<TestAddOpBuiltinOpInterfaceImpl>();
    infos.set_tags(&[TAG_PURE_OP]);
    infos.add_interface::<TestAddOpBuiltinOpInterfaceImpl>();
    ctx.register_operation(infos.build());
}

// @XGENEND

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

// BuiltinOp interface implementation.
impl<'a> TestAddOp<'a> {
    pub fn custom_print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        printer.print_op_results_and_opname(self.generic(), true)?;

        printer.print_value_label_or_unknown(self.get_lhs().into())?;
        write!(printer.os(), ", ")?;
        printer.print_value_label_or_unknown(self.get_rhs().into())?;
        write!(printer.os(), " : ")?;
        printer.print(self.get_result().get_type())
    }

    pub fn custom_parse(
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

/////////////////////////////////////////////////////////////////////////
// TestReturnOp implementation
/////////////////////////////////////////////////////////////////////////

// @XGENDEF:SIROp TestReturnOp
// @opname "test.return"
// @+input VariadicValue<"returns">
// @tags ["TAG_TERMINATOR_OP"]
// @verifier
// @custom_print_parse
// @disable_default_builder

// @XGENBEGIN

// Code automatically generated by sir_core/scripts/xgen.py

const TEST_RETURN_OPNAME: &'static str = "test.return";
const TEST_RETURN_TYPE_UID: OperationTypeUID =
    OperationTypeUID::make_from_opname(TEST_RETURN_OPNAME);

pub struct TestReturnOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl<'a> OperationImpl<'a> for TestReturnOp<'a> {
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
        TEST_RETURN_TYPE_UID
    }
}

impl<'a> TestReturnOp<'a> {}

// Wrapper struct for the BuiltinOp interface implementation.
#[derive(Default)]
pub struct TestReturnOpBuiltinOpInterfaceImpl;

impl BuiltinOpInterfaceImpl for TestReturnOpBuiltinOpInterfaceImpl {
    fn verify<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        diagnostics: &mut DiagnosticsEmitter,
    ) -> ErrorOrSuccess {
        GenericOperation::make_from_data(ctx, data)
            .cast::<TestReturnOp>()
            .unwrap()
            .verify(diagnostics)
    }
    fn custom_print<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        printer: &mut IRPrinter,
    ) -> Result<(), std::io::Error> {
        GenericOperation::make_from_data(ctx, data)
            .cast::<TestReturnOp>()
            .unwrap()
            .custom_print(printer)
    }
    fn custom_parse(
        &self,
        parser: &mut IRParser,
        ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        TestReturnOp::custom_parse(parser, ctx, st)
    }
    fn clone(&self) -> BuiltinOpInterfaceImplWrapper {
        TestReturnOp::clone()
    }
}

// Interface builder for BuiltinOp interface.
impl OpInterfaceBuilder for TestReturnOpBuiltinOpInterfaceImpl {
    type InterfaceObjectType = BuiltinOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = BuiltinOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

impl<'a> TestReturnOp<'a> {
    pub fn verify(&self, diagnostics: &mut DiagnosticsEmitter) -> ErrorOrSuccess {
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 0)?;
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 0)?;
        self.verify_op(diagnostics)?;
        Ok(())
    }
    pub fn clone() -> BuiltinOpInterfaceImplWrapper {
        BuiltinOpInterfaceImplWrapper::new(Box::new(TestReturnOpBuiltinOpInterfaceImpl))
    }
}

fn register_test_return_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(TEST_RETURN_OPNAME);
    infos.set_impl::<TestReturnOp>();
    infos.set_builtin_interface::<TestReturnOpBuiltinOpInterfaceImpl>();
    infos.set_tags(&[TAG_TERMINATOR_OP]);
    infos.add_interface::<TestReturnOpBuiltinOpInterfaceImpl>();
    ctx.register_operation(infos.build());
}

// @XGENEND

impl TestReturnOp<'_> {
    // Need to use `_build` somehow to remove the warning because the compiler can't detect it's used.
    pub fn _build<T: Into<Vec<ValueID>>>(operands: T) -> OpImplBuilderState<Self> {
        let mut st = OpImplBuilderState::make();
        st.set_inputs(operands);
        st
    }
}

// BuiltinOp interface implementation.
impl<'a> TestReturnOp<'a> {
    pub fn verify_op(&self, diagnostics: &mut DiagnosticsEmitter) -> ErrorOrSuccess {
        // Get the function signature of the parent.
        let fun_op = match self.parent_op() {
            Some(x) => x,
            None => return Ok(()),
        };
        let fun_type = match fun_op.cast::<TestFunOp>() {
            Some(fun_op) => fun_op.get_function_type(),
            None => {
                emit_error(
                    diagnostics,
                    &self.generic(),
                    format!("ReturnOp parent must be a TestFunOp"),
                );
                return Err(());
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
            return Err(());
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
                return Err(());
            }
        }

        Ok(())
    }

    pub fn custom_print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        printer.print_op_results_and_opname(self.generic(), true)?;

        // Print the return names.
        for (idx, val) in self.get_inputs().enumerate() {
            printer.print_value_label_or_unknown(val.into())?;
            if idx + 1 < self.get_num_inputs() {
                write!(printer.os(), ", ")?;
            }
        }
        write!(printer.os(), " : ")?;

        // Print the return types.
        for (idx, ty) in self.get_inputs_types().enumerate() {
            printer.print(ty)?;
            if idx + 1 < self.get_num_inputs() {
                write!(printer.os(), ", ")?;
            }
        }

        Ok(())
    }

    pub fn custom_parse(
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

/////////////////////////////////////////////////////////////////////////
// TestOps Registrations
/////////////////////////////////////////////////////////////////////////

// @XGENBEGIN RegisterOps register_test_ops
pub fn register_test_ops(ctx: &mut IRContext) {
    register_test_mod_op(ctx);
    register_test_fun_op(ctx);
    register_test_add_op(ctx);
    register_test_return_op(ctx);
}

// @XGENEND

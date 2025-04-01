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
};

/////////////////////////////////////////////////////////////////////////
// ModuleOp implementation
/////////////////////////////////////////////////////////////////////////

pub struct ModuleOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl ModuleOp<'_> {
    pub fn build() -> OpImplBuilderState<Self> {
        let st = OpImplBuilderState::make();
        st
    }
}

const MODULE_OP_OPNAME: &'static str = "module";
const MODULE_OP_TYPE_UID: OperationTypeUID = OperationTypeUID::make_from_opname(MODULE_OP_OPNAME);

impl<'a> OperationImpl<'a> for ModuleOp<'a> {
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
        MODULE_OP_TYPE_UID
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

impl<'a> ModuleOp<'a> {
    fn get_body(&self) -> Block<'a> {
        self.get_block(0)
    }
}

// TODO: Find a way to get rid of this / generate this.
#[derive(Default)]
pub struct ModuleOpBuiltinInterfaceWrapperImpl;
impl BuiltinOpInterfaceWrapper for ModuleOpBuiltinInterfaceWrapperImpl {
    fn verify(&self, ctx: &IRContext, op: OperationID, diagnostics: &mut DiagnosticsEmitter) {
        let op = ModuleOp::make_from_data(ctx, ctx.get_operation_data(op));
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
            ModuleOp::make_from_data(ctx, ctx.get_operation_data(op)),
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
        ModuleOp::custom_parse(parser, ctx, st)
    }

    fn clone(&self) -> Box<dyn BuiltinOpInterfaceWrapper> {
        Box::new(ModuleOpBuiltinInterfaceWrapperImpl {})
    }
}

fn register_module_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(MODULE_OP_OPNAME);
    infos.set_impl::<ModuleOp>();
    infos.set_builtin_interface::<ModuleOpBuiltinInterfaceWrapperImpl>();
    infos.set_tags(&[TAG_DECLS_BLOCK_OP]);
    ctx.register_operation(infos.build());
}

/////////////////////////////////////////////////////////////////////////
// FunctionOp implementation
/////////////////////////////////////////////////////////////////////////

pub struct FunctionOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl FunctionOp<'_> {
    pub fn build<S: Into<StringAttr>>(
        symbol_name: S,
        function_type: Type,
    ) -> OpImplBuilderState<Self> {
        let symbol_name = Attribute::Str(symbol_name.into());
        assert!(function_type.isa::<FunctionType>());
        let function_type = TypeAttr::new(function_type);
        let mut st = OpImplBuilderState::make();
        st.set_attr(FUNCTION_OP_ATTR_SYMBOL_NAME, symbol_name);
        st.set_attr(FUNCTION_OP_ATTR_FUNCTION_TYPE, function_type);
        st
    }
}

const FUNCTION_OP_OPNAME: &'static str = "function";
const FUNCTION_OP_TYPE_UID: OperationTypeUID =
    OperationTypeUID::make_from_opname(FUNCTION_OP_OPNAME);
const FUNCTION_OP_ATTR_SYMBOL_NAME: &'static str = "symbol_name";
const FUNCTION_OP_ATTR_FUNCTION_TYPE: &'static str = "function_type";

impl<'a> OperationImpl<'a> for FunctionOp<'a> {
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
        FUNCTION_OP_TYPE_UID
    }

    fn verify(&self, diagnostics: &mut DiagnosticsEmitter) {
        ir_checks::verif_inputs_count(diagnostics, self.generic(), 0);
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 0);
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 1);
        ir_checks::verif_has_attr_of_type::<StringAttr>(
            diagnostics,
            self.generic(),
            FUNCTION_OP_ATTR_SYMBOL_NAME,
        );
        ir_checks::verif_has_attr_of_type::<TypeAttr>(
            diagnostics,
            self.generic(),
            FUNCTION_OP_ATTR_FUNCTION_TYPE,
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
        st.set_attr(FUNCTION_OP_ATTR_SYMBOL_NAME, StringAttr::new(symbol_name));

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
        st.set_attr(FUNCTION_OP_ATTR_FUNCTION_TYPE, TypeAttr::new(fun_type));

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

impl<'a> FunctionOp<'a> {
    // Returns the body implementation of the function.
    pub fn get_body(&self) -> Block<'a> {
        self.get_block(0)
    }

    // Returns the symbol name
    pub fn get_symbol_name(&self) -> &'a str {
        self.get_attr(FUNCTION_OP_ATTR_SYMBOL_NAME)
            .expect("Missing `symbol_name` attribute")
            .cast::<StringAttr>()
            .expect("symbol_name attr must be a StringAttr")
            .val()
    }

    // Returns the function type.
    pub fn get_function_type(&self) -> &'a FunctionType {
        self.get_attr(FUNCTION_OP_ATTR_FUNCTION_TYPE)
            .expect("Missing `function_type` attribute")
            .cast::<TypeAttr>()
            .expect("function_type attr must be a TypeAttr")
            .val()
            .cast()
            .expect("function_type attr value must be a function type")
    }
}

// TODO: Find a way to get rid of this / generate this.
#[derive(Default)]
pub struct FunctionOpBuiltinInterfaceWrapperImpl;
impl BuiltinOpInterfaceWrapper for FunctionOpBuiltinInterfaceWrapperImpl {
    fn verify(&self, ctx: &IRContext, op: OperationID, diagnostics: &mut DiagnosticsEmitter) {
        let op = FunctionOp::make_from_data(ctx, ctx.get_operation_data(op));
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
            FunctionOp::make_from_data(ctx, ctx.get_operation_data(op)),
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
        FunctionOp::custom_parse(parser, ctx, st)
    }

    fn clone(&self) -> Box<dyn BuiltinOpInterfaceWrapper> {
        Box::new(FunctionOpBuiltinInterfaceWrapperImpl {})
    }
}

fn register_function_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(FUNCTION_OP_OPNAME);
    infos.set_impl::<FunctionOp>();
    infos.set_builtin_interface::<FunctionOpBuiltinInterfaceWrapperImpl>();
    ctx.register_operation(infos.build());
}

/////////////////////////////////////////////////////////////////////////
// ReturnOp implementation
/////////////////////////////////////////////////////////////////////////

pub struct ReturnOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl ReturnOp<'_> {
    pub fn build<T: Into<Vec<ValueID>>>(operands: T) -> OpImplBuilderState<Self> {
        let mut st = OpImplBuilderState::make();
        st.set_inputs(operands);
        st
    }
}

const RETURN_OP_OPNAME: &'static str = "return";
const RETURN_OP_TYPE_UID: OperationTypeUID = OperationTypeUID::make_from_opname(RETURN_OP_OPNAME);

impl<'a> OperationImpl<'a> for ReturnOp<'a> {
    fn make_from_data(ctx: &'a IRContext, data: &'a OperationData) -> Self {
        Self { ctx, data }
    }

    fn get_op_type_uid() -> OperationTypeUID {
        RETURN_OP_TYPE_UID
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
        let fun_type = match fun_op.cast::<FunctionOp>() {
            Some(fun_op) => fun_op.get_function_type(),
            None => {
                emit_error(
                    diagnostics,
                    &self.generic(),
                    format!("ReturnOp parent must be a FunctionOp"),
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
pub struct ReturnOpBuiltinInterfaceWrapperImpl;
impl BuiltinOpInterfaceWrapper for ReturnOpBuiltinInterfaceWrapperImpl {
    fn verify(&self, ctx: &IRContext, op: OperationID, diagnostics: &mut DiagnosticsEmitter) {
        let op = ReturnOp::make_from_data(ctx, ctx.get_operation_data(op));
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
            ReturnOp::make_from_data(ctx, ctx.get_operation_data(op)),
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
        ReturnOp::custom_parse(parser, ctx, st)
    }

    fn clone(&self) -> Box<dyn BuiltinOpInterfaceWrapper> {
        Box::new(ReturnOpBuiltinInterfaceWrapperImpl {})
    }
}

fn register_return_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(RETURN_OP_OPNAME);
    infos.set_impl::<ReturnOp>();
    infos.set_builtin_interface::<ReturnOpBuiltinInterfaceWrapperImpl>();
    infos.set_tags(&[TAG_TERMINATOR_OP]);
    ctx.register_operation(infos.build());
}

/////////////////////////////////////////////////////////////////////////
// TestOps implementations
/////////////////////////////////////////////////////////////////////////

pub fn register_func_ops(ctx: &mut IRContext) {
    register_module_op(ctx);
    register_function_op(ctx);
    register_return_op(ctx);
}

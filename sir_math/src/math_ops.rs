use diagnostics::diagnostics::DiagnosticsEmitter;
use parse::lexer::TokenValue;
use parse::parser::Parser;
use sir_core::{
    attributes::Attribute,
    ir_builder::OpImplBuilderState,
    ir_context::IRContext,
    ir_data::{OperationData, ValueID},
    ir_parser::{IRParsableObject, IRParser, OperationParserState},
    ir_printer::IRPrinter,
    ir_verifier::ir_checks,
    op_interfaces::{
        BuiltinOp, BuiltinOpInterfaceImpl, BuiltinOpInterfaceImplWrapper, ConstantOp, ConstantOpInterfaceImpl, ConstantOpInterfaceImplWrapper, OpInterfaceBuilder, OpInterfaceWrapper
    },
    op_tags::TAG_PURE_OP,
    operation::{GenericOperation, OperationImpl},
    operation_type::{OperationTypeBuilder, OperationTypeUID},
    types::{IntegerType, Type},
    value::Value,
};

/////////////////////////////////////////////////////////////////////////
// MathConstant implementation
/////////////////////////////////////////////////////////////////////////

// @XGENDEF:SIROp MathConstantOp
// @opname "math.constant"
// @+attr ScalarAttr<"value">
// @+output ValueWithPred<"result", PredMatchTypeOfAttr<"value">, "same type than value">
// @tags "TAG_PURE_OP"
// @interfaces ConstantOp
// @custom_print_parse

// @XGENBEGIN

// Code automatically generated by sir_core/scripts/xgen.py

const MATH_CONSTANT_OPNAME: &'static str = "math.constant";
const MATH_CONSTANT_TYPE_UID: OperationTypeUID = OperationTypeUID::make_from_opname(MATH_CONSTANT_OPNAME);

pub struct MathConstantOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl<'a> OperationImpl<'a> for MathConstantOp<'a> {
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
        MATH_CONSTANT_TYPE_UID
    }
}

const MATH_CONSTANT_ATTR_VALUE: &'static str = "value";

impl<'a> MathConstantOp<'a> {

    pub fn get_result(&self) -> Value<'a> {
        self.get_output(0)
    }

    pub fn get_value(&self) -> &'a Attribute {
        self.get_attr(MATH_CONSTANT_ATTR_VALUE).expect("Missing `value` attribute")
    }

}

// Wrapper struct for the BuiltinOp interface implementation.
#[derive(Default)]
pub struct MathConstantOpBuiltinOpInterfaceImpl;

impl BuiltinOpInterfaceImpl for MathConstantOpBuiltinOpInterfaceImpl {
    fn verify<'a>(&self, ctx: &'a IRContext, data: &'a OperationData, diagnostics: &mut DiagnosticsEmitter) {
        GenericOperation::make_from_data(ctx, data).cast::<MathConstantOp>().unwrap().verify(diagnostics)
    }
    fn custom_print<'a>(&self, ctx: &'a IRContext, data: &'a OperationData, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        GenericOperation::make_from_data(ctx, data).cast::<MathConstantOp>().unwrap().custom_print(printer)
    }
    fn custom_parse(&self, parser: &mut IRParser, ctx: &mut IRContext, st: &mut OperationParserState) -> Option<()> {
        MathConstantOp::custom_parse(parser, ctx, st)
    }
    fn clone(&self) -> BuiltinOpInterfaceImplWrapper {
        MathConstantOp::clone()
    }
}


// Interface builder for BuiltinOp interface.
impl OpInterfaceBuilder for MathConstantOpBuiltinOpInterfaceImpl {
    type InterfaceObjectType = BuiltinOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = BuiltinOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

// Wrapper struct for the ConstantOp interface implementation.
#[derive(Default)]
pub struct MathConstantOpConstantOpInterfaceImpl;

impl ConstantOpInterfaceImpl for MathConstantOpConstantOpInterfaceImpl {
    fn get_value<'a>(&self, ctx: &'a IRContext, data: &'a OperationData) -> &'a Attribute {
        GenericOperation::make_from_data(ctx, data).cast::<MathConstantOp>().unwrap().get_value()
    }
}


// Interface builder for ConstantOp interface.
impl OpInterfaceBuilder for MathConstantOpConstantOpInterfaceImpl {
    type InterfaceObjectType = ConstantOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = ConstantOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

impl<'a> MathConstantOp<'a> {
    pub fn verify(&self, diagnostics: &mut DiagnosticsEmitter) {
        ir_checks::verif_inputs_count(diagnostics, self.generic(), 0);
        ir_checks::verif_has_attr_as(diagnostics, self.generic(), MATH_CONSTANT_ATTR_VALUE, |attr| ir_checks::pred_is_scalar_attribute(attr), "scalar attribute");
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 1);
        ir_checks::verif_io_type(diagnostics, self.generic(), false, 0, "result", |ty| ir_checks::pred_match_type_of_attr(&self.generic(), ty, "value"), "same type than value");
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 0);
    }
}

fn register_math_constant_op(ctx: &mut IRContext) {    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(MATH_CONSTANT_OPNAME);
    infos.set_impl::<MathConstantOp>();
    infos.set_builtin_interface::<MathConstantOpBuiltinOpInterfaceImpl>();
    infos.set_tags(&[TAG_PURE_OP]);
    infos.add_interface::<MathConstantOpBuiltinOpInterfaceImpl>();
    infos.add_interface::<MathConstantOpConstantOpInterfaceImpl>();
    ctx.register_operation(infos.build());
}



// @XGENEND

impl MathConstantOp<'_> {
    pub fn build(val: Attribute) -> OpImplBuilderState<Self> {
        let mut st = OpImplBuilderState::make();
        st.set_inputs(vec![]);
        st.set_outputs_types(vec![val
            .get_type()
            .expect("val must be a typed attr")
            .clone()]);
        st.set_attr(MATH_CONSTANT_ATTR_VALUE, val);
        st
    }
}

// BuiltinOp interface implementation.
impl<'a> MathConstantOp<'a> {
    pub fn custom_print(&self, printer: &mut IRPrinter,
    ) -> Result<(), std::io::Error> {
        printer.print_op_results_and_opname(self.generic(), true)?;

        // Print the attribute
        printer.print(self.get_value())
    }

    pub fn custom_parse(
        parser: &mut IRParser,
        _ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        // Parse the val attribute.
        let val: Attribute = parser.parse()?;
        let ty = val.get_type().expect("`val` must be a typed attr").clone();
        st.set_attr(MATH_CONSTANT_ATTR_VALUE, val);
        st.set_outputs_types(vec![ty]);
        Some(())
    }

    pub fn clone() -> BuiltinOpInterfaceImplWrapper {
        BuiltinOpInterfaceImplWrapper::new(Box::new(MathConstantOpBuiltinOpInterfaceImpl))
    }
}

/////////////////////////////////////////////////////////////////////////
// MathIAdd implementation
/////////////////////////////////////////////////////////////////////////

// @XGENDEF:SIROp MathIAddOp
// @opname "math.iadd"
// @+input IntegerValue<"lhs">
// @+input IntegerValue<"rhs">
// @+output IntegerValue<"result">
// @tags "TAG_PURE_OP"
// @+mod SameInputsAndOutputsTypes
// @custom_print_parse

// @XGENBEGIN

// Code automatically generated by sir_core/scripts/xgen.py

const MATH_I_ADD_OPNAME: &'static str = "math.iadd";
const MATH_I_ADD_TYPE_UID: OperationTypeUID = OperationTypeUID::make_from_opname(MATH_I_ADD_OPNAME);

pub struct MathIAddOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl<'a> OperationImpl<'a> for MathIAddOp<'a> {
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
        MATH_I_ADD_TYPE_UID
    }
}

impl<'a> MathIAddOp<'a> {

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
pub struct MathIAddOpBuiltinOpInterfaceImpl;

impl BuiltinOpInterfaceImpl for MathIAddOpBuiltinOpInterfaceImpl {
    fn verify<'a>(&self, ctx: &'a IRContext, data: &'a OperationData, diagnostics: &mut DiagnosticsEmitter) {
        GenericOperation::make_from_data(ctx, data).cast::<MathIAddOp>().unwrap().verify(diagnostics)
    }
    fn custom_print<'a>(&self, ctx: &'a IRContext, data: &'a OperationData, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        GenericOperation::make_from_data(ctx, data).cast::<MathIAddOp>().unwrap().custom_print(printer)
    }
    fn custom_parse(&self, parser: &mut IRParser, ctx: &mut IRContext, st: &mut OperationParserState) -> Option<()> {
        MathIAddOp::custom_parse(parser, ctx, st)
    }
    fn clone(&self) -> BuiltinOpInterfaceImplWrapper {
        MathIAddOp::clone()
    }
}


// Interface builder for BuiltinOp interface.
impl OpInterfaceBuilder for MathIAddOpBuiltinOpInterfaceImpl {
    type InterfaceObjectType = BuiltinOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = BuiltinOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

impl<'a> MathIAddOp<'a> {
    pub fn verify(&self, diagnostics: &mut DiagnosticsEmitter) {
        ir_checks::verif_inputs_count(diagnostics, self.generic(), 2);
        ir_checks::verif_io_is_of_type::<IntegerType>(diagnostics, self.generic(), true, 0, "lhs");
        ir_checks::verif_io_is_of_type::<IntegerType>(diagnostics, self.generic(), true, 1, "rhs");
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 1);
        ir_checks::verif_io_is_of_type::<IntegerType>(diagnostics, self.generic(), false, 0, "result");
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 0);
        ir_checks::verif_same_input_output_types(diagnostics, self.generic());
    }
}

fn register_math_i_add_op(ctx: &mut IRContext) {    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(MATH_I_ADD_OPNAME);
    infos.set_impl::<MathIAddOp>();
    infos.set_builtin_interface::<MathIAddOpBuiltinOpInterfaceImpl>();
    infos.set_tags(&[TAG_PURE_OP]);
    infos.add_interface::<MathIAddOpBuiltinOpInterfaceImpl>();
    ctx.register_operation(infos.build());
}



// @XGENEND

impl MathIAddOp<'_> {
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

// BuiltinOp interface implementation.
impl<'a> MathIAddOp<'a> {
    pub fn custom_print(&self, printer: &mut IRPrinter,
    ) -> Result<(), std::io::Error> {
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

    pub fn clone() -> BuiltinOpInterfaceImplWrapper {
        BuiltinOpInterfaceImplWrapper::new(Box::new(MathIAddOpBuiltinOpInterfaceImpl))
    }
}


/////////////////////////////////////////////////////////////////////////
// MathOps Registrations
/////////////////////////////////////////////////////////////////////////

// @XGENBEGIN RegisterOps register_math_ops
pub fn register_math_ops(ctx: &mut IRContext) {
    register_math_constant_op(ctx);
    register_math_i_add_op(ctx);
}

// @XGENEND
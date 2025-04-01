use diagnostics::diagnostics::{emit_error, DiagnosticsEmitter};
use parse::lexer::TokenValue;
use parse::parser::Parser;
use sir_core::{
    attributes::Attribute,
    ir_builder::OpImplBuilderState,
    ir_context::IRContext,
    ir_data::{OperationData, OperationID, ValueID},
    ir_parser::{IRParsableObject, IRParser, OperationParserState},
    ir_printer::IRPrinter,
    ir_verifier::ir_checks,
    op_interfaces::{
        BuiltinOpInterfaceWrapper, ConstantOp, ConstantOpInterfaceImpl,
        ConstantOpInterfaceImplWrapper, OpInterfaceBuilder, OpInterfaceWrapper,
    },
    op_tags::TAG_PURE_OP,
    operation::{print_op_dispatch, GenericOperation, OperationImpl},
    operation_type::{OperationTypeBuilder, OperationTypeUID},
    types::Type,
    value::Value,
};

/////////////////////////////////////////////////////////////////////////
// MathConstant implementation
/////////////////////////////////////////////////////////////////////////

pub struct MathConstantOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl MathConstantOp<'_> {
    pub fn build<T0: Into<ValueID>, T1: Into<ValueID>>(val: Attribute) -> OpImplBuilderState<Self> {
        let mut st = OpImplBuilderState::make();
        st.set_inputs(vec![]);
        st.set_outputs_types(vec![val
            .get_type()
            .expect("val must be a typed attr")
            .clone()]);
        st.set_attr(MATH_CONSTANT_ATTR_VAL, val);
        st
    }
}

impl<'a> MathConstantOp<'a> {
    pub fn val(&self) -> &'a Attribute {
        self.get_attr(MATH_CONSTANT_ATTR_VAL)
            .expect("math.constant must have a `val` attribute")
    }
}

const MATH_CONSTANT_OPNAME: &'static str = "math.constant";
const MATH_CONSTANT_TYPE_UID: OperationTypeUID =
    OperationTypeUID::make_from_opname(MATH_CONSTANT_OPNAME);
const MATH_CONSTANT_ATTR_VAL: &'static str = "val";

impl<'a> OperationImpl<'a> for MathConstantOp<'a> {
    fn make_from_data(ctx: &'a IRContext, data: &'a OperationData) -> Self {
        Self { ctx, data }
    }

    fn get_op_type_uid() -> OperationTypeUID {
        MATH_CONSTANT_TYPE_UID
    }

    fn get_op_data(&self) -> &'a OperationData {
        self.data
    }

    fn get_context(&self) -> &'a IRContext {
        self.ctx
    }

    fn verify(&self, diagnostics: &mut DiagnosticsEmitter) {
        ir_checks::verif_inputs_count(diagnostics, self.generic(), 0);
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 1);
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 0);
        ir_checks::verif_has_attr_as(
            diagnostics,
            self.generic(),
            MATH_CONSTANT_ATTR_VAL,
            |attr| {
                let ty = match attr.get_type() {
                    Some(ty) => ty,
                    None => return false,
                };
                match ty {
                    Type::Int(_) | Type::Float(_) => true,
                    _ => false,
                }
            },
            "scalar attribute",
        );
        // TODO: verify val attr.
    }

    fn custom_print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        // Print the attribute
        printer.print(self.val())
    }

    fn custom_parse(
        parser: &mut IRParser,
        _ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        // Parse the val attribute.
        let val: Attribute = parser.parse()?;
        let ty = val.get_type().expect("`val` must be a typed attr").clone();
        st.set_attr(MATH_CONSTANT_ATTR_VAL, val);
        st.set_outputs_types(vec![ty]);
        Some(())
    }
}

// TODO: Find a way to get rid of this / generate this.
#[derive(Default)]
pub struct MathConstantOpBuiltinInterfaceWrapperImpl;
impl BuiltinOpInterfaceWrapper for MathConstantOpBuiltinInterfaceWrapperImpl {
    fn verify(&self, ctx: &IRContext, op: OperationID, diagnostics: &mut DiagnosticsEmitter) {
        let op = MathConstantOp::make_from_data(ctx, ctx.get_operation_data(op));
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
            MathConstantOp::make_from_data(ctx, ctx.get_operation_data(op)),
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
        MathConstantOp::custom_parse(parser, ctx, st)
    }

    fn clone(&self) -> Box<dyn BuiltinOpInterfaceWrapper> {
        Box::new(MathConstantOpBuiltinInterfaceWrapperImpl {})
    }
}

fn register_math_constant_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(MATH_CONSTANT_OPNAME);
    infos.set_impl::<MathConstantOp>();
    infos.set_builtin_interface::<MathConstantOpBuiltinInterfaceWrapperImpl>();
    infos.add_interface::<MathConstantOpConstantOpInterfaceImpl>();
    infos.set_tags(&[TAG_PURE_OP]);
    ctx.register_operation(infos.build());
}

/////////////////////////////////////////////////////////////////////////
// ConstantOp interface implementation for MathConstantOp
/////////////////////////////////////////////////////////////////////////

// ConstantOp interface implementation.
impl<'a> MathConstantOp<'a> {
    fn get_value(&self) -> &'a Attribute {
        self.val()
    }
}

// Wrapper struct for the ConstantOp interface implementation.
pub struct MathConstantOpConstantOpInterfaceImpl;

impl ConstantOpInterfaceImpl for MathConstantOpConstantOpInterfaceImpl {
    fn get_value<'a>(&self, ctx: &'a IRContext, data: &'a OperationData) -> &'a Attribute {
        GenericOperation::make_from_data(ctx, data)
            .cast::<MathConstantOp>()
            .unwrap()
            .get_value()
    }
}

impl OpInterfaceBuilder for MathConstantOpConstantOpInterfaceImpl {
    type InterfaceObjectType = ConstantOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = ConstantOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

/////////////////////////////////////////////////////////////////////////
// MathIAdd implementation
/////////////////////////////////////////////////////////////////////////

pub struct MathIAddOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

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

impl<'a> MathIAddOp<'a> {
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

const MATH_IADD_OPNAME: &'static str = "math.iadd";
const MATH_IADD_TYPE_UID: OperationTypeUID = OperationTypeUID::make_from_opname(MATH_IADD_OPNAME);

impl<'a> OperationImpl<'a> for MathIAddOp<'a> {
    fn make_from_data(ctx: &'a IRContext, data: &'a OperationData) -> Self {
        Self { ctx, data }
    }

    fn get_op_type_uid() -> OperationTypeUID {
        MATH_IADD_TYPE_UID
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
                format!("MathIAddOp must have integer type inputs"),
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
pub struct MathIAddOpBuiltinInterfaceWrapperImpl;
impl BuiltinOpInterfaceWrapper for MathIAddOpBuiltinInterfaceWrapperImpl {
    fn verify(&self, ctx: &IRContext, op: OperationID, diagnostics: &mut DiagnosticsEmitter) {
        let op = MathIAddOp::make_from_data(ctx, ctx.get_operation_data(op));
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
            MathIAddOp::make_from_data(ctx, ctx.get_operation_data(op)),
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
        MathIAddOp::custom_parse(parser, ctx, st)
    }

    fn clone(&self) -> Box<dyn BuiltinOpInterfaceWrapper> {
        Box::new(MathIAddOpBuiltinInterfaceWrapperImpl {})
    }
}

fn register_math_iadd_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(MATH_IADD_OPNAME);
    infos.set_impl::<MathIAddOp>();
    infos.set_builtin_interface::<MathIAddOpBuiltinInterfaceWrapperImpl>();
    infos.set_tags(&[TAG_PURE_OP]);
    ctx.register_operation(infos.build());
}

/////////////////////////////////////////////////////////////////////////
// MathOps implementations
/////////////////////////////////////////////////////////////////////////

pub fn register_math_ops(ctx: &mut IRContext) {
    register_math_iadd_op(ctx);
    register_math_constant_op(ctx);
}

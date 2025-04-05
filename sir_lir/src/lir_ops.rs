use diagnostics::diagnostics::{emit_error, DiagnosticsEmitter, ErrorOrSuccess};
use parse::lexer::TokenValue;
use parse::parser::Parser;
use sir_core::ir_printer::IRPrintableObject;
use sir_core::{
    attributes::{Attribute, AttributeSubClass, IntegerAttr, PointerAttr},
    ir_builder::OpImplBuilderState,
    ir_context::IRContext,
    ir_data::{OperationData, ValueID},
    ir_parser::{IRParsableObject, IRParser, OperationParserState},
    ir_printer::IRPrinter,
    ir_verifier::ir_checks,
    op_interfaces::{
        BuiltinOp, BuiltinOpInterfaceImpl, BuiltinOpInterfaceImplWrapper, OpInterfaceBuilder,
        OpInterfaceWrapper,
    },
    op_tags::TAG_PURE_OP,
    operation::{GenericOperation, OperationImpl},
    operation_type::{OperationTypeBuilder, OperationTypeUID},
    types::{IntegerType, PointerType, Type},
    value::Value,
};
use sir_interpreter::{
    interfaces::{
        InterpretableComputeOp, InterpretableComputeOpInterfaceImpl,
        InterpretableComputeOpInterfaceImplWrapper, InterpretableOp, InterpretableOpInterfaceImpl,
        InterpretableOpInterfaceImplWrapper,
    },
    interpreter::SIRInterpreter,
};
use sir_low_level::{low_level_types, tags::TAG_LOW_LEVEL_OP};

/////////////////////////////////////////////////////////////////////////
// LIRType implementation
/////////////////////////////////////////////////////////////////////////

// Predicate function that value is a scalar
// @XGENDEF PredIsLIRScalarType = {{ low_level_types::is_valid_scalar_type(ty) }}

// Predicate function that value is a scalar integer
// @XGENDEF PredIsLIRScalarIntType = {{ low_level_types::pred_valid_scalar_int_type(ty) }}

// Predicate function that value is a scalar floating point
// @XGENDEF PredIsLIRScalarFloatType = {{ low_level_types::pred_valid_scalar_float_type(ty) }}

// LIR Scalar Value
// @XGENDEF LIRScalarValue<Name> : ValueWithPred<Name, PredIsLIRScalarType, "LIR scalar">

// LIR Int Scalar Value
// @XGENDEF LIRIntScalarValue<Name> : ValueWithPred<Name, PredIsLIRScalarIntType, "LIR integer scalar">

// LIR Float Scalar Value
// @XGENDEF LIRFloatScalarValue<Name> : ValueWithPred<Name, PredIsLIRScalarFloatType, "LIR floating point scalar">

/////////////////////////////////////////////////////////////////////////
// LIRIAddOp implementation
/////////////////////////////////////////////////////////////////////////

// @XGENDEF:SIROp LIRIAddOp
// @opname "lir.iadd"
// @+input LIRIntScalarValue<"lhs">
// @+input LIRIntScalarValue<"rhs">
// @+output LIRIntScalarValue<"result">
// @tags ["TAG_PURE_OP", "TAG_LOW_LEVEL_OP"]
// @interfaces [InterpretableComputeOp]
// @+mod SameInputsAndOutputsTypes
// @custom_print_parse
// @disable_default_builder

// @XGENBEGIN

// Code automatically generated by sir_core/scripts/xgen.py

const L_I_R_I_ADD_OPNAME: &'static str = "lir.iadd";
const L_I_R_I_ADD_TYPE_UID: OperationTypeUID =
    OperationTypeUID::make_from_opname(L_I_R_I_ADD_OPNAME);

pub struct LIRIAddOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl<'a> OperationImpl<'a> for LIRIAddOp<'a> {
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
        L_I_R_I_ADD_TYPE_UID
    }
}

impl<'a> LIRIAddOp<'a> {
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
pub struct LIRIAddOpBuiltinOpInterfaceImpl;

impl BuiltinOpInterfaceImpl for LIRIAddOpBuiltinOpInterfaceImpl {
    fn verify<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        diagnostics: &mut DiagnosticsEmitter,
    ) -> ErrorOrSuccess {
        GenericOperation::make_from_data(ctx, data)
            .cast::<LIRIAddOp>()
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
            .cast::<LIRIAddOp>()
            .unwrap()
            .custom_print(printer)
    }
    fn custom_parse(
        &self,
        parser: &mut IRParser,
        ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        LIRIAddOp::custom_parse(parser, ctx, st)
    }
    fn clone(&self) -> BuiltinOpInterfaceImplWrapper {
        LIRIAddOp::clone()
    }
}

// Interface builder for BuiltinOp interface.
impl OpInterfaceBuilder for LIRIAddOpBuiltinOpInterfaceImpl {
    type InterfaceObjectType = BuiltinOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = BuiltinOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

// Wrapper struct for the InterpretableComputeOp interface implementation.
#[derive(Default)]
pub struct LIRIAddOpInterpretableComputeOpInterfaceImpl;

impl InterpretableComputeOpInterfaceImpl for LIRIAddOpInterpretableComputeOpInterfaceImpl {
    fn interpret<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        inputs: &[&Attribute],
    ) -> Vec<Attribute> {
        GenericOperation::make_from_data(ctx, data)
            .cast::<LIRIAddOp>()
            .unwrap()
            .interpret(inputs)
    }
    fn fold_with_canonicalize<'a>(&self, ctx: &'a IRContext, data: &'a OperationData) -> bool {
        GenericOperation::make_from_data(ctx, data)
            .cast::<LIRIAddOp>()
            .unwrap()
            .fold_with_canonicalize()
    }
}

// Interface builder for InterpretableComputeOp interface.
impl OpInterfaceBuilder for LIRIAddOpInterpretableComputeOpInterfaceImpl {
    type InterfaceObjectType = InterpretableComputeOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = InterpretableComputeOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

impl<'a> LIRIAddOp<'a> {
    pub fn verify(&self, diagnostics: &mut DiagnosticsEmitter) -> ErrorOrSuccess {
        ir_checks::verif_inputs_count(diagnostics, self.generic(), 2)?;
        ir_checks::verif_io_type(
            diagnostics,
            self.generic(),
            true,
            0,
            "lhs",
            |ty| low_level_types::pred_valid_scalar_int_type(ty),
            "LIR integer scalar",
        )?;
        ir_checks::verif_io_type(
            diagnostics,
            self.generic(),
            true,
            1,
            "rhs",
            |ty| low_level_types::pred_valid_scalar_int_type(ty),
            "LIR integer scalar",
        )?;
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 1)?;
        ir_checks::verif_io_type(
            diagnostics,
            self.generic(),
            false,
            0,
            "result",
            |ty| low_level_types::pred_valid_scalar_int_type(ty),
            "LIR integer scalar",
        )?;
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 0)?;
        ir_checks::verif_same_input_output_types(diagnostics, self.generic())?;
        Ok(())
    }
    pub fn clone() -> BuiltinOpInterfaceImplWrapper {
        BuiltinOpInterfaceImplWrapper::new(Box::new(LIRIAddOpBuiltinOpInterfaceImpl))
    }
}

fn register_l_i_r_i_add_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(L_I_R_I_ADD_OPNAME);
    infos.set_impl::<LIRIAddOp>();
    infos.set_builtin_interface::<LIRIAddOpBuiltinOpInterfaceImpl>();
    infos.set_tags(&[TAG_PURE_OP, TAG_LOW_LEVEL_OP]);
    infos.add_interface::<LIRIAddOpBuiltinOpInterfaceImpl>();
    infos.add_interface::<LIRIAddOpInterpretableComputeOpInterfaceImpl>();
    ctx.register_operation(infos.build());
}

// @XGENEND

impl LIRIAddOp<'_> {
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

    // pub fn build_generic<InputsValues: Into<Vec<ValueID>>,
    //                      OutputsTypes: Into<Vec<Type>>,
    //                      Attrs: Into<DictAttr>,
    //                      Blocks: Into<Vec<BlockID>>>(
    //     inputs: InputsValues,
    //     outputs_types: OutputsTypes,
    //     attrs_dict: Attrs,
    //     blocks: Blocks,
    // ) -> OpImplBuilderState<Self> {
    //     let mut st = OpImplBuilderState::make();
    //     st.set_inputs(inputs);
    //     st.set_outputs_types(outputs_types);
    //     st.set_attrs_dict(attrs_dict);
    //     st.set_blocks(blocks);
    //     st
    // }
}

// BuiltinOp interface implementation.
impl<'a> LIRIAddOp<'a> {
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

    // InterpretableComputeOp implementation

    pub fn interpret(&self, inputs: &[&Attribute]) -> Vec<Attribute> {
        assert_eq!(inputs.len(), 2);
        let lhs = inputs[0].cast::<IntegerAttr>().unwrap();
        let rhs = inputs[1].cast::<IntegerAttr>().unwrap();

        let width = lhs.bitwidth();

        // Do a wrapping add with the right types.

        let res = if width == 8 {
            let lhs = lhs.raw_val() as u8;
            let rhs = rhs.raw_val() as u8;
            u8::wrapping_add(lhs, rhs) as u64
        } else if width == 16 {
            let lhs = lhs.raw_val() as u16;
            let rhs = rhs.raw_val() as u16;
            u16::wrapping_add(lhs, rhs) as u64
        } else if width == 32 {
            let lhs = lhs.raw_val() as u32;
            let rhs = rhs.raw_val() as u32;
            u32::wrapping_add(lhs, rhs) as u64
        } else if width == 64 {
            let lhs = lhs.raw_val() as u64;
            let rhs = rhs.raw_val() as u64;
            u64::wrapping_add(lhs, rhs) as u64
        } else {
            panic!("unsupported bitwdith `{}`", width);
        };

        vec![IntegerAttr::new(res, lhs.get_type().unwrap().clone())]
    }

    pub fn fold_with_canonicalize(&self) -> bool {
        true
    }
}

/////////////////////////////////////////////////////////////////////////
// LIRLoadOp implementation
/////////////////////////////////////////////////////////////////////////

// @XGENDEF:SIROp LIRLoadOp
// @opname "lir.load"
// @+input PointerValue<"ptr">
// @+output AnyValue<"result">
// @tags ["TAG_LOW_LEVEL_OP"]
// @interfaces [InterpretableOp]
// @verifier
// @custom_print_parse

// @XGENBEGIN

// Code automatically generated by sir_core/scripts/xgen.py

const L_I_R_LOAD_OPNAME: &'static str = "lir.load";
const L_I_R_LOAD_TYPE_UID: OperationTypeUID = OperationTypeUID::make_from_opname(L_I_R_LOAD_OPNAME);

pub struct LIRLoadOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl<'a> OperationImpl<'a> for LIRLoadOp<'a> {
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
        L_I_R_LOAD_TYPE_UID
    }
}

impl<'a> LIRLoadOp<'a> {
    pub fn get_ptr(&self) -> Value<'a> {
        self.get_input(0)
    }

    pub fn get_result(&self) -> Value<'a> {
        self.get_output(0)
    }
}

// Wrapper struct for the BuiltinOp interface implementation.
#[derive(Default)]
pub struct LIRLoadOpBuiltinOpInterfaceImpl;

impl BuiltinOpInterfaceImpl for LIRLoadOpBuiltinOpInterfaceImpl {
    fn verify<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        diagnostics: &mut DiagnosticsEmitter,
    ) -> ErrorOrSuccess {
        GenericOperation::make_from_data(ctx, data)
            .cast::<LIRLoadOp>()
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
            .cast::<LIRLoadOp>()
            .unwrap()
            .custom_print(printer)
    }
    fn custom_parse(
        &self,
        parser: &mut IRParser,
        ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        LIRLoadOp::custom_parse(parser, ctx, st)
    }
    fn clone(&self) -> BuiltinOpInterfaceImplWrapper {
        LIRLoadOp::clone()
    }
}

// Interface builder for BuiltinOp interface.
impl OpInterfaceBuilder for LIRLoadOpBuiltinOpInterfaceImpl {
    type InterfaceObjectType = BuiltinOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = BuiltinOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

// Wrapper struct for the InterpretableOp interface implementation.
#[derive(Default)]
pub struct LIRLoadOpInterpretableOpInterfaceImpl;

impl InterpretableOpInterfaceImpl for LIRLoadOpInterpretableOpInterfaceImpl {
    fn interpret<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        interpreter: &mut SIRInterpreter,
    ) {
        GenericOperation::make_from_data(ctx, data)
            .cast::<LIRLoadOp>()
            .unwrap()
            .interpret(interpreter)
    }
}

// Interface builder for InterpretableOp interface.
impl OpInterfaceBuilder for LIRLoadOpInterpretableOpInterfaceImpl {
    type InterfaceObjectType = InterpretableOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = InterpretableOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

impl<'a> LIRLoadOp<'a> {
    pub fn build(ptr: ValueID, result: Type) -> OpImplBuilderState<Self> {
        let mut st = OpImplBuilderState::make();
        st.set_inputs(vec![ptr]);
        st.set_outputs_types(vec![result]);
        st
    }
    pub fn verify(&self, diagnostics: &mut DiagnosticsEmitter) -> ErrorOrSuccess {
        ir_checks::verif_inputs_count(diagnostics, self.generic(), 1)?;
        ir_checks::verif_io_is_of_type::<PointerType>(diagnostics, self.generic(), true, 0, "ptr")?;
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 1)?;
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 0)?;
        self.verify_op(diagnostics)?;
        Ok(())
    }
    pub fn clone() -> BuiltinOpInterfaceImplWrapper {
        BuiltinOpInterfaceImplWrapper::new(Box::new(LIRLoadOpBuiltinOpInterfaceImpl))
    }
}

fn register_l_i_r_load_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(L_I_R_LOAD_OPNAME);
    infos.set_impl::<LIRLoadOp>();
    infos.set_builtin_interface::<LIRLoadOpBuiltinOpInterfaceImpl>();
    infos.set_tags(&[TAG_LOW_LEVEL_OP]);
    infos.add_interface::<LIRLoadOpBuiltinOpInterfaceImpl>();
    infos.add_interface::<LIRLoadOpInterpretableOpInterfaceImpl>();
    ctx.register_operation(infos.build());
}

// @XGENEND

// LIRLoadOp BuiltinOp implementation
impl<'a> LIRLoadOp<'a> {
    fn custom_print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        printer.print_op_results_and_opname(self.generic(), true)?;

        printer.print_value_label_or_unknown(self.get_ptr().into())?;
        write!(printer.os(), " : ")?;
        printer.print(self.get_ptr().get_type())
    }

    fn custom_parse(
        parser: &mut IRParser,
        _ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        // Parse the ptr input.
        let mut inputs_names = vec![];
        inputs_names.push(parser.parse_value_ref()?);

        // Parse the pointer type.
        let mut inputs_types = vec![];
        let mut outputs_types = vec![];
        parser.consume_sym_or_error(TokenValue::sym_colon())?;
        let ty = Type::parse(parser)?;
        inputs_types.push(ty.clone());

        // Get the element type for the output.
        let ptr_ty = match ty.cast::<PointerType>() {
            Some(ptr_ty) => ptr_ty,
            None => {
                let ty_loc = parser.get_last_token_loc();
                emit_error(
                    parser,
                    &ty_loc,
                    format!(
                        "LIRLoadOp must have a pointer type, but got `{}`",
                        ty.to_string_repr()
                    ),
                );
                return None;
            }
        };
        outputs_types.push(ptr_ty.element().clone());

        st.set_inputs_names(inputs_names);
        st.set_inputs_types(inputs_types);
        st.set_outputs_types(outputs_types);
        Some(())
    }

    fn verify_op(&self, diagnostics: &mut DiagnosticsEmitter) -> ErrorOrSuccess {
        let ptr_elem_ty = self
            .get_ptr()
            .get_type()
            .cast::<PointerType>()
            .unwrap()
            .element();
        let val_ty = self.get_result().get_type();
        if ptr_elem_ty != val_ty {
            emit_error(
                diagnostics,
                &self.generic(),
                format!(
                    "Pointer element type {} differs from value type {}",
                    ptr_elem_ty.to_string_repr(),
                    val_ty.to_string_repr()
                ),
            );
            return Err(());
        }

        Ok(())
    }
}

// InterpretableOp interface implementation for LIRLoadOp.
impl<'a> LIRLoadOp<'a> {
    pub fn interpret(&self, interpreter: &mut SIRInterpreter) {
        let addr = interpreter
            .get_value(self.get_ptr())
            .cast::<PointerAttr>()
            .unwrap()
            .val();
        let val = interpreter.get_mem_ref_unsafe::<Attribute>(addr);
        interpreter.set_value(self.get_result(), val.clone());
        interpreter.move_pc_to_next_instruction();
    }
}

/////////////////////////////////////////////////////////////////////////
// LIRStoreOp implementation
/////////////////////////////////////////////////////////////////////////

// @XGENDEF:SIROp LIRStoreOp
// @opname "lir.store"
// @+input AnyValue<"value">
// @+input PointerValue<"ptr">
// @tags ["TAG_LOW_LEVEL_OP"]
// @verifier
// @interfaces [InterpretableOp]
// @custom_print_parse

// @XGENBEGIN

// Code automatically generated by sir_core/scripts/xgen.py

const L_I_R_STORE_OPNAME: &'static str = "lir.store";
const L_I_R_STORE_TYPE_UID: OperationTypeUID =
    OperationTypeUID::make_from_opname(L_I_R_STORE_OPNAME);

pub struct LIRStoreOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl<'a> OperationImpl<'a> for LIRStoreOp<'a> {
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
        L_I_R_STORE_TYPE_UID
    }
}

impl<'a> LIRStoreOp<'a> {
    pub fn get_value(&self) -> Value<'a> {
        self.get_input(0)
    }

    pub fn get_ptr(&self) -> Value<'a> {
        self.get_input(1)
    }
}

// Wrapper struct for the BuiltinOp interface implementation.
#[derive(Default)]
pub struct LIRStoreOpBuiltinOpInterfaceImpl;

impl BuiltinOpInterfaceImpl for LIRStoreOpBuiltinOpInterfaceImpl {
    fn verify<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        diagnostics: &mut DiagnosticsEmitter,
    ) -> ErrorOrSuccess {
        GenericOperation::make_from_data(ctx, data)
            .cast::<LIRStoreOp>()
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
            .cast::<LIRStoreOp>()
            .unwrap()
            .custom_print(printer)
    }
    fn custom_parse(
        &self,
        parser: &mut IRParser,
        ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        LIRStoreOp::custom_parse(parser, ctx, st)
    }
    fn clone(&self) -> BuiltinOpInterfaceImplWrapper {
        LIRStoreOp::clone()
    }
}

// Interface builder for BuiltinOp interface.
impl OpInterfaceBuilder for LIRStoreOpBuiltinOpInterfaceImpl {
    type InterfaceObjectType = BuiltinOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = BuiltinOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

// Wrapper struct for the InterpretableOp interface implementation.
#[derive(Default)]
pub struct LIRStoreOpInterpretableOpInterfaceImpl;

impl InterpretableOpInterfaceImpl for LIRStoreOpInterpretableOpInterfaceImpl {
    fn interpret<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        interpreter: &mut SIRInterpreter,
    ) {
        GenericOperation::make_from_data(ctx, data)
            .cast::<LIRStoreOp>()
            .unwrap()
            .interpret(interpreter)
    }
}

// Interface builder for InterpretableOp interface.
impl OpInterfaceBuilder for LIRStoreOpInterpretableOpInterfaceImpl {
    type InterfaceObjectType = InterpretableOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = InterpretableOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

impl<'a> LIRStoreOp<'a> {
    pub fn build(value: ValueID, ptr: ValueID) -> OpImplBuilderState<Self> {
        let mut st = OpImplBuilderState::make();
        st.set_inputs(vec![value, ptr]);
        st
    }
    pub fn verify(&self, diagnostics: &mut DiagnosticsEmitter) -> ErrorOrSuccess {
        ir_checks::verif_inputs_count(diagnostics, self.generic(), 2)?;
        ir_checks::verif_io_is_of_type::<PointerType>(diagnostics, self.generic(), true, 1, "ptr")?;
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 0)?;
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 0)?;
        self.verify_op(diagnostics)?;
        Ok(())
    }
    pub fn clone() -> BuiltinOpInterfaceImplWrapper {
        BuiltinOpInterfaceImplWrapper::new(Box::new(LIRStoreOpBuiltinOpInterfaceImpl))
    }
}

fn register_l_i_r_store_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(L_I_R_STORE_OPNAME);
    infos.set_impl::<LIRStoreOp>();
    infos.set_builtin_interface::<LIRStoreOpBuiltinOpInterfaceImpl>();
    infos.set_tags(&[TAG_LOW_LEVEL_OP]);
    infos.add_interface::<LIRStoreOpBuiltinOpInterfaceImpl>();
    infos.add_interface::<LIRStoreOpInterpretableOpInterfaceImpl>();
    ctx.register_operation(infos.build());
}

// @XGENEND

// LIRStoreOp BuiltinOp implementation
impl<'a> LIRStoreOp<'a> {
    fn custom_print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        printer.print_op_results_and_opname(self.generic(), true)?;

        printer.print_value_label_or_unknown(self.get_value().into())?;
        write!(printer.os(), ", ")?;
        printer.print_value_label_or_unknown(self.get_ptr().into())?;
        write!(printer.os(), " : ")?;
        printer.print(self.get_ptr().get_type())
    }

    fn custom_parse(
        parser: &mut IRParser,
        _ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        // Parse the two inputs.
        let mut inputs_names = vec![];
        inputs_names.push(parser.parse_value_ref()?);
        parser.consume_sym_or_error(TokenValue::sym_comma())?;
        inputs_names.push(parser.parse_value_ref()?);

        // Parse the pointer type.
        let mut inputs_types = vec![];
        parser.consume_sym_or_error(TokenValue::sym_colon())?;
        let ty = Type::parse(parser)?;

        // Get the element type for the value.
        let ptr_ty = match ty.cast::<PointerType>() {
            Some(ptr_ty) => ptr_ty,
            None => {
                let ty_loc = parser.get_last_token_loc();
                emit_error(
                    parser,
                    &ty_loc,
                    format!(
                        "LIRStoreOp must have a pointer type, but got `{}`",
                        ty.to_string_repr()
                    ),
                );
                return None;
            }
        };
        inputs_types.push(ptr_ty.element().clone());
        inputs_types.push(ty);

        st.set_inputs_names(inputs_names);
        st.set_inputs_types(inputs_types);
        st.set_outputs_types(vec![]);
        Some(())
    }

    fn verify_op(&self, diagnostics: &mut DiagnosticsEmitter) -> ErrorOrSuccess {
        let ptr_elem_ty = self
            .get_ptr()
            .get_type()
            .cast::<PointerType>()
            .unwrap()
            .element();
        let val_ty = self.get_value().get_type();
        if ptr_elem_ty != val_ty {
            emit_error(
                diagnostics,
                &self.generic(),
                format!(
                    "Pointer element type {} differs from value type {}",
                    ptr_elem_ty.to_string_repr(),
                    val_ty.to_string_repr()
                ),
            );
            return Err(());
        }

        Ok(())
    }
}

// InterpretableOp interface implementation for LIRStoreOp.
impl<'a> LIRStoreOp<'a> {
    pub fn interpret(&self, interpreter: &mut SIRInterpreter) {
        let val = interpreter.get_value(self.get_value()).clone();
        let addr = interpreter
            .get_value(self.get_ptr())
            .cast::<PointerAttr>()
            .unwrap()
            .val();
        interpreter.write_mem_data(addr, val);
        interpreter.move_pc_to_next_instruction();
    }
}

/////////////////////////////////////////////////////////////////////////
// LIRAllocaOp implementation
/////////////////////////////////////////////////////////////////////////

// @XGENDEF:SIROp LIRAllocaOp
// @opname "lir.alloca"
// @+attr IndexAttr<"align">
// @+output PointerValue<"result">
// @tags ["TAG_LOW_LEVEL_OP"]
// @interfaces [InterpretableOp]
// @custom_print_parse

// @XGENBEGIN

// Code automatically generated by sir_core/scripts/xgen.py

const L_I_R_ALLOCA_OPNAME: &'static str = "lir.alloca";
const L_I_R_ALLOCA_TYPE_UID: OperationTypeUID =
    OperationTypeUID::make_from_opname(L_I_R_ALLOCA_OPNAME);

pub struct LIRAllocaOp<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl<'a> OperationImpl<'a> for LIRAllocaOp<'a> {
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
        L_I_R_ALLOCA_TYPE_UID
    }
}

const L_I_R_ALLOCA_ATTR_ALIGN: &'static str = "align";

impl<'a> LIRAllocaOp<'a> {
    pub fn get_result(&self) -> Value<'a> {
        self.get_output(0)
    }

    pub fn get_align(&self) -> u64 {
        self.get_attr(L_I_R_ALLOCA_ATTR_ALIGN)
            .expect("Missing `align` attribute")
            .cast::<IntegerAttr>()
            .expect("`align` attribute must be an IntegerType")
            .raw_val()
    }

    pub fn get_align_attr(&self) -> &'a Attribute {
        self.get_attr(L_I_R_ALLOCA_ATTR_ALIGN)
            .expect("Missing `align` attribute")
    }
}

// Wrapper struct for the BuiltinOp interface implementation.
#[derive(Default)]
pub struct LIRAllocaOpBuiltinOpInterfaceImpl;

impl BuiltinOpInterfaceImpl for LIRAllocaOpBuiltinOpInterfaceImpl {
    fn verify<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        diagnostics: &mut DiagnosticsEmitter,
    ) -> ErrorOrSuccess {
        GenericOperation::make_from_data(ctx, data)
            .cast::<LIRAllocaOp>()
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
            .cast::<LIRAllocaOp>()
            .unwrap()
            .custom_print(printer)
    }
    fn custom_parse(
        &self,
        parser: &mut IRParser,
        ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        LIRAllocaOp::custom_parse(parser, ctx, st)
    }
    fn clone(&self) -> BuiltinOpInterfaceImplWrapper {
        LIRAllocaOp::clone()
    }
}

// Interface builder for BuiltinOp interface.
impl OpInterfaceBuilder for LIRAllocaOpBuiltinOpInterfaceImpl {
    type InterfaceObjectType = BuiltinOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = BuiltinOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

// Wrapper struct for the InterpretableOp interface implementation.
#[derive(Default)]
pub struct LIRAllocaOpInterpretableOpInterfaceImpl;

impl InterpretableOpInterfaceImpl for LIRAllocaOpInterpretableOpInterfaceImpl {
    fn interpret<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        interpreter: &mut SIRInterpreter,
    ) {
        GenericOperation::make_from_data(ctx, data)
            .cast::<LIRAllocaOp>()
            .unwrap()
            .interpret(interpreter)
    }
}

// Interface builder for InterpretableOp interface.
impl OpInterfaceBuilder for LIRAllocaOpInterpretableOpInterfaceImpl {
    type InterfaceObjectType = InterpretableOp<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {
        let wrapper = InterpretableOpInterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }
}

impl<'a> LIRAllocaOp<'a> {
    pub fn build(align: Attribute, result: Type) -> OpImplBuilderState<Self> {
        let mut st = OpImplBuilderState::make();
        st.set_attr(L_I_R_ALLOCA_ATTR_ALIGN, align);
        st.set_outputs_types(vec![result]);
        st
    }
    pub fn verify(&self, diagnostics: &mut DiagnosticsEmitter) -> ErrorOrSuccess {
        ir_checks::verif_inputs_count(diagnostics, self.generic(), 0)?;
        ir_checks::verif_has_attr_as(
            diagnostics,
            self.generic(),
            L_I_R_ALLOCA_ATTR_ALIGN,
            |attr| ir_checks::pred_is_index_attribute(attr),
            "index type",
        )?;
        ir_checks::verif_outputs_count(diagnostics, self.generic(), 1)?;
        ir_checks::verif_io_is_of_type::<PointerType>(
            diagnostics,
            self.generic(),
            false,
            0,
            "result",
        )?;
        ir_checks::verif_blocks_count(diagnostics, self.generic(), 0)?;
        Ok(())
    }
    pub fn clone() -> BuiltinOpInterfaceImplWrapper {
        BuiltinOpInterfaceImplWrapper::new(Box::new(LIRAllocaOpBuiltinOpInterfaceImpl))
    }
}

fn register_l_i_r_alloca_op(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(L_I_R_ALLOCA_OPNAME);
    infos.set_impl::<LIRAllocaOp>();
    infos.set_builtin_interface::<LIRAllocaOpBuiltinOpInterfaceImpl>();
    infos.set_tags(&[TAG_LOW_LEVEL_OP]);
    infos.add_interface::<LIRAllocaOpBuiltinOpInterfaceImpl>();
    infos.add_interface::<LIRAllocaOpInterpretableOpInterfaceImpl>();
    ctx.register_operation(infos.build());
}

// @XGENEND

// LIRAllocaOp BuiltinOp implementation
impl<'a> LIRAllocaOp<'a> {
    fn custom_print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        printer.print_op_results_and_opname(self.generic(), true)?;
        printer.print(self.get_result().get_type())?;
        write!(printer.os(), ", align {}", self.get_align())
    }

    fn custom_parse(
        parser: &mut IRParser,
        _ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        // Parse the output type.
        let output_type = Type::parse(parser)?;
        parser.consume_sym_or_error(TokenValue::sym_comma())?;

        // Parse the align value.
        parser.consume_identifier_val_or_error("align")?;
        let align = parser.consume_int_or_error()?.get_int().unwrap();
        let align = IntegerAttr::new(align, IntegerType::index_type());

        st.set_inputs_names(vec![]);
        st.set_inputs_types(vec![]);
        st.set_attr(L_I_R_ALLOCA_ATTR_ALIGN, align);
        st.set_outputs_types(vec![output_type]);
        Some(())
    }
}

// InterpretableOp interface implementation for LIRAllocaOp.
impl<'a> LIRAllocaOp<'a> {
    pub fn interpret(&self, interpreter: &mut SIRInterpreter) {
        let addr = interpreter.alloca(std::mem::size_of::<Attribute>(), self.get_align() as usize);
        let addr = PointerAttr::new(addr, self.get_result().get_type().clone());
        interpreter.set_value(self.get_result(), addr);
        interpreter.move_pc_to_next_instruction();
    }
}

/////////////////////////////////////////////////////////////////////////
// LIROps Registrations
/////////////////////////////////////////////////////////////////////////

// @XGENBEGIN RegisterOps register_lir_ops
pub fn register_lir_ops(ctx: &mut IRContext) {
    register_l_i_r_i_add_op(ctx);
    register_l_i_r_load_op(ctx);
    register_l_i_r_store_op(ctx);
    register_l_i_r_alloca_op(ctx);
}

// @XGENEND

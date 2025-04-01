use std::any::Any;

use const_fnv1a_hash::fnv1a_hash_str_64;
use diagnostics::diagnostics::DiagnosticsEmitter;

use crate::{
    attributes::Attribute,
    ir_context::IRContext,
    ir_data::OperationData,
    ir_parser::{IRParser, OperationParserState},
    ir_printer::IRPrinter,
    operation::OperationImpl,
    operation_type::OperationTypeUID,
};

/////////////////////////////////////////////////////////////////////////
// OpInterface Base
/////////////////////////////////////////////////////////////////////////

// Base building blocks to build custom op interfaces.

// Represents a unique identifier for every Op Interface
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OpInterfaceUID(u64);

impl OpInterfaceUID {
    pub const fn make_from_interface_identifier(id: &'static str) -> Self {
        Self(fnv1a_hash_str_64(id))
    }
}

// Class implemented by all wrapper objects of an interface.
// We need wrapper objects to dispatch from a dynamic Interface object to a specific interface object.
// This trait must be implemented for each newly defined interface.
pub trait OpInterfaceWrapper {
    fn as_any(&self) -> &dyn Any;
}

// This is the trait implemented by the wrapper object that holds a dynamic object corresponding to an implementation of the interface.
// This trait must be defined for each newly defined interface.
pub trait OpInterfaceObject<'a> {
    fn get_interface_uid() -> OpInterfaceUID;

    fn make(
        wrapper: &'a dyn OpInterfaceWrapper,
        ctx: &'a IRContext,
        data: &'a OperationData,
    ) -> Self;
}

// This must be implemented to build a `OpInterfaceWrapper` for a specific Interface and a specific operation class.
// InterfaceObjectType is used to figure out which interface is implemented.
// This must be specified every time an op implements an interface.
pub trait OpInterfaceBuilder {
    // Type of the interface being built.
    type InterfaceObjectType: OpInterfaceObject<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static>;
}

/////////////////////////////////////////////////////////////////////////
// BuiltinOp Interface
/////////////////////////////////////////////////////////////////////////

// This interface must be implemented by every operation.

// @TODO[I1][SIR-CORE]: Remove BuiltinOpInterfaceWrapper.clone() hack

// @XGENDEF:SIRInterface BuiltinOp
// @+method {{ verify(diagnostics: "&mut DiagnosticsEmitter") }}
// @+method {{ custom_print(printer: "&mut IRPrinter")
//   -> "Result<(), std::io::Error>" }}
// @+staticmethod {{ custom_parse(parser: "&mut IRParser", ctx: "&mut IRContext", st: "&mut OperationParserState")
//   -> "Option<()>" }}
// @+staticmethod {{ clone() -> "BuiltinOpInterfaceImplWrapper" }}

// @XGENBEGIN
// Unique identifier for the BuiltinOp Interface
const BUILTIN_OP_INTERFACE_UID: OpInterfaceUID =
    OpInterfaceUID::make_from_interface_identifier("BuiltinOp");

// Interface Implementation for the BuiltinOp interface.
pub trait BuiltinOpInterfaceImpl {
    fn verify<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        diagnostics: &mut DiagnosticsEmitter,
    );
    fn custom_print<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        printer: &mut IRPrinter,
    ) -> Result<(), std::io::Error>;
    fn custom_parse(
        &self,
        parser: &mut IRParser,
        ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()>;
    fn clone(&self) -> BuiltinOpInterfaceImplWrapper;
}

// Wrapper object holding an implementation of the BuiltinOp interface.
pub struct BuiltinOpInterfaceImplWrapper {
    dyn_impl: Box<dyn BuiltinOpInterfaceImpl>,
}

impl BuiltinOpInterfaceImplWrapper {
    pub fn new(dyn_impl: Box<dyn BuiltinOpInterfaceImpl>) -> Self {
        Self { dyn_impl }
    }
}

impl OpInterfaceWrapper for BuiltinOpInterfaceImplWrapper {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

//Definition of helper functions for the static methods of BuiltinOp interface.
impl BuiltinOpInterfaceImplWrapper {
    pub fn custom_parse(
        &self,
        parser: &mut IRParser,
        ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()> {
        self.dyn_impl.custom_parse(parser, ctx, st)
    }
    pub fn clone(&self) -> BuiltinOpInterfaceImplWrapper {
        self.dyn_impl.clone()
    }
}

// Object to materialize an operation that implements the BuiltinOp interface.
pub struct BuiltinOp<'a> {
    wrapper: &'a BuiltinOpInterfaceImplWrapper,
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl<'a> OpInterfaceObject<'a> for BuiltinOp<'a> {
    fn get_interface_uid() -> OpInterfaceUID {
        BUILTIN_OP_INTERFACE_UID
    }

    fn make(
        wrapper: &'a dyn OpInterfaceWrapper,
        ctx: &'a IRContext,
        data: &'a OperationData,
    ) -> Self {
        let wrapper = wrapper
            .as_any()
            .downcast_ref::<BuiltinOpInterfaceImplWrapper>()
            .unwrap();
        Self { ctx, data, wrapper }
    }
}

impl<'a> OperationImpl<'a> for BuiltinOp<'a> {
    fn make_from_data(_ctx: &'a IRContext, _data: &'a OperationData) -> Self {
        panic!("Not supported for InterfaceOp")
    }

    fn get_op_data(&self) -> &'a OperationData {
        &self.data
    }

    fn get_context(&self) -> &'a IRContext {
        &self.ctx
    }

    fn get_op_type_uid() -> OperationTypeUID {
        panic!("Cannot get the TypeUID of an InterfaceOp")
    }
}

// Methods implementation for BuiltinOp.
impl<'a> BuiltinOp<'a> {
    pub fn verify(&self, diagnostics: &mut DiagnosticsEmitter) {
        self.wrapper
            .dyn_impl
            .verify(&self.ctx, &self.data, diagnostics)
    }
    pub fn custom_print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        self.wrapper
            .dyn_impl
            .custom_print(&self.ctx, &self.data, printer)
    }
}

// @XGENEND

impl<'a> BuiltinOp<'a> {
    // Special implementation since the BuiltinOpInterface isn't dynamic like the others.
    // We can create it more easily.
    pub(crate) fn get_from_builtin_interface(
        wrapper: &'a BuiltinOpInterfaceImplWrapper,
        ctx: &'a IRContext,
        data: &'a OperationData,
    ) -> Self {
        Self { wrapper, ctx, data }
    }
}

/////////////////////////////////////////////////////////////////////////
// ConstantOp Interface
/////////////////////////////////////////////////////////////////////////

// This interface can be implemented by any op which is constant, and hold a constant attribute value.

// @XGENDEF:SIRInterface ConstantOp
// @+method {{ get_value() -> "&'a Attribute" }}

// @XGENBEGIN
// Unique identifier for the ConstantOp Interface
const CONSTANT_OP_INTERFACE_UID: OpInterfaceUID =
    OpInterfaceUID::make_from_interface_identifier("ConstantOp");

// Interface Implementation for the ConstantOp interface.
pub trait ConstantOpInterfaceImpl {
    fn get_value<'a>(&self, ctx: &'a IRContext, data: &'a OperationData) -> &'a Attribute;
}

// Wrapper object holding an implementation of the ConstantOp interface.
pub struct ConstantOpInterfaceImplWrapper {
    dyn_impl: Box<dyn ConstantOpInterfaceImpl>,
}

impl ConstantOpInterfaceImplWrapper {
    pub fn new(dyn_impl: Box<dyn ConstantOpInterfaceImpl>) -> Self {
        Self { dyn_impl }
    }
}

impl OpInterfaceWrapper for ConstantOpInterfaceImplWrapper {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

//Definition of helper functions for the static methods of ConstantOp interface.
impl ConstantOpInterfaceImplWrapper {}

// Object to materialize an operation that implements the ConstantOp interface.
pub struct ConstantOp<'a> {
    wrapper: &'a ConstantOpInterfaceImplWrapper,
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl<'a> OpInterfaceObject<'a> for ConstantOp<'a> {
    fn get_interface_uid() -> OpInterfaceUID {
        CONSTANT_OP_INTERFACE_UID
    }

    fn make(
        wrapper: &'a dyn OpInterfaceWrapper,
        ctx: &'a IRContext,
        data: &'a OperationData,
    ) -> Self {
        let wrapper = wrapper
            .as_any()
            .downcast_ref::<ConstantOpInterfaceImplWrapper>()
            .unwrap();
        Self { ctx, data, wrapper }
    }
}

impl<'a> OperationImpl<'a> for ConstantOp<'a> {
    fn make_from_data(_ctx: &'a IRContext, _data: &'a OperationData) -> Self {
        panic!("Not supported for InterfaceOp")
    }

    fn get_op_data(&self) -> &'a OperationData {
        &self.data
    }

    fn get_context(&self) -> &'a IRContext {
        &self.ctx
    }

    fn get_op_type_uid() -> OperationTypeUID {
        panic!("Cannot get the TypeUID of an InterfaceOp")
    }
}

// Methods implementation for ConstantOp.
impl<'a> ConstantOp<'a> {
    pub fn get_value(&self) -> &'a Attribute {
        self.wrapper.dyn_impl.get_value(&self.ctx, &self.data)
    }
}

// @XGENEND

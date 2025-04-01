use std::any::Any;

use const_fnv1a_hash::fnv1a_hash_str_64;
use diagnostics::diagnostics::DiagnosticsEmitter;

use crate::{
    attributes::Attribute,
    ir_context::IRContext,
    ir_data::{OperationData, OperationID},
    ir_parser::{IRParser, OperationParserState},
    ir_printer::IRPrinter,
    operation::OperationImpl,
};

/////////////////////////////////////////////////////////////////////////
// Builtin Operation Interface
/////////////////////////////////////////////////////////////////////////

// The builtin interface must be implemented by all operations.

// Wrapper for the builtin interface.
pub trait BuiltinOpInterfaceWrapper {
    fn verify(&self, ctx: &IRContext, op: OperationID, diagnostics: &mut DiagnosticsEmitter);

    fn custom_print(
        &self,
        printer: &mut IRPrinter,
        ctx: &IRContext,
        op: OperationID,
    ) -> Result<(), std::io::Error>;

    fn custom_parse(
        &self,
        parser: &mut IRParser,
        ctx: &mut IRContext,
        st: &mut OperationParserState,
    ) -> Option<()>;

    // @TODO[I1][SIR-CORE]: Remove BuiltinOpInterfaceWrapper.clone() hack
    fn clone(&self) -> Box<dyn BuiltinOpInterfaceWrapper>;
}

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
// ConstantOp Interface
/////////////////////////////////////////////////////////////////////////

// This interface can be implemented by any op which is constant, and hold a constant attribute value.

/// Unique identifier for the ConstantOp Interface
const CONSTANT_OP_INTERFACE_UID: OpInterfaceUID =
    OpInterfaceUID::make_from_interface_identifier("sir_core::constant_op");

/// Interface Implementation for the ConstantOp interface.
pub trait ConstantOpInterfaceImpl {
    // Returns the constant value of the operation.
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

// Object to materialize an operation that implements the ConstantOp interface.
pub struct ConstantOp<'a> {
    wrapper: &'a ConstantOpInterfaceImplWrapper,
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl<'a> ConstantOp<'a> {
    pub fn get_value(&self) -> &'a Attribute {
        self.wrapper.dyn_impl.get_value(&self.ctx, &self.data)
    }
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

    fn get_op_type_uid() -> crate::operation_type::OperationTypeUID {
        panic!("Cannot get the TypeUID of an InterfaceOp")
    }
}

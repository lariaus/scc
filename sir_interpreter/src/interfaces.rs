use std::any::Any;

use sir_core::{
    attributes::Attribute,
    ir_context::IRContext,
    ir_data::OperationData,
    op_interfaces::{OpInterfaceObject, OpInterfaceUID, OpInterfaceWrapper},
    operation::OperationImpl,
    operation_type::OperationTypeUID,
};

use crate::interpreter::SIRInterpreter;

/////////////////////////////////////////////////////////////////////////
// InterpretableComputeOp Interface
/////////////////////////////////////////////////////////////////////////

// This interface can be implemented by any basic op which any op that can be simply interpreted by taking the input values and returning the ouputs.
// Only control flow ops don't fit in that category.

// @XGENDEF:SIRInterface InterpretableComputeOp
// @+method {{ interpret(inputs: "&[&Attribute]") -> "Vec<Attribute>" }}
// @+method {{ fold_with_canonicalize() -> "bool" }}

// @XGENBEGIN
// Unique identifier for the InterpretableComputeOp Interface
const INTERPRETABLE_COMPUTE_OP_INTERFACE_UID: OpInterfaceUID =
    OpInterfaceUID::make_from_interface_identifier("InterpretableComputeOp");

// Interface Implementation for the InterpretableComputeOp interface.
pub trait InterpretableComputeOpInterfaceImpl {
    fn interpret<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        inputs: &[&Attribute],
    ) -> Vec<Attribute>;
    fn fold_with_canonicalize<'a>(&self, ctx: &'a IRContext, data: &'a OperationData) -> bool;
}

// Wrapper object holding an implementation of the InterpretableComputeOp interface.
pub struct InterpretableComputeOpInterfaceImplWrapper {
    dyn_impl: Box<dyn InterpretableComputeOpInterfaceImpl>,
}

impl InterpretableComputeOpInterfaceImplWrapper {
    pub fn new(dyn_impl: Box<dyn InterpretableComputeOpInterfaceImpl>) -> Self {
        Self { dyn_impl }
    }
}

impl OpInterfaceWrapper for InterpretableComputeOpInterfaceImplWrapper {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

//Definition of helper functions for the static methods of InterpretableComputeOp interface.
impl InterpretableComputeOpInterfaceImplWrapper {}

// Object to materialize an operation that implements the InterpretableComputeOp interface.
pub struct InterpretableComputeOp<'a> {
    wrapper: &'a InterpretableComputeOpInterfaceImplWrapper,
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl<'a> OpInterfaceObject<'a> for InterpretableComputeOp<'a> {
    fn get_interface_uid() -> OpInterfaceUID {
        INTERPRETABLE_COMPUTE_OP_INTERFACE_UID
    }

    fn make(
        wrapper: &'a dyn OpInterfaceWrapper,
        ctx: &'a IRContext,
        data: &'a OperationData,
    ) -> Self {
        let wrapper = wrapper
            .as_any()
            .downcast_ref::<InterpretableComputeOpInterfaceImplWrapper>()
            .unwrap();
        Self { ctx, data, wrapper }
    }
}

impl<'a> OperationImpl<'a> for InterpretableComputeOp<'a> {
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

// Methods implementation for InterpretableComputeOp.
impl<'a> InterpretableComputeOp<'a> {
    pub fn interpret(&self, inputs: &[&Attribute]) -> Vec<Attribute> {
        self.wrapper
            .dyn_impl
            .interpret(&self.ctx, &self.data, inputs)
    }
    pub fn fold_with_canonicalize(&self) -> bool {
        self.wrapper
            .dyn_impl
            .fold_with_canonicalize(&self.ctx, &self.data)
    }
}

// @XGENEND

// @XGENDEF:SIRInterface InterpretableOp
// @+method {{ interpret(interpreter: "&mut SIRInterpreter") }}

// @XGENBEGIN
// Unique identifier for the InterpretableOp Interface
const INTERPRETABLE_OP_INTERFACE_UID: OpInterfaceUID =
    OpInterfaceUID::make_from_interface_identifier("InterpretableOp");

// Interface Implementation for the InterpretableOp interface.
pub trait InterpretableOpInterfaceImpl {
    fn interpret<'a>(
        &self,
        ctx: &'a IRContext,
        data: &'a OperationData,
        interpreter: &mut SIRInterpreter,
    );
}

// Wrapper object holding an implementation of the InterpretableOp interface.
pub struct InterpretableOpInterfaceImplWrapper {
    dyn_impl: Box<dyn InterpretableOpInterfaceImpl>,
}

impl InterpretableOpInterfaceImplWrapper {
    pub fn new(dyn_impl: Box<dyn InterpretableOpInterfaceImpl>) -> Self {
        Self { dyn_impl }
    }
}

impl OpInterfaceWrapper for InterpretableOpInterfaceImplWrapper {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

//Definition of helper functions for the static methods of InterpretableOp interface.
impl InterpretableOpInterfaceImplWrapper {}

// Object to materialize an operation that implements the InterpretableOp interface.
pub struct InterpretableOp<'a> {
    wrapper: &'a InterpretableOpInterfaceImplWrapper,
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl<'a> OpInterfaceObject<'a> for InterpretableOp<'a> {
    fn get_interface_uid() -> OpInterfaceUID {
        INTERPRETABLE_OP_INTERFACE_UID
    }

    fn make(
        wrapper: &'a dyn OpInterfaceWrapper,
        ctx: &'a IRContext,
        data: &'a OperationData,
    ) -> Self {
        let wrapper = wrapper
            .as_any()
            .downcast_ref::<InterpretableOpInterfaceImplWrapper>()
            .unwrap();
        Self { ctx, data, wrapper }
    }
}

impl<'a> OperationImpl<'a> for InterpretableOp<'a> {
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

// Methods implementation for InterpretableOp.
impl<'a> InterpretableOp<'a> {
    pub fn interpret(&self, interpreter: &mut SIRInterpreter) {
        self.wrapper
            .dyn_impl
            .interpret(&self.ctx, &self.data, interpreter)
    }
}

// @XGENEND

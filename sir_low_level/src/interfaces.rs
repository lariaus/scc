/////////////////////////////////////////////////////////////////////////
// ConditionallyLowLevelOp Interface
/////////////////////////////////////////////////////////////////////////

// This interface can be implemented by any op which may or may not be a LowLevelOp.
// Most ops won't need this, except for some special / structured ops.

// @XGENDEF:SIRInterface ConditionallyLowLevelOp
// @+method {{ is_low_level() -> "bool" }}

use std::any::Any;

use sir_core::{
    ir_context::IRContext,
    ir_data::OperationData,
    op_interfaces::{OpInterfaceObject, OpInterfaceUID, OpInterfaceWrapper},
    operation::OperationImpl,
    operation_type::OperationTypeUID,
};

// @XGENBEGIN
// Unique identifier for the ConditionallyLowLevelOp Interface
const CONDITIONALLY_LOW_LEVEL_OP_INTERFACE_UID: OpInterfaceUID =
    OpInterfaceUID::make_from_interface_identifier("ConditionallyLowLevelOp");

// Interface Implementation for the ConditionallyLowLevelOp interface.
pub trait ConditionallyLowLevelOpInterfaceImpl {
    fn is_low_level<'a>(&self, ctx: &'a IRContext, data: &'a OperationData) -> bool;
}

// Wrapper object holding an implementation of the ConditionallyLowLevelOp interface.
pub struct ConditionallyLowLevelOpInterfaceImplWrapper {
    dyn_impl: Box<dyn ConditionallyLowLevelOpInterfaceImpl>,
}

impl ConditionallyLowLevelOpInterfaceImplWrapper {
    pub fn new(dyn_impl: Box<dyn ConditionallyLowLevelOpInterfaceImpl>) -> Self {
        Self { dyn_impl }
    }
}

impl OpInterfaceWrapper for ConditionallyLowLevelOpInterfaceImplWrapper {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

//Definition of helper functions for the static methods of ConditionallyLowLevelOp interface.
impl ConditionallyLowLevelOpInterfaceImplWrapper {}

// Object to materialize an operation that implements the ConditionallyLowLevelOp interface.
pub struct ConditionallyLowLevelOp<'a> {
    wrapper: &'a ConditionallyLowLevelOpInterfaceImplWrapper,
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl<'a> OpInterfaceObject<'a> for ConditionallyLowLevelOp<'a> {
    fn get_interface_uid() -> OpInterfaceUID {
        CONDITIONALLY_LOW_LEVEL_OP_INTERFACE_UID
    }

    fn make(
        wrapper: &'a dyn OpInterfaceWrapper,
        ctx: &'a IRContext,
        data: &'a OperationData,
    ) -> Self {
        let wrapper = wrapper
            .as_any()
            .downcast_ref::<ConditionallyLowLevelOpInterfaceImplWrapper>()
            .unwrap();
        Self { ctx, data, wrapper }
    }
}

impl<'a> OperationImpl<'a> for ConditionallyLowLevelOp<'a> {
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

// Methods implementation for ConditionallyLowLevelOp.
impl<'a> ConditionallyLowLevelOp<'a> {
    pub fn is_low_level(&self) -> bool {
        self.wrapper.dyn_impl.is_low_level(&self.ctx, &self.data)
    }
}

// @XGENEND

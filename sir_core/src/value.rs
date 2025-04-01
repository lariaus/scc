use iostreams::location::Location;

use crate::{
    ir_context::IRContext,
    ir_data::{ValueData, ValueID},
    types::Type,
};

// Extended Value object with the associated context
pub struct Value<'a> {
    ctx: &'a IRContext,
    data: &'a ValueData,
}

impl<'a> Value<'a> {
    pub(crate) fn make(ctx: &'a IRContext, data: &'a ValueData) -> Self {
        Self { ctx, data }
    }

    pub fn ctx(&self) -> &'a IRContext {
        self.ctx
    }

    pub fn data(&self) -> &'a ValueData {
        self.data
    }

    pub fn as_id(&self) -> ValueID {
        self.data.as_id()
    }

    // Returns true if the value is a block operand
    pub fn is_block_operand(&self) -> bool {
        match self.data {
            ValueData::BlockOperand(_) => true,
            ValueData::OperationOutput(_) => false,
        }
    }

    // Returns true if the value is an op output
    pub fn is_op_output(&self) -> bool {
        match self.data {
            ValueData::BlockOperand(_) => false,
            ValueData::OperationOutput(_) => true,
        }
    }

    // Returns the value location.
    pub fn loc(&self) -> Location {
        self.data.loc()
    }

    // Returns the value type.
    pub fn get_type(&self) -> &'a Type {
        self.data.get_type()
    }
}

impl<'a> From<Value<'a>> for ValueID {
    fn from(value: Value<'a>) -> Self {
        value.as_id()
    }
}

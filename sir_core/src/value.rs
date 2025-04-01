use diagnostics::diagnostics::LocatableObject;
use iostreams::location::Location;

use crate::{
    ir_context::IRContext,
    ir_data::{ValueData, ValueID},
    ir_printer::{IRPrintableObject, IRPrinter, IRPrinterOptions},
    operation::{GenericOperation, OperationImpl},
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

    // Returns the operation defining this value.
    // Or returns none for block arguments.
    pub fn defining_op(&self) -> Option<GenericOperation<'a>> {
        match self.data {
            ValueData::BlockOperand(_) => None,
            ValueData::OperationOutput(out) => Some(self.ctx.get_generic_operation(out.op())),
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

    // Returns the number of operations using this value.
    pub fn users_count(&self) -> usize {
        self.data.users().len()
    }

    // Returns an iterator of all user operations of the value
    pub fn users(&self) -> impl Iterator<Item = GenericOperation<'a>> {
        let ctx = self.ctx();
        self.data
            .users()
            .iter()
            .map(|uid| GenericOperation::make_from_data(ctx, ctx.get_operation_data(*uid)))
    }
}

impl<'a> From<Value<'a>> for ValueID {
    fn from(value: Value<'a>) -> Self {
        value.as_id()
    }
}

impl<'a> IRPrintableObject for Value<'a> {
    fn print(&self, printer: &mut crate::ir_printer::IRPrinter) -> Result<(), std::io::Error> {
        match self.data {
            ValueData::BlockOperand(operand) => {
                write!(printer.os(), "block operand #{}: ", operand.arg_idx())?;
                printer.print(operand.get_type())
            }
            ValueData::OperationOutput(out) => {
                let op = GenericOperation::make(&self.ctx, out.op());
                printer.print(&op)
            }
        }
    }
    fn initialize_context_on_root(&self, printer: &mut IRPrinter) {
        if let Some(op) = self.defining_op() {
            printer.initialize_context_with_root_op(op);
        } else {
            printer.initialize_context();
        }
    }
}

impl<'a> LocatableObject for Value<'a> {
    fn get_location(&self) -> Location {
        self.loc()
    }

    fn get_string_repr(&self) -> Option<String> {
        // Always print using the generic form.
        let mut opts = IRPrinterOptions::new();
        opts.use_generic_form = true;
        let mut printer = IRPrinter::new_string_builder(opts);
        printer.print_root(self).unwrap();
        Some(printer.take_output_string().unwrap())
    }
}

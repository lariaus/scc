use diagnostics::diagnostics::LocatableObject;
use iostreams::location::Location;
use parse::{lexer::TokenValue, parser::Parser};

use crate::{
    ir_context::IRContext,
    ir_data::{BlockData, BlockID, ValueID},
    ir_parser::{BlockParserState, IRParsableObject, IRParsableObjectWithContext},
    ir_printer::{IRPrintableObject, IRPrinter, IRPrinterOptions},
    operation::{GenericOperation, OperationImpl},
    types::Type,
    value::Value,
};

// Extended Block object with the associated context
#[derive(Clone, Copy)]
pub struct Block<'a> {
    ctx: &'a IRContext,
    data: &'a BlockData,
}

impl<'a> Block<'a> {
    pub(crate) fn make(ctx: &'a IRContext, data: &'a BlockData) -> Self {
        Self { ctx, data }
    }

    pub fn ctx(&self) -> &'a IRContext {
        self.ctx
    }

    pub fn data(&self) -> &'a BlockData {
        self.data
    }

    pub fn as_id(&self) -> BlockID {
        self.data.as_id()
    }

    // Returns the value location.
    pub fn loc(&self) -> Location {
        self.data.loc()
    }

    // Returns the number of operations in the block
    pub fn get_num_ops(&self) -> usize {
        self.data.ops().len()
    }

    /// Returns the last op of the block.
    /// Warning: it might not be a terminator op.
    /// Returns None if the block is empty
    pub fn get_terminator_op(&self) -> Option<GenericOperation<'a>> {
        let ctx = self.ctx();
        self.data
            .ops()
            .last()
            .map(|uid| GenericOperation::make_from_data(ctx, ctx.get_operation_data(*uid)))
    }

    // Returns an iterator of all operations in the block.
    pub fn get_ops(&self) -> impl DoubleEndedIterator<Item = GenericOperation<'a>> {
        let ctx = self.ctx();
        self.data
            .ops()
            .iter()
            .map(|uid| GenericOperation::make_from_data(ctx, ctx.get_operation_data(*uid)))
    }

    // Get the number of operands of the block.
    pub fn get_num_operands(&self) -> usize {
        self.data.args().len()
    }

    // Returns an iterator of the block operands.
    pub fn get_operands(&self) -> impl DoubleEndedIterator<Item = Value<'a>> {
        let ctx = self.ctx();
        self.data
            .args()
            .iter()
            .map(|uid| Value::make(ctx, ctx.get_value_data(*uid)))
    }

    pub fn get_operands_ids(&self) -> &[ValueID] {
        self.data.args()
    }

    // Returns the block operand #idx.
    pub fn get_operand(&self, idx: usize) -> Value<'a> {
        let args = self.data.args();
        assert!(idx < args.len());
        let ctx = self.ctx();
        Value::make(ctx, ctx.get_value_data(args[idx]))
    }

    // Returns an iterator of the block operands types.
    pub fn get_operands_types(&self) -> impl DoubleEndedIterator<Item = &'a Type> {
        self.get_operands().map(|val| val.get_type())
    }

    // Returns the parent op of the block, or none if it has no parents.
    pub fn parent(&self) -> Option<GenericOperation<'a>> {
        let op = self.data.parent()?;
        Some(self.ctx.get_generic_operation(op))
    }

    // Find the op before `op` in the block.
    // Panics if op not is block.
    // Returns None if op is the first op of the block.
    pub fn get_op_before(&self, op: GenericOperation<'a>) -> Option<GenericOperation<'a>> {
        let idx = self
            .data
            .ops()
            .iter()
            .position(|other| *other == op.as_id())
            .expect("Op not found");
        let idx = if idx == 0 {
            return None;
        } else {
            idx - 1
        };
        Some(self.ctx.get_generic_operation(self.data.ops()[idx]))
    }

    // Find the op after `op` in the block.
    // Panics if op not is block.
    // Returns None if op is the last op of the block.
    pub fn get_op_after(&self, op: GenericOperation<'a>) -> Option<GenericOperation<'a>> {
        let idx = self
            .data
            .ops()
            .iter()
            .position(|other| *other == op.as_id())
            .expect("Op not found");
        let idx = if idx + 1 == self.data.ops().len() {
            return None;
        } else {
            idx + 1
        };
        Some(self.ctx.get_generic_operation(self.data.ops()[idx]))
    }
}

impl<'a> From<Block<'a>> for BlockID {
    fn from(block: Block<'a>) -> Self {
        block.as_id()
    }
}

impl<'b> IRPrintableObject for Block<'b> {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        // Print the arguments.
        write!(printer.os(), "^(")?;
        for (idx, val) in self.get_operands().enumerate() {
            printer.assign_and_print_value_label(val.as_id(), true)?;
            write!(printer.os(), ": ")?;
            printer.print(val.get_type())?;
            if idx + 1 < self.get_num_operands() {
                write!(printer.os(), ", ")?;
            }
        }
        write!(printer.os(), ") ")?;

        // Print the body.
        write!(printer.os(), "{{")?;
        printer.inc_indent();
        for op in self.get_ops() {
            printer.newline()?;
            op.print(printer)?;
        }
        printer.end_printing_block();
        printer.nl_dec_indent()?;
        write!(printer.os(), "}}")
    }

    fn initialize_context_on_root(&self, printer: &mut IRPrinter) {
        printer.initialize_context_with_root_block(*self);
    }
}

impl IRParsableObjectWithContext for BlockID {
    fn parse_with_context(
        parser: &mut crate::ir_parser::IRParser,
        ctx: &mut IRContext,
    ) -> Option<Self> {
        let mut st = BlockParserState::new();
        // parse the args
        let loc_beg = parser.get_next_token_loc();
        let mut operands_names = vec![];
        let mut operands_types = vec![];
        parser.consume_sym_or_error(TokenValue::sym_xor())?;
        parser.consume_sym_or_error(TokenValue::sym_lparen())?;
        if !parser.next_token_is_sym(TokenValue::sym_rparen()) {
            loop {
                operands_names.push(
                    parser
                        .consume_identifier_or_error()?
                        .take_identifier()
                        .unwrap(),
                );
                parser.consume_sym_or_error(TokenValue::sym_colon())?;
                operands_types.push(Type::parse(parser)?);
                if !parser.try_consume_sym(TokenValue::sym_comma()) {
                    break;
                }
            }
        }
        parser.consume_sym_or_error(TokenValue::sym_rparen())?;
        st.set_operands_names(operands_names);
        st.set_operands_types(operands_types);

        // Parse the body
        let loc_end = parser
            .consume_sym_or_error(TokenValue::sym_lcbracket())?
            .loc();
        st.set_loc(Location::join(loc_beg, loc_end));
        parser.make_block_from_state(st, ctx, TokenValue::sym_rcbracket())
    }
}

impl<'a> LocatableObject for Block<'a> {
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

#[cfg(test)]
mod tests {
    use crate::{
        attributes::DictAttr, ir_data::ValueID, operation_type::OperationTypeRef,
        types::IntegerType,
    };

    use super::*;

    fn make_val(ctx: &mut IRContext, ty: Type) -> ValueID {
        let op = ctx._make_operation(
            Location::unknown_test_loc(),
            OperationTypeRef::Unknown("custom".to_owned()),
            vec![],
            vec![ty],
            DictAttr::empty(),
            vec![],
        );
        ctx.get_operation_data(op).outputs()[0]
    }

    #[test]
    fn test_block_multiple_ops() {
        let loc = Location::unknown_test_loc();
        let val_ty = IntegerType::new(32, None);
        let mut ctx = IRContext::new();
        let lhs = make_val(&mut ctx, val_ty.clone());
        let lhs_op = ctx.get_value_data(lhs).get_opdef().unwrap();
        let rhs = make_val(&mut ctx, val_ty.clone());
        let rhs_op = ctx.get_value_data(rhs).get_opdef().unwrap();

        // Build another op.
        let op = ctx._make_operation(
            loc,
            OperationTypeRef::Unknown("my_add".to_owned()),
            vec![lhs, rhs],
            vec![val_ty.clone()],
            DictAttr::empty(),
            vec![],
        );

        let block = ctx._make_block(loc, vec![], vec![lhs_op, rhs_op, op]);
        let block = ctx.get_block(block);

        // Check it
        assert_eq!(block.get_num_operands(), 0);
        assert_eq!(block.get_num_ops(), 3);
        assert_eq!(block.to_string_repr(), "^() {\n    %0 = \"custom\"() : () -> (i32)\n    %1 = \"custom\"() : () -> (i32)\n    %2 = \"my_add\"(%0, %1) : (i32, i32) -> (i32)\n}");
    }

    #[test]
    fn test_block_multiple_ops_parse() {
        let mut ctx = IRContext::new();

        // Parse block
        let block = BlockID::from_string_repr_with_context("^() {\n    %0 = \"custom\"() : () -> (i32)\n    %1 = \"custom\"() : () -> (i32)\n    %2 = \"my_add\"(%0, %1) : (i32, i32) -> (i32)\n}".to_string(), &mut ctx);
        let block = ctx.get_block(block);

        // Check it
        assert_eq!(block.get_num_operands(), 0);
        assert_eq!(block.get_num_ops(), 3);
        assert_eq!(block.to_string_repr(), "^() {\n    %0 = \"custom\"() : () -> (i32)\n    %1 = \"custom\"() : () -> (i32)\n    %2 = \"my_add\"(%0, %1) : (i32, i32) -> (i32)\n}");
    }
}

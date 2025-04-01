use crate::{
    ir_context::IRContext,
    ir_data::OperationID,
    ir_parser::{IRParser, OperationParserState},
    ir_printer::IRPrinter,
};

// Wrapper for the builtin interface.
pub trait BuiltinOpInterfaceWrapper {
    fn verify(&self, ctx: &IRContext, op: OperationID);

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

    // TODO: remove this. Hack to be able to implement parser.
    fn clone(&self) -> Box<dyn BuiltinOpInterfaceWrapper>;
}

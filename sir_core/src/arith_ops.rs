use crate::{
    ir_context::IRContext,
    ir_data::OperationData,
    op_interfaces::BuiltinOpInterfaceWrapper,
    operation::OperationImpl,
    operation_type::{OperationTypeBuilder, OperationTypeUID},
};

/////////////////////////////////////////////////////////////////////////
// ArithOpAdd implementation
/////////////////////////////////////////////////////////////////////////

pub struct ArithOpAdd<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

const ARITH_ADD_OPNAME: &'static str = "arith.add";
const ARITH_ADD_TYPE_UID: OperationTypeUID = OperationTypeUID::make_from_opname(ARITH_ADD_OPNAME);

impl<'a> OperationImpl<'a> for ArithOpAdd<'a> {
    fn make_from_data(ctx: &'a IRContext, data: &'a OperationData) -> Self {
        Self { ctx, data }
    }

    fn get_op_data(&self) -> &'a OperationData {
        self.data
    }

    fn get_context(&self) -> &'a IRContext {
        self.ctx
    }

    fn get_op_type_uid() -> crate::operation_type::OperationTypeUID {
        ARITH_ADD_TYPE_UID
    }

    fn verify(&self) {
        todo!("verify ArithAdd");
    }
}

// TODO: Find a way to get rid of this / generate this.
#[derive(Default)]
pub struct ArithOpBuiltinInterfaceWrapperImpl;
impl BuiltinOpInterfaceWrapper for ArithOpBuiltinInterfaceWrapperImpl {
    fn verify(&self, ctx: &IRContext, op: crate::ir_data::OperationID) {
        let op = ArithOpAdd::make_from_data(ctx, ctx.get_operation_data(op));
        op.verify()
    }

    fn custom_print(
        &self,
        _printer: &mut crate::ir_printer::IRPrinter,
        _ctx: &IRContext,
        _op: crate::ir_data::OperationID,
    ) -> Result<(), std::io::Error> {
        todo!()
    }

    fn custom_parse(
        &self,
        _parser: &mut crate::ir_parser::IRParser,
        _ctx: &mut IRContext,
        _st: &mut crate::ir_parser::OperationParserState,
    ) -> Option<()> {
        todo!()
    }

    fn clone(&self) -> Box<dyn BuiltinOpInterfaceWrapper> {
        Box::new(ArithOpBuiltinInterfaceWrapperImpl {})
    }
}

fn register_arith_op_add(ctx: &mut IRContext) {
    let mut infos = OperationTypeBuilder::new();
    infos.set_opname(ARITH_ADD_OPNAME);
    infos.set_builtin_interface::<ArithOpBuiltinInterfaceWrapperImpl>();
    ctx.register_operation(infos.build());
}

/////////////////////////////////////////////////////////////////////////
// ArithOps implementations
/////////////////////////////////////////////////////////////////////////

pub fn register_arith_ops(ctx: &mut IRContext) {
    register_arith_op_add(ctx);
}

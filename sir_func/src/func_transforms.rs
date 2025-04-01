use sir_core::pass_manager::PassRegistration;
use sir_core::{
    compiler_setup::CompilerSetup,
    ir_builder::InsertionPoint,
    ir_transforms::OpTransform,
    operation::OperationImpl,
    types::{FunctionType, Type},
};
use sir_low_level::legalize_pass::LegalizeToLowLevelPass;

use crate::func_ops::FunctionOp;

struct LegalizeFunctionOp;

impl OpTransform for LegalizeFunctionOp {
    fn can_transform_op(&self, op: sir_core::operation::GenericOperation) -> bool {
        op.isa::<FunctionOp>()
    }

    fn transform_op(
        &self,
        _diagnostics: &mut diagnostics::diagnostics::DiagnosticsEmitter,
        rewriter: &mut sir_core::ir_rewriter::IRRewriter,
        op: sir_core::ir_data::OperationID,
    ) -> diagnostics::diagnostics::ErrorOrSuccess {
        let func_op = rewriter.get_operation(op).cast::<FunctionOp>().unwrap();
        let loc = func_op.loc();
        let symbol_name = func_op.get_symbol_name().to_owned();
        let type_converter = rewriter.type_converter().unwrap();

        // Todo: avoid the clone.
        let function_type = func_op.get_function_type().clone();

        let new_function_type = match type_converter.convert_type(&Type::Function(function_type)) {
            Some(ty) => ty,
            None => return Err(()),
        };

        let old_block = func_op.get_body().as_id();

        // Create a new function.
        let new_func = rewriter
            .create_op(
                loc,
                FunctionOp::build(symbol_name, new_function_type.clone()),
            )
            .as_id();

        // Create a new block with the right arguments.
        let new_block = rewriter
            .create_block_at_end_of(
                new_func,
                loc,
                new_function_type
                    .cast::<FunctionType>()
                    .unwrap()
                    .arguments(),
            )
            .as_id();

        // Move all ops to the new block.
        rewriter.splice_block_at(old_block, InsertionPoint::AtEndOf(new_block), true);

        // Replace the op.
        rewriter.replace_op_with_op(op, new_func);
        Ok(())
    }
}

/// Register all transforms related to func operations.
pub fn register_func_transforms(cs: &mut CompilerSetup) {
    cs.register_extra_pass_transforms(LegalizeToLowLevelPass::get_pass_name(), |transforms| {
        transforms.add_transform(LegalizeFunctionOp);
    });
}

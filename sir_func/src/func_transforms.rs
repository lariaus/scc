use diagnostics::diagnostics::{emit_error, ErrorOrSuccess};
use sir_core::ir_builder::BlockArgumentReplacements;
use sir_core::ir_context::IRContext;
use sir_core::ir_printer::IRPrintableObject;
use sir_core::{
    ir_builder::InsertionPoint,
    operation::OperationImpl,
    types::{FunctionType, Type},
};
use sir_low_level::legalize_pass::LegalizeToLowLevelPass;
use sir_low_level::low_level_types::convert_attr;
use sir_transform::ir_rewriter::IRRewriter;
use sir_transform::ir_transforms::OpTransform;
use sir_transform::pass::PassRegistration;

use crate::func_ops::{FunctionOp, GenericConstantOp};

struct LegalizeFunctionOp;

impl OpTransform for LegalizeFunctionOp {
    fn can_transform_op(&self, op: sir_core::operation::GenericOperation) -> bool {
        op.isa::<FunctionOp>()
    }

    fn transform_op(
        &self,
        _diagnostics: &mut diagnostics::diagnostics::DiagnosticsEmitter,
        rewriter: &mut IRRewriter,
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
        rewriter.splice_block_at(
            old_block,
            InsertionPoint::AtEndOf(new_block),
            BlockArgumentReplacements::ReplaceWithNewBlockArguments,
        );

        // Replace the op.
        rewriter.replace_op_with_op(op, new_func);
        Ok(())
    }
}

struct LegalizeConstantOp;

impl OpTransform for LegalizeConstantOp {
    fn can_transform_op(&self, op: sir_core::operation::GenericOperation) -> bool {
        op.isa::<GenericConstantOp>()
    }

    fn transform_op(
        &self,
        diagnostics: &mut diagnostics::diagnostics::DiagnosticsEmitter,
        rewriter: &mut IRRewriter,
        op: sir_core::ir_data::OperationID,
    ) -> ErrorOrSuccess {
        let op = rewriter
            .get_operation(op)
            .cast::<GenericConstantOp>()
            .unwrap();
        let loc = op.loc();
        let val = op.get_value();
        let op = op.as_id();

        // Convert the value.
        let val = match convert_attr(val) {
            Some(val) => val,
            None => {
                emit_error(
                    diagnostics,
                    &loc,
                    format!("failed to convert attribute `{}`", val.to_string_repr()),
                );
                return Err(());
            }
        };

        let new_op = rewriter
            .create_op(loc, GenericConstantOp::build(val))
            .as_id();

        rewriter.replace_op_with_op(op, new_op);
        Ok(())
    }
}

/// Register all transforms related to func operations.
pub fn register_func_transforms(ctx: &mut IRContext) {
    sir_transform::context_registry::ContextRegistry::exec_register_fn(
        ctx,
        "__sir/transforms/register_func_transforms",
        |mut registry| {
            registry.register_extra_pass_transforms(
                LegalizeToLowLevelPass::get_pass_name(),
                |transforms| {
                    transforms.add_transform(LegalizeFunctionOp);
                    transforms.add_transform(LegalizeConstantOp);
                },
            );
        },
    );
}

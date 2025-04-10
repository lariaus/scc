use std::marker::PhantomData;

use diagnostics::diagnostics::{emit_error, DiagnosticsEmitter, ErrorOrSuccess};
use sir_core::{
    attributes::Attribute,
    ir_context::IRContext,
    ir_data::OperationID,
    op_interfaces::ConstantOp,
    operation::{GenericOperation, OperationImpl},
    value::Value,
};
use sir_lir::lir_ops::LIRIAddOp;
use sir_low_level::legalize_pass::LegalizeToLowLevelPass;
use sir_transform::pass::PassRegistration;
use sir_transform::{
    canonicalize_pass::CanonicalizePass, ir_rewriter::IRRewriter, ir_transforms::OpTransform,
};

use crate::math_ops::MathIAddOp;

fn is_const_zero(val: Value) -> bool {
    let op = match val.defining_op() {
        Some(op) => op,
        None => return false,
    };
    let const_op = match op.get_interface::<ConstantOp>() {
        Some(const_op) => const_op,
        None => return false,
    };

    let val = const_op.get_value();
    match val {
        Attribute::Float(v) => v.raw_val() == 0.,
        Attribute::Int(v) => v.raw_val() == 0,
        _ => false,
    }
}

struct CanonicalizeIAddOp;

impl OpTransform for CanonicalizeIAddOp {
    fn can_transform_op(&self, op: sir_core::operation::GenericOperation) -> bool {
        op.isa::<MathIAddOp>()
    }

    fn transform_op(
        &self,
        _diagnostics: &mut DiagnosticsEmitter,
        rewriter: &mut IRRewriter,
        op: OperationID,
    ) -> ErrorOrSuccess {
        let add_op = rewriter.get_operation(op).cast::<MathIAddOp>().unwrap();
        if is_const_zero(add_op.get_lhs()) {
            let new_output = add_op.get_rhs().as_id();
            rewriter.replace_op_with_values(op, &[new_output]);
            Ok(())
        } else if is_const_zero(add_op.get_rhs()) {
            let new_output = add_op.get_lhs().as_id();
            rewriter.replace_op_with_values(op, &[new_output]);
            Ok(())
        } else {
            Err(())
        }
    }
}

struct LegalizeElementwiseOps<SrcOp: OperationImpl<'static>, DstOp: OperationImpl<'static>> {
    phantom_src: PhantomData<SrcOp>,
    phantom_dst: PhantomData<DstOp>,
}

impl<SrcOp: OperationImpl<'static>, DstOp: OperationImpl<'static>>
    LegalizeElementwiseOps<SrcOp, DstOp>
{
    pub fn new() -> Self {
        Self {
            phantom_src: PhantomData,
            phantom_dst: PhantomData,
        }
    }
}

impl<SrcOp: OperationImpl<'static>, DstOp: OperationImpl<'static>> OpTransform
    for LegalizeElementwiseOps<SrcOp, DstOp>
{
    fn can_transform_op(&self, op: sir_core::operation::GenericOperation) -> bool {
        op.isa::<SrcOp>()
    }

    fn transform_op(
        &self,
        diagnostics: &mut DiagnosticsEmitter,
        rewriter: &mut IRRewriter,
        op: OperationID,
    ) -> ErrorOrSuccess {
        let op = rewriter.get_operation(op);
        let op_id = op.as_id();
        assert!(
            op.get_num_outputs() == 1,
            "elementwise op must have a single output"
        );

        // Let's convert the result type.
        let type_converter = rewriter.type_converter().unwrap();
        let result_ty = match type_converter.convert_type(op.get_output(0).get_type()) {
            Some(ty) => ty,
            None => {
                emit_error(diagnostics, &op, format!("Failed to convert result type"));
                return Err(());
            }
        };

        let new_op = rewriter
            .create_op(
                op.loc(),
                GenericOperation::build(
                    DstOp::get_op_type_uid(),
                    op.get_inputs_ids(),
                    &[result_ty],
                    op.get_attrs_dict().clone(),
                    &[],
                ),
            )
            .as_id();

        rewriter.replace_op_with_op(op_id, new_op);
        Ok(())
    }
}

/// Register all transforms related to math operations.
pub fn register_math_transforms(ctx: &mut IRContext) {
    sir_transform::context_registry::ContextRegistry::exec_register_fn(
        ctx,
        "__sir/transforms/register_math_transforms",
        |mut registry| {
            // Canonicalization patterns.
            registry.register_extra_pass_transforms(
                CanonicalizePass::get_pass_name(),
                |transforms| {
                    transforms.add_transform(CanonicalizeIAddOp);
                },
            );

            // Legalization (LowLevel) patterns.
            registry.register_extra_pass_transforms(
                LegalizeToLowLevelPass::get_pass_name(),
                |transforms| {
                    transforms
                        .add_transform(LegalizeElementwiseOps::<MathIAddOp, LIRIAddOp>::new());
                },
            );
        },
    );
}

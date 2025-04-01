use diagnostics::diagnostics::{DiagnosticsEmitter, ErrorOrSuccess};
use sir_core::{
    attributes::Attribute, canonicalize_pass::CanonicalizePass, compiler_setup::CompilerSetup,
    ir_data::OperationID, ir_rewriter::IRRewriter, ir_transforms::OpTransform,
    op_interfaces::ConstantOp, operation::OperationImpl, pass_manager::PassRegistration,
    value::Value,
};

use crate::math_ops::MathIAddOp;

struct CanonicalizeIAddOp;

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
        if is_const_zero(add_op.lhs()) {
            let new_output = add_op.rhs().as_id();
            rewriter.replace_op_with_values(op, &[new_output]);
            Ok(())
        } else if is_const_zero(add_op.rhs()) {
            let new_output = add_op.lhs().as_id();
            rewriter.replace_op_with_values(op, &[new_output]);
            Ok(())
        } else {
            Err(())
        }
    }
}

/// Register all transforms related to math operations.
pub fn register_math_transforms(cs: &mut CompilerSetup) {
    cs.register_extra_pass_transforms(CanonicalizePass::get_pass_name(), |transforms| {
        transforms.add_transform(CanonicalizeIAddOp);
    });
}

use crate::interfaces::InterpretableComputeOp;
use diagnostics::diagnostics::{DiagnosticsEmitter, ErrorOrSuccess};
use sir_core::{
    ir_context::IRContext,
    ir_data::{OperationID, ValueID},
    op_interfaces::match_constant,
    operation::{GenericOperation, OperationImpl},
};
use sir_transform::pass::PassRegistration;
use sir_transform::{
    canonicalize_pass::CanonicalizePass, ir_rewriter::IRRewriter, ir_transforms::OpTransform,
};

struct CanonicalizeInterpretableOp;

impl OpTransform for CanonicalizeInterpretableOp {
    fn can_transform_op(&self, op: GenericOperation) -> bool {
        op.has_interface::<InterpretableComputeOp>()
    }

    fn transform_op(
        &self,
        _diagnostics: &mut DiagnosticsEmitter,
        rewriter: &mut IRRewriter,
        op: OperationID,
    ) -> ErrorOrSuccess {
        let op = rewriter
            .get_operation(op)
            .get_interface::<InterpretableComputeOp>()
            .unwrap();
        let loc = op.loc();
        // Check we can canonicalize this op.
        if !op.fold_with_canonicalize() {
            return Err(());
        }

        // Get all the constant inputs
        let mut inputs = Vec::new();
        for val in op.get_inputs() {
            if let Some(cst) = match_constant(val) {
                inputs.push(cst);
            } else {
                return Err(());
            }
        }

        // Compute the outputs.
        let outputs = op.interpret(&inputs);
        assert_eq!(outputs.len(), op.get_num_outputs());
        let op = op.as_id();

        // Materialize all constants for the outputs.
        let outputs: Vec<ValueID> = outputs
            .into_iter()
            .map(|attr| {
                rewriter
                    .materialize_constant(loc, attr)
                    .get_output(0)
                    .as_id()
            })
            .collect();

        // Then replace them.
        rewriter.replace_op_with_values(op, &outputs);
        Ok(())
    }
}

pub fn register_interpreter_transforms(ctx: &mut IRContext) {
    sir_transform::context_registry::ContextRegistry::exec_register_fn(
        ctx,
        "__sir/transforms/register_interpreter_transforms",
        |mut registry| {
            // Canonicalization patterns.
            registry.register_extra_pass_transforms(
                CanonicalizePass::get_pass_name(),
                |transforms| {
                    transforms.add_transform(CanonicalizeInterpretableOp);
                },
            );
        },
    );
}

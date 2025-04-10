use diagnostics::diagnostics::{emit_error, ErrorOrSuccess};
use sir_core::operation::OperationImpl;
use sir_core::types::PointerType;
use sir_core::{ir_context::IRContext, ir_printer::IRPrintableObject};
use sir_lir::lir_ops::{LIRAllocaOp, LIRLoadOp, LIRStoreOp};
use sir_low_level::legalize_pass::LegalizeToLowLevelPass;
use sir_transform::{ir_rewriter::IRRewriter, ir_transforms::OpTransform, pass::PassRegistration};

use crate::mem_ops::{MemAllocaOp, MemLoadOp, MemStoreOp};

struct LegalizeMemLoadOp;

impl OpTransform for LegalizeMemLoadOp {
    fn can_transform_op(&self, op: sir_core::operation::GenericOperation) -> bool {
        op.isa::<MemLoadOp>()
    }

    fn transform_op(
        &self,
        _diagnostics: &mut diagnostics::diagnostics::DiagnosticsEmitter,
        rewriter: &mut IRRewriter,
        op: sir_core::ir_data::OperationID,
    ) -> ErrorOrSuccess {
        let op = rewriter.get_operation(op).cast::<MemLoadOp>().unwrap();
        let loc = op.loc();
        let ptr = op.get_ptr();
        let op = op.as_id();

        // The pointer value was already legalized, so we can simply use it to get the result type.
        let new_result_ty = ptr
            .get_type()
            .cast::<PointerType>()
            .unwrap()
            .element()
            .clone();

        let new_op = rewriter
            .create_op(loc, LIRLoadOp::build(ptr.as_id(), new_result_ty))
            .as_id();

        rewriter.replace_op_with_op(op, new_op);
        Ok(())
    }
}

struct LegalizeMemStoreOp;

impl OpTransform for LegalizeMemStoreOp {
    fn can_transform_op(&self, op: sir_core::operation::GenericOperation) -> bool {
        op.isa::<MemStoreOp>()
    }

    fn transform_op(
        &self,
        _diagnostics: &mut diagnostics::diagnostics::DiagnosticsEmitter,
        rewriter: &mut IRRewriter,
        op: sir_core::ir_data::OperationID,
    ) -> ErrorOrSuccess {
        let op = rewriter.get_operation(op).cast::<MemStoreOp>().unwrap();
        let loc = op.loc();
        let value = op.get_value().as_id();
        let ptr = op.get_ptr().as_id();
        let op = op.as_id();

        let new_op = rewriter
            .create_op(loc, LIRStoreOp::build(value, ptr))
            .as_id();

        rewriter.replace_op_with_op(op, new_op);
        Ok(())
    }
}

struct LegalizeMemAllocaOp;

impl OpTransform for LegalizeMemAllocaOp {
    fn can_transform_op(&self, op: sir_core::operation::GenericOperation) -> bool {
        op.isa::<MemAllocaOp>()
    }

    fn transform_op(
        &self,
        diagnostics: &mut diagnostics::diagnostics::DiagnosticsEmitter,
        rewriter: &mut IRRewriter,
        op: sir_core::ir_data::OperationID,
    ) -> ErrorOrSuccess {
        let op = rewriter.get_operation(op).cast::<MemAllocaOp>().unwrap();
        let loc = op.loc();
        let align = op.get_align_attr().clone();
        let result_ty = op.get_result().get_type();

        let new_result_ty = match rewriter.type_converter().unwrap().convert_type(result_ty) {
            Some(ty) => ty,
            None => {
                emit_error(
                    diagnostics,
                    &op.generic(),
                    format!(
                        "Unsupported result type for mem.alloca: `{}`",
                        result_ty.to_string_repr()
                    ),
                );
                return Err(());
            }
        };

        let op = op.as_id();

        let new_op = rewriter
            .create_op(loc, LIRAllocaOp::build(align, new_result_ty))
            .as_id();

        rewriter.replace_op_with_op(op, new_op);
        Ok(())
    }
}

/// Register all transforms related to mem operations.
pub fn register_mem_transforms(ctx: &mut IRContext) {
    sir_transform::context_registry::ContextRegistry::exec_register_fn(
        ctx,
        "__sir/transforms/register_mem_transforms",
        |mut registry| {
            registry.register_extra_pass_transforms(
                LegalizeToLowLevelPass::get_pass_name(),
                |transforms| {
                    transforms.add_transform(LegalizeMemLoadOp);
                    transforms.add_transform(LegalizeMemStoreOp);
                    transforms.add_transform(LegalizeMemAllocaOp);
                },
            );
        },
    );
}

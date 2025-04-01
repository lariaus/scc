use diagnostics::diagnostics::{DiagnosticsEmitter, ErrorOrSuccess};
use sir_core::{
    ir_transforms::{legalize_ir_with_type_converter, IRLegalizationVerifier, TransformsList},
    op_interfaces::ConstantOp,
    operation::{GenericOperation, OperationImpl},
    pass_manager::{Pass, PassRegistration},
    types::Type,
};

use crate::{interfaces::ConditionallyLowLevelOp, low_level_types, tags::TAG_LOW_LEVEL_OP};

// Pass to legalize any IR code to a low Level IR.
pub struct LegalizeToLowLevelPass {
    transforms: TransformsList,
    debug_mode: bool,
}

impl LegalizeToLowLevelPass {
    pub fn new() -> Self {
        Self {
            transforms: TransformsList::new(),
            debug_mode: false,
        }
    }
}

impl Pass for LegalizeToLowLevelPass {
    fn set_debug_mode(&mut self, debug_mode: bool) {
        self.debug_mode = debug_mode
    }

    fn run_on_operation(
        &self,
        diagnostics: &mut DiagnosticsEmitter,
        ctx: &mut sir_core::ir_context::IRContext,
        op: sir_core::ir_data::OperationID,
    ) -> ErrorOrSuccess {
        legalize_ir_with_type_converter(
            LowLevelLegalizer,
            low_level_types::LowLevelTypeConverter,
            ctx,
            diagnostics,
            &self.transforms,
            op,
            self.debug_mode,
        )
    }

    fn get_exported_transforms_list(&mut self) -> Option<&mut TransformsList> {
        Some(&mut self.transforms)
    }
}

impl PassRegistration for LegalizeToLowLevelPass {
    fn get_pass_name() -> &'static str {
        "legalize-to-low-level"
    }

    fn get_pass_description() -> &'static str {
        "Lower any IR to a valid Low Level IR form"
    }
}

struct LowLevelLegalizer;

// Check if op has a LowLevel tag / interface implementation to prove that it's legal
fn marked_as_legal(op: GenericOperation) -> bool {
    if op.has_tag(TAG_LOW_LEVEL_OP) {
        return true;
    }
    // Constant ops are also always marked as legal.
    if op.has_interface::<ConstantOp>() {
        return true;
    }

    // Check if it has the low level interface.
    if let Some(maybe_low_level) = op.get_interface::<ConditionallyLowLevelOp>() {
        return maybe_low_level.is_low_level();
    }

    // Any other ops is never legal.
    false
}

impl IRLegalizationVerifier for LowLevelLegalizer {
    fn is_legal_type(&self, ty: &Type) -> bool {
        low_level_types::is_valid_type(ty)
    }

    fn is_legal_op(&self, op: GenericOperation) -> bool {
        // Check if the op is marked legal.
        if !marked_as_legal(op) {
            return false;
        }

        // Then ensure all inputs / outputs types are legal.
        op.get_inputs_types().all(|ty| self.is_legal_type(ty))
            && op.get_outputs_types().all(|ty| self.is_legal_type(ty))

        // I don't think we need to check the blocks here ?
    }
}

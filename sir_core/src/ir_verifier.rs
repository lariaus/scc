use diagnostics::{
    diagnostics::{
        emit_error, CompilerDiagnostics, CompilerDiagnosticsEmitter, DiagnosticsEmitter,
    },
    result::CompilerResult,
};

use crate::{
    block::Block,
    ir_printer::ValueDefsMap,
    op_tags::TAG_DECLS_BLOCK_OP,
    operation::{GenericOperation, OperationImpl},
};

// Options for the IRVerifier
pub struct IRVerifierOptions {
    pub allow_unregistered_ops: bool,
}

impl IRVerifierOptions {
    // Create the default options.
    pub fn new() -> Self {
        Self {
            allow_unregistered_ops: false,
        }
    }
}

// Struct to verify if the IR structure is correct.
pub struct IRVerifier {
    diagnostics: CompilerDiagnostics,
    value_defs: ValueDefsMap,
    opts: IRVerifierOptions,
}

impl IRVerifier {
    // Create a new verifier
    pub fn new(opts: IRVerifierOptions) -> Self {
        Self {
            diagnostics: CompilerDiagnostics::new(),
            value_defs: ValueDefsMap::new(),
            opts,
        }
    }

    // Verify if an op is well formed. Return diagnostics if anything is wrong with it.
    pub fn verify(mut self, op: GenericOperation) -> CompilerResult<()> {
        // Prepare the values map first.
        self.value_defs.open_scope();
        self.value_defs.fill_with_predecessors_defs(op);

        // Then run the verifier.
        self._verify_op(op);

        CompilerResult::make(self.diagnostics, Some(()))
    }

    fn _call_verifier(&mut self, op: GenericOperation) {
        let mut diagnostics = DiagnosticsEmitter::new(&mut self.diagnostics, "IRVerifier");
        let interface = match op.get_op_builtin_interface() {
            Some(interface) => interface,
            None => {
                if !self.opts.allow_unregistered_ops {
                    emit_error(self, &op, format!("Unregistered operation not allowed"));
                }
                return;
            }
        };
        interface.verify(op.get_context(), op.as_id(), &mut diagnostics);
    }

    fn _verify_op(&mut self, op: GenericOperation) {
        // First, ensure all operands are defined.
        for (idx, input_val) in op.get_inputs().enumerate() {
            if !self.value_defs.contains_def(input_val.as_id()) {
                emit_error(
                    self,
                    &op,
                    format!(
                        "Value definition of operation input #{} doesn't dominate its use",
                        idx
                    ),
                )
                .attach_infos(&input_val, format!("Value definition is"));
            }
        }

        // Then visit all blocks.
        for block in op.get_blocks() {
            self._verify_block(block);
        }

        // Then define all the op results.
        for out_val in op.get_outputs() {
            self.value_defs.insert_op_out_def(out_val.as_id());
        }

        self._call_verifier(op);
    }

    fn _verify_block(&mut self, block: Block) {
        // Define the block operands.
        self.value_defs.open_scope();
        for operand_val in block.get_operands() {
            self.value_defs.insert_block_arg_def(operand_val.as_id());
        }

        let check_terminator = match block.parent() {
            Some(op) => !op.has_tag(TAG_DECLS_BLOCK_OP),
            None => false,
        };

        // Then check the ops.
        for (idx, op) in block.get_ops().enumerate() {
            let is_last = idx + 1 == block.get_num_ops();
            if is_last && check_terminator && !op.is_terminator() {
                emit_error(
                    self,
                    &op,
                    format!("Last op of a block must be a terminator"),
                );
            } else if !is_last && op.is_terminator() {
                emit_error(
                    self,
                    &op,
                    format!("Terminators must be the ultimate ops of a block"),
                );
            }
            self._verify_op(op);
        }

        self.value_defs.close_scope();
    }
}

impl CompilerDiagnosticsEmitter for IRVerifier {
    fn get_diagnostics_emitter_mut(&mut self) -> &mut CompilerDiagnostics {
        &mut self.diagnostics
    }

    fn get_diagnostics_emitter_name(&self) -> &str {
        "IR Verifier"
    }
}

// Verify `op` and returns diagnostics.
pub fn verify_op(op: GenericOperation) -> CompilerResult<()> {
    let verifier = IRVerifier::new(IRVerifierOptions::new());
    verifier.verify(op)
}

// Verify `op` and returns diagnostics.
pub fn verify_op_with_options(
    op: GenericOperation,
    options: IRVerifierOptions,
) -> CompilerResult<()> {
    let verifier = IRVerifier::new(options);
    verifier.verify(op)
}

// Helper functions to write op verifiers
pub mod ir_checks {
    use diagnostics::diagnostics::{emit_error, DiagnosticsEmitter};

    use crate::{
        attributes::{Attribute, AttributeSubClass},
        ir_printer::IRPrintableObject,
        operation::{GenericOperation, OperationImpl},
    };

    pub fn verif<S: Into<String>>(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        cond: bool,
        msg: S,
    ) {
        if !cond {
            emit_error(emitter, &op, msg.into());
        }
    }

    // Verify if op has the right number of inputs.
    pub fn verif_inputs_count(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        exp_ninputs: usize,
    ) {
        if op.get_num_inputs() != exp_ninputs {
            emit_error(
                emitter,
                &op,
                format!(
                    "Op has {} inputs, but {} ops must have {}",
                    op.get_num_inputs(),
                    op.opname(),
                    exp_ninputs
                ),
            );
        }
    }

    // Verify if op has the right number of outputs.
    pub fn verif_outputs_count(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        exp_noutputs: usize,
    ) {
        if op.get_num_outputs() != exp_noutputs {
            emit_error(
                emitter,
                &op,
                format!(
                    "Op has {} outputs, but {} ops must have {}",
                    op.get_num_outputs(),
                    op.opname(),
                    exp_noutputs
                ),
            );
        }
    }

    // Verify if op has the right number of blocks.
    pub fn verif_blocks_count(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        exp_nblocks: usize,
    ) {
        if op.get_num_blocks() != exp_nblocks {
            emit_error(
                emitter,
                &op,
                format!(
                    "Op has {} blocks, but {} ops must have {}",
                    op.get_num_blocks(),
                    op.opname(),
                    exp_nblocks
                ),
            );
        }
    }

    // Verify if the op has the same type for all inputs and outputs.
    pub fn verif_same_input_output_types(emitter: &mut DiagnosticsEmitter, op: GenericOperation) {
        let exp_ty = if op.get_num_inputs() > 0 {
            op.get_input(0).get_type()
        } else if op.get_num_outputs() > 0 {
            op.get_output(0).get_type()
        } else {
            return;
        };

        for in_ty in op.get_inputs_types() {
            if in_ty != exp_ty {
                emit_error(
                    emitter,
                    &op,
                    format!(
                        "All inputs and outputs of {} must have the same type",
                        op.opname()
                    ),
                );
                return;
            }
        }

        for out_ty in op.get_outputs_types() {
            if out_ty != exp_ty {
                emit_error(
                    emitter,
                    &op,
                    format!(
                        "All inputs and outputs of {} must have the same type",
                        op.opname()
                    ),
                );
                return;
            }
        }
    }

    // Verify if op has an attribute of type T
    pub fn verif_has_attr_of_type<T: AttributeSubClass>(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        name: &str,
    ) {
        let attr = match op.get_attr(name) {
            Some(attr) => attr,
            None => {
                emit_error(
                    emitter,
                    &op,
                    format!("Missing required attribute `{}`", name),
                );
                return;
            }
        };
        if !attr.isa::<T>() {
            emit_error(
                emitter,
                &op,
                format!(
                    "Required attribute `{}` must be of type `{}`, but got {}",
                    name,
                    T::get_typename(),
                    attr.to_string_repr()
                ),
            );
        }
    }

    // Verify if op has an attribute of type T
    pub fn verif_has_attr_as<Predicate: FnOnce(&Attribute) -> bool>(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        name: &str,
        predicate: Predicate,
        attr_label: &str,
    ) {
        let attr = match op.get_attr(name) {
            Some(attr) => attr,
            None => {
                emit_error(
                    emitter,
                    &op,
                    format!("Missing required attribute `{}`", name),
                );
                return;
            }
        };
        if !predicate(attr) {
            emit_error(
                emitter,
                &op,
                format!(
                    "Required attribute `{}` must be a {}, but got {}",
                    name,
                    attr_label,
                    attr.to_string_repr()
                ),
            );
        }
    }
}

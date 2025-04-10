use std::collections::HashMap;

use diagnostics::diagnostics::{emit_error, DiagnosticsEmitter, ErrorOrSuccess};
use sir_core::{
    attributes::{Attribute, RegisterAttr},
    ir_context::IRContext,
    ir_data::{BlockID, OperationID, ValueID},
    ir_printer::IRPrintableObject,
    op_tags::TAG_DECLS_BLOCK_OP,
    operation::OperationImpl,
};
use sir_transform::{
    dag_transformer::{dag_transformer_rewrite_block, DAGPattern, DAGPatternsList},
    ir_rewriter::{IRRewriter, IRRewriterOptions},
    ir_transforms::TransformsList,
    pass::{DynamicPassOptions, Pass, PassRegistration},
    sir_backend::SIRBackend,
};
use utils::bitmanip_utils::offset_align_p2;

use crate::backend_ops::BackendRegToValOp;

/// Helper class used during instruction selection to lower to backend IR.
pub struct ISelDAGHelper {
    regs_map: HashMap<ValueID, Attribute>,
    next_reg_idx: usize,
    stack_allocs_map: HashMap<ValueID, usize>,
    stack_offset: usize,
    dag_blocks: Vec<BlockID>,
}

impl ISelDAGHelper {
    pub fn new() -> Self {
        Self {
            regs_map: HashMap::new(),
            next_reg_idx: 0,
            dag_blocks: Vec::new(),
            stack_allocs_map: HashMap::new(),
            stack_offset: 0,
        }
    }

    /// Returns the register mapped to `val`.
    /// If val is not mapped, returns None.
    pub fn get_reg(&self, val: ValueID) -> Option<&Attribute> {
        self.regs_map.get(&val)
    }

    /// Returns true if `val` is mapped to a register.
    pub fn is_mapped(&self, val: ValueID) -> bool {
        self.regs_map.contains_key(&val)
    }

    /// Maps the value `val` to the register `reg`.
    pub fn map_reg(&mut self, val: ValueID, reg: Attribute) {
        let reg_ref = reg
            .cast::<RegisterAttr>()
            .expect("reg should be a register attr");
        if let Some(idx) = reg_ref.vreg_idx() {
            self.next_reg_idx = std::cmp::max(self.next_reg_idx, idx + 1);
        }
        if self.regs_map.insert(val, reg).is_some() {
            panic!("Value already mapped")
        }
    }

    pub fn get_or_map_virtual_reg(&mut self, val: ValueID, reg_name: &str) -> Attribute {
        match self.regs_map.entry(val) {
            std::collections::hash_map::Entry::Occupied(occupied_entry) => {
                occupied_entry.get().clone()
            }
            std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                let idx = self.next_reg_idx;
                self.next_reg_idx += 1;
                let reg = RegisterAttr::new_vreg(reg_name.to_owned(), idx);
                vacant_entry.insert(reg).clone()
            }
        }
    }

    /// Allocate a new unique virtual register attribute.
    pub fn alloc_new_virtual_reg(&mut self, name: String) -> Attribute {
        let idx = self.next_reg_idx;
        self.next_reg_idx += 1;
        RegisterAttr::new_vreg(name, idx)
    }

    /// Set the blocks of the op that needs to be rewritten using DAG Instruction selection.
    pub fn set_dag_blocks(&mut self, blocks: Vec<BlockID>) {
        self.dag_blocks = blocks;
    }

    /// Map `val` to a stack allocation.
    /// If `val` was already allocated, returns the previous allocation stack offset.
    /// Allocates `size` bytes of data in the stack.
    /// Returns an offset that is aligned by `alignment` bytes.
    pub fn get_or_alloc_stack_mem(&mut self, val: ValueID, alignment: usize, size: usize) -> usize {
        match self.stack_allocs_map.entry(val) {
            std::collections::hash_map::Entry::Occupied(occupied_entry) => {
                return *occupied_entry.get();
            }
            std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                // New val, allocate it.
                let new_offset = offset_align_p2(self.stack_offset, alignment);
                self.stack_offset = new_offset + size;
                *vacant_entry.insert(new_offset)
            }
        }
    }

    /// Returns the allocation stack offset for `val`.
    /// Returns none if `val` wasn't allocated.
    pub fn get_stack_mem_offset(&self, val: ValueID) -> Option<usize> {
        self.stack_allocs_map.get(&val).map(|x| *x)
    }

    /// Returns the offset for the current stack frame.
    pub fn get_stack_frame_offset(&self) -> usize {
        self.stack_offset
    }

    /// Manually set the offset for the current stack frame.
    pub fn set_stack_frame_offset(&mut self, offset: usize) {
        self.stack_offset = offset;
    }
}

pub struct InstructionSelectionOptions {
    pub patterns: DAGPatternsList<ISelDAGHelper>,
}

impl DynamicPassOptions for InstructionSelectionOptions {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

// Pass to execute common canonicalization transforms
pub struct InstructionSelectionPass {
    transforms: TransformsList,
    debug_mode: bool,
    opts: InstructionSelectionOptions,
}

impl InstructionSelectionPass {
    // Build the CanonicalizePass object.
    pub fn new() -> Self {
        // Start by adding a few special patterns;
        let mut patterns = DAGPatternsList::new();
        patterns.add(ConvertRegToValOp);

        Self {
            transforms: TransformsList::new(),
            debug_mode: false,
            opts: InstructionSelectionOptions { patterns },
        }
    }

    fn _transform_decls_list_op(
        &self,
        diagnostics: &mut DiagnosticsEmitter,
        rewriter: &mut IRRewriter,
        op: OperationID,
    ) -> ErrorOrSuccess {
        let block = rewriter.get_operation(op).get_block(0);
        let sub_ops: Vec<OperationID> = block.get_ops().map(|x| x.as_id()).collect();
        for op in sub_ops {
            self._transform_op(diagnostics, rewriter, op)?;
        }

        Ok(())
    }

    fn _transform_op(
        &self,
        diagnostics: &mut DiagnosticsEmitter,
        rewriter: &mut IRRewriter,
        op: OperationID,
    ) -> ErrorOrSuccess {
        if rewriter.get_operation(op).has_tag(TAG_DECLS_BLOCK_OP) {
            return self._transform_decls_list_op(diagnostics, rewriter, op);
        }

        if rewriter.debug_mode() {
            eprintln!(
                "InstructionSelection: before transform decl: {}",
                rewriter.get_operation(op).to_string_repr()
            );
        }

        let mut dag_helper = ISelDAGHelper::new();

        // Here we have a LIR declaration op (func / global / etc)
        // Step 1: Lower it to the backend ir.
        let changes =
            match self
                .opts
                .patterns
                .try_transform_op_with_changes(&mut dag_helper, rewriter, op)
            {
                Ok(changes) => changes,
                Err(()) => {
                    emit_error(
                        diagnostics,
                        &rewriter.get_operation(op),
                        format!("Failed to transform declaration op"),
                    );
                    return Err(());
                }
            };
        let op = changes
            .op_change()
            .unwrap()
            .get_replaced_op(op)
            .expect("Pattern should have replaced the op with another op");

        // Get the blocks on which to run DAG.
        let blocks: Vec<BlockID> = dag_helper.dag_blocks.drain(..).collect();
        if blocks.is_empty() {
            if rewriter.debug_mode() {
                eprintln!(
                    "InstructionSelection: rewrote declaration op without DAG blocks: {}",
                    rewriter.get_operation(op).to_string_repr()
                );
            }
            return Ok(());
        }

        if rewriter.debug_mode() {
            eprintln!(
                "InstructionSelection: Need to rewrite {} DAG blocks",
                blocks.len()
            );
            eprintln!(
                "InstructionSelection: before ISEL: {}",
                rewriter.get_operation(op).to_string_repr()
            );
        }

        // Now let's apply instruction selection on every block.
        for block in blocks {
            dag_transformer_rewrite_block(
                &mut dag_helper,
                diagnostics,
                rewriter,
                &self.opts.patterns,
                block,
            )?;
        }

        if rewriter.debug_mode() {
            eprintln!(
                "InstructionSelection: after ISEL: {}",
                rewriter.get_operation(op).to_string_repr()
            );
        }

        Ok(())
    }
}

impl Pass for InstructionSelectionPass {
    fn run_on_operation(
        &self,
        _backend: &SIRBackend,
        diagnostics: &mut DiagnosticsEmitter,
        ctx: &mut IRContext,
        op: OperationID,
    ) -> diagnostics::diagnostics::ErrorOrSuccess {
        let mut rewriter_opts = IRRewriterOptions::new();
        rewriter_opts.debug_mode = self.debug_mode;
        let mut rewriter = IRRewriter::new(ctx, rewriter_opts);
        self._transform_op(diagnostics, &mut rewriter, op)
    }

    fn set_debug_mode(&mut self, debug_mode: bool) {
        self.debug_mode = debug_mode;
    }

    fn get_exported_transforms_list(&mut self) -> Option<&mut TransformsList> {
        Some(&mut self.transforms)
    }

    fn get_dynamic_options(&mut self) -> Option<&mut dyn DynamicPassOptions> {
        Some(&mut self.opts)
    }
}

impl PassRegistration for InstructionSelectionPass {
    fn get_pass_name() -> &'static str {
        "instruction-selection"
    }

    fn get_pass_description() -> &'static str {
        "Run the instruction selection pass"
    }
}

// Special DAG Patterns.

struct ConvertRegToValOp;

impl DAGPattern<ISelDAGHelper> for ConvertRegToValOp {
    fn get_cost(&self) -> usize {
        0
    }

    fn match_op(
        &self,
        _helper: &ISelDAGHelper,
        op: sir_core::operation::GenericOperation,
    ) -> Option<Vec<sir_core::ir_data::OperationID>> {
        if op.isa::<BackendRegToValOp>() {
            Some(vec![op.as_id()])
        } else {
            None
        }
    }

    fn rewrite_op(
        &self,
        helper: &mut ISelDAGHelper,
        rewriter: &mut IRRewriter,
        op: sir_core::ir_data::OperationID,
    ) {
        // BackendRegToValOp is a special op.
        // It's only there to keep the IR legal.
        // It should already be assigned to a register.
        // This pattern only checks if the assignment is correct, but do nothing else.
        // (The op will automatically be deleted).
        let op = rewriter
            .get_operation(op)
            .cast::<BackendRegToValOp>()
            .unwrap();
        let assigned_reg = helper
            .get_reg(op.get_result().as_id())
            .expect("result value of RegToVal must already be assigned");

        assert_eq!(assigned_reg, op.get_reg_attr(), "incorrect reg assignment");
    }
}

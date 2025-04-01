use crate::{
    block::Block,
    ir_builder::IRBuilder,
    ir_context::IRContext,
    ir_data::{BlockID, OperationID, ValueID},
    ir_printer::IRPrintableObject,
    operation::{GenericOperation, OperationImpl},
};

// Options for the rewriter.
pub struct IRRewriterOptions {
    pub debug_mode: bool,
}

impl IRRewriterOptions {
    pub fn new() -> Self {
        Self { debug_mode: false }
    }
}

// Enum to indicate in what way an op was updated.
pub enum OpChange {
    Erased,
    Updated,
    ReplacedWithOp(OperationID),
    ReplacedWithVals(Vec<ValueID>),
}

// Struct to keep track of all the changes happened to the IR. (targetting one op)
pub struct IRRewriteChanges {
    created_ops: Vec<OperationID>,
    op_change: Option<OpChange>,
}

impl IRRewriteChanges {
    // Returns the list of created ops during the op change.
    pub fn created_ops(&self) -> &[OperationID] {
        &self.created_ops
    }

    // Returns the details of how the op was changed.
    pub fn op_change(&self) -> Option<&OpChange> {
        self.op_change.as_ref()
    }
}

/// Struct built on top of IRBuilder
/// Supports extra transformations needed when transforming the IR.
pub struct IRRewriter<'a> {
    builder: IRBuilder<'a>,
    opts: IRRewriterOptions,
    updating_op: Option<OperationID>,
    updating_changes: Option<IRRewriteChanges>,
}

impl<'a> IRRewriter<'a> {
    /// Build a new IRRewriter for `ctx`.
    pub fn new(ctx: &'a mut IRContext, opts: IRRewriterOptions) -> Self {
        Self {
            builder: IRBuilder::new(ctx),
            opts,
            updating_op: None,
            updating_changes: None,
        }
    }

    /// Returns true if the debug_mode is enabled.
    pub fn debug_mode(&mut self) -> bool {
        self.opts.debug_mode
    }

    /// Set the op which is currently being updated by the rewriter.
    /// This is needed to track / modify rewrites.
    pub fn set_updating_op(&mut self, op: Option<OperationID>) {
        if let Some(op) = op {
            self.updating_op = Some(op);
            self.updating_changes = Some(IRRewriteChanges {
                created_ops: Vec::new(),
                op_change: None,
            });
        } else {
            self.updating_op = None;
            self.updating_changes = None;
        }
    }

    /// Returns the changes applied to the IR, after calling set_updating_op.
    pub fn get_updating_change(&self) -> Option<&IRRewriteChanges> {
        self.updating_changes.as_ref()
    }

    /// Returns the associated builder.
    pub fn builder(&mut self) -> &mut IRBuilder<'a> {
        &mut self.builder
    }

    // Returns the associated context for the object
    pub fn ctx(&mut self) -> &mut IRContext {
        self.builder.ctx()
    }

    // Get the generic operation from the uid.
    pub fn get_operation(&self, uid: OperationID) -> GenericOperation {
        self.builder.get_operation(uid)
    }

    // Get the block from the uid.
    pub fn get_block(&self, uid: BlockID) -> Block {
        self.builder.get_block(uid)
    }

    /// Replace all outputs of op with `new_values`, and then erase `op`.
    /// It doesn't check anything about the validity of the values.
    pub fn replace_op_with_values(&mut self, op: OperationID, new_values: &[ValueID]) {
        // Check we can do the modification.
        if let Some(updating_op) = self.updating_op {
            assert!(updating_op == op, "You can only modify the current op");
        }

        // Get the outputs.
        let old_values: Vec<ValueID> = self
            .ctx()
            .get_generic_operation(op)
            .get_outputs()
            .map(|v| v.as_id())
            .collect();
        assert_eq!(
            old_values.len(),
            new_values.len(),
            "The number of new_values doesn't match the number of outputs of op"
        );

        if self.debug_mode() {
            if new_values.len() == 1 {
                eprintln!(
                    "IRRewriter: replace op `{}` with `{}`",
                    self.ctx().get_generic_operation(op).to_string_repr(),
                    self.ctx().get_value(new_values[0]).to_string_repr()
                );
            } else {
                eprintln!(
                    "IRRewriter: replace op {} with:",
                    self.ctx().get_generic_operation(op).to_string_repr()
                );
                for val in new_values {
                    eprintln!("  - {}", self.ctx().get_value(*val).to_string_repr());
                }
            }
        }

        // Replace the outputs.
        for (old_val, new_val) in old_values.iter().zip(new_values) {
            self.ctx().replace_all_uses_of_value(*old_val, *new_val);
        }

        // Erase the op.
        if self.debug_mode() {
            eprintln!(
                "IRRewriter: erase op `{}`",
                self.ctx().get_generic_operation(op).to_string_repr()
            );
        }
        self.ctx().erase_op(op);

        // Update the changes object.
        if let Some(changes) = &mut self.updating_changes {
            assert!(changes.op_change.is_none(), "op already remplaced");
            changes.op_change = Some(OpChange::ReplacedWithVals(new_values.to_owned()));
        }
    }

    /// Erase op from the IR.
    /// Doesn't check if it's still in use or not.
    pub fn erase_op(&mut self, op: OperationID) {
        // Check we can do the modification.
        if let Some(updating_op) = self.updating_op {
            assert!(updating_op == op, "You can only modify the current op");
        }

        // Erase the op.
        if self.debug_mode() {
            eprintln!(
                "IRRewriter: erase op `{}`",
                self.ctx().get_generic_operation(op).to_string_repr()
            );
        }
        self.ctx().erase_op(op);

        // Update the changes object.
        if let Some(changes) = &mut self.updating_changes {
            assert!(changes.op_change.is_none(), "op already remplaced");
            changes.op_change = Some(OpChange::Erased);
        }
    }
}

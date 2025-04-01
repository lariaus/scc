use diagnostics::diagnostics::{DiagnosticsEmitter, ErrorOrSuccess};

use crate::{
    ir_context::IRContext,
    ir_data::OperationID,
    ir_printer::IRPrintableObject,
    ir_rewriter::{IRRewriter, IRRewriterOptions, OpChange},
    operation::{GenericOperation, OperationImpl},
};

/// Perform a transformation on the IR.
pub trait OpTransform {
    // Returns the priority of the transform.
    // It will apply first the pattern with the highest priorities.
    fn priority(&self) -> usize {
        0
    }

    /// Returns true if the pattern can be applied to this op.
    fn can_transform_op(&self, op: GenericOperation) -> bool;

    /// Rewrite the operation.
    /// Returns success only if `op` was updated.
    /// Error might just mean the pattern wasn't applied.
    /// Actually, it's not really clear when to use error for hard errors or not.
    /// For now, only add to diagnostic for hard warnings / errors, not because transform doesn't match this op.
    fn transform_op(
        &self,
        diagnostics: &mut DiagnosticsEmitter,
        rewriter: &mut IRRewriter,
        op: OperationID,
    ) -> ErrorOrSuccess;
}

// A list of transforms to apply to an op.
pub struct TransformsList {
    transforms: Vec<Box<dyn OpTransform>>,
    sorted: bool,
}

impl TransformsList {
    pub fn new() -> Self {
        Self {
            transforms: Vec::new(),
            sorted: true,
        }
    }

    /// Add a new transform to the list.
    pub fn add_transform<T: OpTransform + 'static>(&mut self, transform: T) {
        self.transforms.push(Box::new(transform));
        self.sorted = false;
    }

    /// Since we usually only add all transforms before using them, sort them only before use.
    /// It's probably faster than trying to insert them in a sorted list.
    fn _sort_transforms(&mut self) {
        if !self.sorted {
            self.transforms
                .sort_by(|a, b| a.priority().cmp(&b.priority()));
        }
        self.sorted = true;
    }

    /// Apply all transforms to the op in order of priority.
    pub fn transform_op(
        &self,
        diagnostics: &mut DiagnosticsEmitter,
        rewriter: &mut IRRewriter,
        op: OperationID,
    ) -> ErrorOrSuccess {
        // We are going in reverse order here.
        // The list is also sorted in reverse.
        // That means in case of equal priority the last added transforms are executed first.
        // That helps in case other parts of the code "extend" passes with other patterns.

        for transform in self.transforms.iter().rev() {
            if !transform.can_transform_op(rewriter.get_operation(op)) {
                continue;
            }
            // Early return as soon as a pattern managed to transform the op.
            if transform.transform_op(diagnostics, rewriter, op).is_ok() {
                return Ok(());
            }
        }

        Err(())
    }
}

// Basic class to apply IRTranforms, going through every op in the IR only once.
struct SingleTimeIRTransformer<'a, 'b> {
    rewriter: IRRewriter<'a>,
    root: OperationID,
    transforms: &'b TransformsList,
    debug_mode: bool,
}

impl<'a, 'b> SingleTimeIRTransformer<'a, 'b> {
    fn new(
        ctx: &'a mut IRContext,
        transforms: &'b TransformsList,
        root: OperationID,
        debug_mode: bool,
    ) -> Self {
        let mut rewriter_opts = IRRewriterOptions::new();
        rewriter_opts.debug_mode = debug_mode;
        Self {
            rewriter: IRRewriter::new(ctx, rewriter_opts),
            root,
            transforms,
            debug_mode,
        }
    }

    // Run on the whole IR.
    fn run(&mut self, diagnostics: &mut DiagnosticsEmitter) {
        self._transform_op(diagnostics, self.root);
    }

    fn _apply_transform_on_op(&mut self, diagnostics: &mut DiagnosticsEmitter, op: OperationID) {
        if self.debug_mode {
            let generic_op = self.rewriter.get_operation(op);
            eprintln!(
                "SingleTimeIRTransformer: apply pattern on `{}`",
                generic_op.to_string_repr()
            );
        }

        // First prepare the rewriter.
        self.rewriter.set_updating_op(Some(op));

        // Try to transform the op.
        let res = self
            .transforms
            .transform_op(diagnostics, &mut self.rewriter, op);
        if self.debug_mode && res.is_ok() {
            eprintln!("Pattern application success !");
        } else if self.debug_mode && res.is_err() {
            eprintln!("No matching pattern found");
        }

        // Doesn't matter if it was a success or not for the single transformer.
        self.rewriter.set_updating_op(None);
    }

    fn _transform_op(&mut self, diagnostics: &mut DiagnosticsEmitter, op: OperationID) {
        // First: collect all children
        let mut children = Vec::new();
        for block in self.rewriter.get_operation(op).get_blocks() {
            for op in block.get_ops() {
                children.push(op.as_id());
            }
        }

        // Then visit them all.
        for op in children {
            self._transform_op(diagnostics, op);
        }

        // Finally visit the current op.
        self._apply_transform_on_op(diagnostics, op);
    }
}

/// Apply `transforms` in a single pass through the IR.
/// Modified ops won't be transformed again.
pub fn apply_single_pass_transforms(
    ctx: &mut IRContext,
    diagnostics: &mut DiagnosticsEmitter,
    transforms: &TransformsList,
    root: OperationID,
    debug_mode: bool,
) {
    let mut transformer = SingleTimeIRTransformer::new(ctx, transforms, root, debug_mode);
    transformer.run(diagnostics);
}

// Apply transforms greedily, chosing always the highest priority pattern for each op.
// Revisit every updated / created op until IR stop changes.
// @TODO[B3][SIR-CORE]: GreedyIRTRansformer applying transforms every time an op change might lead to infinite loop. We need a limit to stop this.
struct GreedyIRTRansformer<'a, 'b> {
    rewriter: IRRewriter<'a>,
    root: OperationID,
    transforms: &'b TransformsList,
    debug_mode: bool,
    worklist: Vec<OperationID>,
}

impl<'a, 'b> GreedyIRTRansformer<'a, 'b> {
    fn new(
        ctx: &'a mut IRContext,
        transforms: &'b TransformsList,
        root: OperationID,
        debug_mode: bool,
    ) -> Self {
        let mut rewriter_opts = IRRewriterOptions::new();
        rewriter_opts.debug_mode = debug_mode;
        Self {
            rewriter: IRRewriter::new(ctx, rewriter_opts),
            root,
            transforms,
            debug_mode,
            worklist: Vec::new(),
        }
    }

    // Run on the whole IR.
    fn run(&mut self, diagnostics: &mut DiagnosticsEmitter) {
        // Start by filling the worklist with all the ops in the IR.
        Self::_fill_worklist(&mut self.worklist, self.rewriter.ctx(), self.root);

        // Then visit all ops in the worklist until empty
        while !self.worklist.is_empty() {
            let op = self.worklist.pop().unwrap();
            self._apply_transform_on_op(diagnostics, op);
        }
    }

    fn _fill_worklist(worklist: &mut Vec<OperationID>, ctx: &IRContext, op: OperationID) {
        // We want to visit the children first so start by adding the op (LIFO).
        worklist.push(op);

        for block in ctx.get_generic_operation(op).get_blocks() {
            for op in block.get_ops() {
                Self::_fill_worklist(worklist, ctx, op.as_id());
            }
        }
    }

    fn _apply_transform_on_op(&mut self, diagnostics: &mut DiagnosticsEmitter, op: OperationID) {
        if self.debug_mode {
            let generic_op = self.rewriter.get_operation(op);
            eprintln!(
                "GreedyIRTRansformer: apply pattern on `{}`",
                generic_op.to_string_repr()
            );
        }

        // First prepare the rewriter.
        self.rewriter.set_updating_op(Some(op));

        // Try to transform the op.
        let res = self
            .transforms
            .transform_op(diagnostics, &mut self.rewriter, op);
        if self.debug_mode && res.is_ok() {
            eprintln!("Pattern application success !");
        } else if self.debug_mode && res.is_err() {
            eprintln!("No matching pattern found");
        }

        // Add changed ops to the worklist.
        if res.is_ok() {
            let changes = self.rewriter.get_updating_change().unwrap();
            for op in changes.created_ops() {
                self.worklist.push(*op);
            }

            if let Some(op_change) = changes.op_change() {
                match op_change {
                    OpChange::Erased => {}
                    OpChange::Updated => {
                        self.worklist.push(op);
                    }
                    OpChange::ReplacedWithOp(new_op) => {
                        if !changes.created_ops().contains(new_op) {
                            self.worklist.push(*new_op);
                        }
                    }
                    OpChange::ReplacedWithVals(_vals) => {
                        // Should we visit again the users of the new vals ?
                    }
                }
            }

            // Should we visit the users of the outputs of the ops ?
        }

        // Doesn't matter if it was a success or not for the single transformer.
        self.rewriter.set_updating_op(None);
    }
}

/// Apply `transforms` in a single pass through the IR.
/// Modified ops won't be transformed again.
pub fn apply_transforms_greedily(
    ctx: &mut IRContext,
    diagnostics: &mut DiagnosticsEmitter,
    transforms: &TransformsList,
    root: OperationID,
    debug_mode: bool,
) {
    let mut transformer = GreedyIRTRansformer::new(ctx, transforms, root, debug_mode);
    transformer.run(diagnostics);
}

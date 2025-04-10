use std::collections::HashSet;

use diagnostics::diagnostics::{emit_error, DiagnosticsEmitter, ErrorOrSuccess};
use sir_core::{
    ir_context::IRContext,
    ir_data::{BlockID, OperationID},
    ir_printer::IRPrintableObject,
    operation::{GenericOperation, OperationImpl},
};

use crate::ir_rewriter::{IRRewriteChanges, IRRewriter};

pub trait DAGPattern<T> {
    fn match_op(&self, helper: &T, op: GenericOperation) -> Option<Vec<OperationID>>;

    fn rewrite_op(&self, helper: &mut T, rewriter: &mut IRRewriter, op: OperationID);

    fn get_cost(&self) -> usize;
}

/// Object used to hold a list of patterns.
pub struct DAGPatternsList<T> {
    patterns: Vec<Box<dyn DAGPattern<T>>>,
}

impl<T> DAGPatternsList<T> {
    // Create an empty list of patterns.
    pub fn new() -> Self {
        Self {
            patterns: Vec::new(),
        }
    }

    // Add new patterns at the end of the list.
    pub fn extend(&mut self, mut new_patterns: Vec<Box<dyn DAGPattern<T>>>) {
        self.patterns.append(&mut new_patterns);
    }

    // Add a new pattern at the end of the list.
    pub fn add<Pat: DAGPattern<T> + 'static>(&mut self, pattern: Pat) {
        self.patterns.push(Box::new(pattern));
    }

    /// Apply the first matching pattern for the op, this is more for clasic patterns instead of DAG ones.
    pub fn apply_direct_pattern(
        &self,
        helper: &mut T,
        rewriter: &mut IRRewriter,
        op: OperationID,
    ) -> ErrorOrSuccess {
        // Go through all patterns.
        for pattern in self.patterns.iter().rev() {
            let matches = match pattern.match_op(helper, rewriter.get_operation(op)) {
                Some(matches) => matches,
                None => continue,
            };
            assert!(matches.len() == 1 && matches[0] == op, "invalid match");

            // Apply the pattern and return.
            pattern.rewrite_op(helper, rewriter, op);
            return Ok(());
        }

        Err(())
    }

    /// Try to transform an op with the rewriter, and return the changes.
    /// Wrapper around self.apply_direct_pattern
    pub fn try_transform_op_with_changes(
        &self,
        helper: &mut T,
        rewriter: &mut IRRewriter,
        op: OperationID,
    ) -> Result<IRRewriteChanges, ()> {
        if rewriter.debug_mode() {
            eprintln!("TransformsList: trying to apply pattern");
        }

        // First prepare the rewriter.
        rewriter.set_updating_op(Some(op));

        // Try to transform the op.
        let res = self.apply_direct_pattern(helper, rewriter, op);
        if rewriter.debug_mode() && res.is_ok() {
            eprintln!("TransformsList: Pattern application success !");
        } else if rewriter.debug_mode() && res.is_err() {
            eprintln!("TransformsList: no matching pattern found");
        }

        // Failed to apply patterns.
        if res.is_err() {
            return Err(());
        }

        // Return the changes.
        let changes = rewriter.take_updating_changes().unwrap();
        Ok(changes)
    }
}

struct DAGTransformer<'a, 'b, T> {
    helper: &'a mut T,
    patterns: &'b DAGPatternsList<T>,
    to_convert_ops: Vec<OperationID>,
    converted_ops: HashSet<OperationID>,
}

impl<'a, 'b, T> DAGTransformer<'a, 'b, T> {
    fn new(helper: &'a mut T, patterns: &'b DAGPatternsList<T>) -> Self {
        Self {
            helper,
            patterns,
            to_convert_ops: Vec::new(),
            converted_ops: HashSet::new(),
        }
    }

    // Returns the patterns index, or None if not found.
    fn select_pattern(&mut self, ctx: &IRContext) -> Option<(OperationID, usize)> {
        // First cleanup the list (the last pattern application might have left unused ops).
        self.to_convert_ops
            .retain(|op| !self.converted_ops.contains(op));

        // (op, pattern_idx, pattern_cost).
        let mut best: Option<(OperationID, usize, usize)> = None;

        // Now go through all ops to find the best pattern.
        for op in &self.to_convert_ops {
            let op = ctx.get_generic_operation(*op);
            for (idx, pattern) in self.patterns.patterns.iter().enumerate() {
                let cost = pattern.get_cost();
                // If the cost is higher there is no point to even check.
                if let Some(best) = best {
                    if cost >= best.2 {
                        break;
                    }
                }

                if pattern.match_op(&self.helper, op).is_some() {
                    // Match, rewrite the bet
                    best = Some((op.as_id(), idx, cost));
                }
            }
        }

        // Returns the best pattern found so far.
        let best = best?;
        Some((best.0, best.1))
    }

    fn transform_block(
        &mut self,
        diagnostics: &mut DiagnosticsEmitter,
        rewriter: &mut IRRewriter,
        block: BlockID,
    ) -> ErrorOrSuccess {
        // Start by filling the to_convert list
        self.converted_ops.clear();
        self.to_convert_ops = rewriter
            .get_block(block)
            .get_ops()
            .map(|op| op.as_id())
            .collect();

        // Clone the vec to keep the ops to remove in the right order
        let to_remove_ops = self.to_convert_ops.clone();

        // Loop until all ops are converted
        loop {
            let (op, pattern) = match self.select_pattern(rewriter.ctx()) {
                Some(best) => best,
                None => {
                    // If the to convert list is empty, it means we're done.
                    if self.to_convert_ops.is_empty() {
                        break;
                    }

                    // We can't find a pattern, emmit error on the first op.
                    let op = rewriter.get_operation(*self.to_convert_ops.first().unwrap());
                    emit_error(
                        diagnostics,
                        &op,
                        format!("DAGTransformer: no pattern match found to rewrite this op"),
                    );
                    return Err(());
                }
            };
            let pattern = &self.patterns.patterns[pattern];
            let replaced_ops = pattern
                .match_op(self.helper, rewriter.get_operation(op))
                .unwrap();
            assert!(replaced_ops.len() > 0, "We can't replace 0 ops");

            if rewriter.debug_mode() {
                eprintln!(
                    "Applying pattern to reply {} operation(s):",
                    replaced_ops.len()
                );
                for op in &replaced_ops {
                    eprintln!("{}", rewriter.get_operation(*op).to_string_repr());
                }
            }

            // First apply the pattern.
            pattern.rewrite_op(self.helper, rewriter, op);

            // Then mark the ops as replaced.
            for op in replaced_ops {
                self.converted_ops.insert(op);
            }
        }

        // The whole block is transformed.
        // Just remove the original ops.
        for op in to_remove_ops.into_iter().rev() {
            rewriter.erase_op(op);
        }

        Ok(())
    }
}

/// Apply a DAG transform algorithm to rewrite the block `block`.
pub fn dag_transformer_rewrite_block<T>(
    helper: &mut T,
    diagnostics: &mut DiagnosticsEmitter,
    rewriter: &mut IRRewriter,
    patterns: &DAGPatternsList<T>,
    block: BlockID,
) -> ErrorOrSuccess {
    let mut transformer = DAGTransformer::new(helper, patterns);
    transformer.transform_block(diagnostics, rewriter, block)
}

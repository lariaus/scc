use std::{
    collections::HashMap,
    hash::{DefaultHasher, Hash, Hasher},
    marker::PhantomData,
};

use diagnostics::diagnostics::{DiagnosticsEmitter, ErrorOrSuccess};

use crate::{
    ir_context::IRContext,
    ir_data::{BlockID, OperationID},
    ir_printer::IRPrintableObject,
    ir_rewriter::{IRRewriter, IRRewriterOptions},
    op_tags::TAG_PURE_OP,
    operation::{GenericOperation, OperationImpl},
    pass_manager::{Pass, PassRegistration},
};

/// Implementation of a Common Subexpression Eliminitation Pass.
/// This is a very basic implementation that requires lots of improvements.
pub struct CSEPass {
    debug_mode: bool,
}

// Hash all the op information.
fn hash_operation(op: GenericOperation) -> u64 {
    let mut h = DefaultHasher::new();

    // Hash the optype.
    op.get_op_type_id().unwrap().hash(&mut h);

    // Hash the inputs.
    for input in op.get_inputs() {
        input.as_id().hash(&mut h);
    }

    // Hash the attrs.
    op.get_attrs_dict().hash(&mut h);

    // Hash the result types.
    for ty in op.get_outputs_types() {
        ty.hash(&mut h);
    }

    // We don't support blocks yet.
    assert!(op.get_num_blocks() == 0);

    h.finish()
}

// Returns true if two ops are considered "equal".
fn check_operation_eq(lhs: GenericOperation, rhs: GenericOperation) -> bool {
    // Check the optype.
    if lhs.get_op_type_id() != rhs.get_op_type_id() {
        return false;
    }

    // Check the inputs.
    if lhs.get_num_inputs() != rhs.get_num_inputs()
        || !lhs
            .get_inputs()
            .zip(rhs.get_inputs())
            .all(|(v0, v1)| v0.as_id() == v1.as_id())
    {
        return false;
    }

    // Check the attributes.
    if lhs.get_attrs_dict() != rhs.get_attrs_dict() {
        return false;
    }

    // Check the outputs.
    if lhs.get_num_outputs() != rhs.get_num_outputs()
        || !lhs
            .get_outputs_types()
            .zip(rhs.get_outputs_types())
            .all(|(v0, v1)| v0 == v1)
    {
        return false;
    }

    // We don't support blocks yet.
    assert!(lhs.get_num_blocks() == 0 && rhs.get_num_blocks() == 0);

    true
}

// Special HashSet<Operation> class.
// We can't rely on HashSet because we would need to store the operation, binding the lifetime.
// Here we only store OperationID in the map, but the API takes GenericOperation operands.
pub struct OperationHashSet<HashFn, EqualFn>
where
    HashFn: Fn(GenericOperation) -> u64,
    EqualFn: Fn(GenericOperation, GenericOperation) -> bool,
{
    hash_fn: HashFn,
    equal_fn: EqualFn,
    vals: HashMap<u64, Vec<OperationID>>,
}

impl<HashFn, EqualFn> OperationHashSet<HashFn, EqualFn>
where
    HashFn: Fn(GenericOperation) -> u64,
    EqualFn: Fn(GenericOperation, GenericOperation) -> bool,
{
    pub fn new(hash_fn: HashFn, equal_fn: EqualFn) -> Self {
        Self {
            hash_fn,
            equal_fn,
            vals: HashMap::new(),
        }
    }

    // Try to find the value in set the set.
    // Returns none if it wasn't found
    pub fn find<'a>(&self, op: GenericOperation<'a>) -> Option<GenericOperation<'a>> {
        let entry = self.vals.get(&(self.hash_fn)(op))?;
        let res = entry.iter().find(|id| {
            let other_op = op.get_context().get_generic_operation(**id);
            (self.equal_fn)(op, other_op)
        })?;
        Some(op.get_context().get_generic_operation(*res))
    }

    // Returns true if `op` is in the set.
    pub fn contains(&self, op: GenericOperation) -> bool {
        self.find(op).is_some()
    }

    // Try to insert `op` into the map.
    // Return None if op wasn't found in the map already.
    // If it was, it's updated, and the old value is returned.
    pub fn insert<'a>(&mut self, op: GenericOperation<'a>) -> Option<GenericOperation<'a>> {
        let h = (self.hash_fn)(op);
        let entry = match self.vals.get_mut(&(self.hash_fn)(op)) {
            Some(op) => op,
            None => {
                self.vals.insert(h, vec![op.as_id()]);
                return None;
            }
        };

        let old_op = entry.iter_mut().find(|id| {
            let other_op = op.get_context().get_generic_operation(**id);
            (self.equal_fn)(op, other_op)
        });

        if let Some(old_op) = old_op {
            let res = *old_op;
            *old_op = op.as_id();
            Some(op.get_context().get_generic_operation(res))
        } else {
            entry.push(op.as_id());
            None
        }
    }
}

fn can_be_cse(op: GenericOperation) -> bool {
    // Don't support ops with blocks for now
    if op.get_num_blocks() > 0 {
        return false;
    }

    // Only cse pure ops.
    op.has_tag(TAG_PURE_OP)
}

impl CSEPass {
    // Create an instance of the pass
    pub fn new() -> Self {
        Self { debug_mode: false }
    }

    fn _run_cse(
        &self,
        diagnostics: &mut DiagnosticsEmitter,
        rewriter: &mut IRRewriter,
        root: OperationID,
    ) {
        // First collect all blocks in the IR.
        let mut blocks = Vec::new();
        self._collect_blocks_rec(&mut blocks, rewriter.get_operation(root));

        // Then apply the algorithm on the block.
        for block in blocks {
            self._run_cse_on_block(diagnostics, rewriter, block);
        }
    }

    fn _collect_blocks_rec(&self, blocks: &mut Vec<BlockID>, op: GenericOperation) {
        for block in op.get_blocks() {
            blocks.push(block.as_id());

            for children in block.get_ops() {
                self._collect_blocks_rec(blocks, children);
            }
        }
    }

    // We apply the algorithm block by block.
    fn _run_cse_on_block(
        &self,
        _diagnostics: &mut DiagnosticsEmitter,
        rewriter: &mut IRRewriter,
        block_id: BlockID,
    ) {
        let mut ops_map = OperationHashSet::new(hash_operation, check_operation_eq);
        let mut ops_to_erase = Vec::new();
        let mut ops_to_replace = Vec::new();

        let block = rewriter.get_block(block_id);
        if self.debug_mode {
            eprintln!("Block before CSE:\n{}", block.to_string_repr());
        }

        for op in block.get_ops() {
            if !can_be_cse(op) {
                continue;
            }
            if op.get_num_outputs() == 0 || op.get_outputs().all(|v| v.users_count() == 0) {
                // The ops has no users, just erase it
                if self.debug_mode {
                    eprintln!(
                        "CSE: Found operation without users: {}",
                        op.to_string_repr()
                    );
                }
                ops_to_erase.push(op.as_id());
                continue;
            }

            if let Some(same_op) = ops_map.find(op) {
                assert!(same_op.as_id() != op.as_id());
                // We find a match, mark it for replacement.
                if self.debug_mode {
                    eprintln!(
                        "CSE: Found operation `{}` can that be replaced with `{}`",
                        op.to_string_repr(),
                        same_op.to_string_repr()
                    );
                    ops_to_replace.push((op.as_id(), same_op.as_id()));
                }
            } else {
                // Just insert it in the map.
                ops_map.insert(op);
            }
        }

        // Now apply all the transforms
        for op in ops_to_erase {
            // rewriter.
        }

        let block = rewriter.get_block(block_id);
        if self.debug_mode {
            eprintln!("Block after CSE:\n{}", block.to_string_repr());
        }
    }
}

impl Pass for CSEPass {
    fn set_debug_mode(&mut self, debug_mode: bool) {
        self.debug_mode = debug_mode;
    }

    fn run_on_operation(
        &self,
        diagnostics: &mut DiagnosticsEmitter,
        ctx: &mut IRContext,
        op: OperationID,
    ) -> ErrorOrSuccess {
        let mut rewriter = IRRewriter::new(
            ctx,
            IRRewriterOptions {
                debug_mode: self.debug_mode,
            },
        );
        self._run_cse(diagnostics, &mut rewriter, op);
        Ok(())
    }
}

impl PassRegistration for CSEPass {
    fn get_pass_name() -> &'static str {
        "cse"
    }

    fn get_pass_description() -> &'static str {
        "Apply Common Subexpression Elimination"
    }
}

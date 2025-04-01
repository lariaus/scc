use std::collections::HashMap;

use iostreams::location::Location;

use crate::{
    attributes::DictAttr,
    block::Block,
    ir_data::{
        BlockData, BlockID, BlockOperand, OperationData, OperationID, OperationOutput, ValueData,
        ValueID,
    },
    operation::{GenericOperation, OperationImpl},
    operation_type::{OperationTypeInfos, OperationTypeRef, OperationTypeUID},
    types::Type,
    value::Value,
};

// Full context for the current IR.
// You can see the program graph as huge inter-connected graph.
// And this object is the direct way to manipulate it in rust without breaking the ownership rules.
pub struct IRContext {
    // All raw objects data.
    _data_vals: HashMap<ValueID, ValueData>,
    _data_val_next: usize,
    _data_blocks: HashMap<BlockID, BlockData>,
    _data_block_next: usize,
    _data_ops: HashMap<OperationID, OperationData>,
    _data_op_next: usize,
    _data_ops_types: HashMap<OperationTypeUID, OperationTypeInfos>,
    _opname_to_uid_map: HashMap<&'static str, OperationTypeUID>,
}

impl IRContext {
    pub fn new() -> Self {
        Self {
            _data_vals: HashMap::new(),
            _data_val_next: 0,
            _data_blocks: HashMap::new(),
            _data_block_next: 0,
            _data_ops: HashMap::new(),
            _data_op_next: 0,
            _data_ops_types: HashMap::new(),
            _opname_to_uid_map: HashMap::new(),
        }
    }

    pub(crate) fn _make_block_operand(
        &mut self,
        block: BlockID,
        arg_idx: usize,
        ty: Type,
        loc: Location,
    ) -> ValueID {
        let uid = ValueID(self._data_val_next);
        self._data_val_next += 1;
        let value =
            ValueData::BlockOperand(BlockOperand::new(uid, block, arg_idx, ty, loc, vec![]));
        self._data_vals.insert(uid, value);
        uid
    }

    pub(crate) fn _make_operation_output(
        &mut self,
        op: OperationID,
        output_idx: usize,
        ty: Type,
        loc: Location,
    ) -> ValueID {
        let uid = ValueID(self._data_val_next);
        self._data_val_next += 1;
        let value =
            ValueData::OperationOutput(OperationOutput::new(uid, op, output_idx, ty, loc, vec![]));
        self._data_vals.insert(uid, value);
        uid
    }

    pub(crate) fn _make_block(
        &mut self,
        loc: Location,
        args_types: Vec<Type>,
        ops: Vec<OperationID>,
    ) -> BlockID {
        let uid = BlockID(self._data_block_next);
        self._data_block_next += 1;
        let args: Vec<_> = args_types
            .into_iter()
            .enumerate()
            .map(|(idx, ty)| self._make_block_operand(uid, idx, ty, loc))
            .collect();

        self._data_block_next += 1;
        let block = BlockData::new(uid, loc, args, ops);
        self._data_blocks.insert(uid, block);
        uid
    }

    pub(crate) fn _make_operation(
        &mut self,
        loc: Location,
        op_type: OperationTypeRef,
        inputs: Vec<ValueID>,
        outputs_types: Vec<Type>,
        attrs: DictAttr,
        blocks: Vec<BlockID>,
    ) -> OperationID {
        let uid = OperationID(self._data_op_next);
        self._data_op_next += 1;
        let outputs: Vec<_> = outputs_types
            .into_iter()
            .enumerate()
            .map(|(idx, ty)| self._make_operation_output(uid, idx, ty, loc))
            .collect();

        // Set the parent.
        for block in &blocks {
            let block = self.get_block_data_mut(*block);
            assert!(block.parent().is_none());
            block.set_parent(Some(uid));
        }

        // Create the op.
        let op = OperationData::new(uid, loc, op_type, inputs, outputs, attrs, blocks);

        // Update the users of the operands.
        for val in op.inputs() {
            let val = self._data_vals.get_mut(&val).unwrap();
            val.add_user(uid);
        }

        self._data_ops.insert(uid, op);
        uid
    }

    // Get a ValueData ref from a ValueID.
    pub fn get_value_data(&self, uid: ValueID) -> &ValueData {
        match self._data_vals.get(&uid) {
            Some(val) => val,
            None => panic!("Invalid value id {:?}", uid),
        }
    }

    // Get a Value ref from a ValueID.
    pub fn get_value<'a>(&'a self, uid: ValueID) -> Value<'a> {
        let val_data = self.get_value_data(uid);
        Value::make(self, val_data)
    }

    // Get a BlockData ref from a BlockID.
    pub fn get_block_data(&self, uid: BlockID) -> &BlockData {
        match self._data_blocks.get(&uid) {
            Some(block) => block,
            None => panic!("Invalid block id {:?}", uid),
        }
    }

    // Get a Block ref from a BlockID.
    pub fn get_block<'a>(&'a self, uid: BlockID) -> Block<'a> {
        let block_data = self.get_block_data(uid);
        Block::make(self, block_data)
    }

    // Get an operation data ref from an OperationID.
    pub fn get_operation_data(&self, uid: OperationID) -> &OperationData {
        match self._data_ops.get(&uid) {
            Some(op) => op,
            None => panic!("Invalid operation id {:?}", uid),
        }
    }

    // Get a GenericOperation ref from an OperationID.
    pub fn get_generic_operation<'a>(&'a self, uid: OperationID) -> GenericOperation<'a> {
        let op_data = self.get_operation_data(uid);
        GenericOperation::make_from_data(self, op_data)
    }

    // Register an operation inside the Context.
    pub fn register_operation(&mut self, infos: OperationTypeInfos) {
        let uid = infos.uid();
        let opname = infos.opname();
        assert!(
            !self._data_ops_types.contains_key(&uid),
            "OperationType uid {:?} already registered",
            uid
        );
        assert!(
            !self._opname_to_uid_map.contains_key(&opname),
            "Operation `{:?}` already registered",
            uid
        );
        self._opname_to_uid_map.insert(opname, uid);
        self._data_ops_types.insert(uid, infos);
    }

    // Get the registered OperationTypeInfos from its uid.
    pub fn get_op_type_infos(&self, uid: OperationTypeUID) -> &OperationTypeInfos {
        match self._data_ops_types.get(&uid) {
            Some(infos) => infos,
            None => panic!("Invalid OperationType uid {:?}", uid),
        }
    }

    // Find the registered OperationTypeInfos from its opname.
    // Returns None if no such op was registered.
    pub fn find_op_type_infos_from_opname(&self, opname: &str) -> Option<&OperationTypeInfos> {
        let uid = self._opname_to_uid_map.get(opname)?;
        Some(self.get_op_type_infos(*uid))
    }

    /////////////////////////////////////////////////////
    // Helper methods to manipulate the IR
    /////////////////////////////////////////////////////

    // Get a ValueData ref from a ValueID.
    pub(crate) fn get_value_data_mut(&mut self, uid: ValueID) -> &mut ValueData {
        match self._data_vals.get_mut(&uid) {
            Some(val) => val,
            None => panic!("Invalid value id {:?}", uid),
        }
    }

    // Get a BlockData ref from a BlockID.
    pub(crate) fn get_block_data_mut(&mut self, uid: BlockID) -> &mut BlockData {
        match self._data_blocks.get_mut(&uid) {
            Some(block) => block,
            None => panic!("Invalid block id {:?}", uid),
        }
    }

    // Get an OperationData ref from an OperationID.
    pub(crate) fn get_op_data_mut(&mut self, uid: OperationID) -> &mut OperationData {
        match self._data_ops.get_mut(&uid) {
            Some(op) => op,
            None => panic!("Invalid op id {:?}", uid),
        }
    }

    // Detach the op from the IR, but without erasing the op from memory.
    pub(crate) fn detach_op_from_ir(&mut self, op: OperationID) {
        let parent = match self.get_operation_data(op).parent() {
            Some(parent) => parent,
            None => return,
        };

        // Find and remove the op from the parent.
        let parent_ops = self.get_block_data_mut(parent).ops_mut();
        let op_idx = parent_ops.iter().position(|other| *other == op).unwrap();
        parent_ops.remove(op_idx);

        // And clear the parent field.
        self.get_op_data_mut(op).set_parent(None);
    }

    // Detach the block from the IR, but without erasing the block from memory.
    pub(crate) fn detach_block_from_ir(&mut self, block: BlockID) {
        let parent = match self.get_block_data_mut(block).parent() {
            Some(parent) => parent,
            None => return,
        };

        // Find and remove the op from the parent.
        let parent_blocks = self.get_op_data_mut(parent).blocks_mut();
        let block_idx = parent_blocks
            .iter()
            .position(|other| *other == block)
            .unwrap();
        parent_blocks.remove(block_idx);

        // And clear the parent field.
        self.get_block_data_mut(block).set_parent(None);
    }

    // Move `op` before `pos` in its block.
    // Should only be called from the IRBuilder.
    pub(crate) fn move_op_before_in_block(&mut self, pos: OperationID, op: OperationID) {
        self.detach_op_from_ir(op);

        // Find the parent of `pos`
        let parent = match self.get_operation_data(pos).parent() {
            Some(parent) => parent,
            None => panic!("move_op_before_in_block failure: `pos` doesn't have a parent"),
        };

        // Insert the op in the list.
        let parent_ops = self.get_block_data_mut(parent).ops_mut();
        let pos_idx = parent_ops.iter().position(|other| *other == pos).unwrap();
        parent_ops.insert(pos_idx, op);

        // And set the parent field.
        self.get_op_data_mut(op).set_parent(Some(parent));
    }

    // Move `op` after `pos` in its block.
    // Should only be called from the IRBuilder.
    pub(crate) fn move_op_after_in_block(&mut self, pos: OperationID, op: OperationID) {
        self.detach_op_from_ir(op);

        // Find the parent of `pos`
        let parent = match self.get_operation_data(pos).parent() {
            Some(parent) => parent,
            None => panic!("move_op_before_in_block failure: `pos` doesn't have a parent"),
        };

        // Insert the op in the list.
        let parent_ops = self.get_block_data_mut(parent).ops_mut();
        let pos_idx = parent_ops.iter().position(|other| *other == pos).unwrap();
        parent_ops.insert(pos_idx + 1, op);

        // And set the parent field.
        self.get_op_data_mut(op).set_parent(Some(parent));
    }

    // Move `op` at the beginning of `block``.
    pub(crate) fn move_op_at_begin_of_block(&mut self, block: BlockID, op: OperationID) {
        self.detach_op_from_ir(op);

        // Add the op to the list.
        let block_ops = self.get_block_data_mut(block).ops_mut();
        block_ops.insert(0, op);

        // And set the parent field.
        self.get_op_data_mut(op).set_parent(Some(block));
    }

    // Move `op` at the end of `block``.
    pub(crate) fn move_op_at_end_of_block(&mut self, block: BlockID, op: OperationID) {
        self.detach_op_from_ir(op);

        // Add the op to the list.
        let block_ops = self.get_block_data_mut(block).ops_mut();
        block_ops.push(op);

        // And set the parent field.
        self.get_op_data_mut(op).set_parent(Some(block));
    }

    // Move `block` at the beginning of `new_parent`.
    pub(crate) fn move_block_at_begin_of_op_children(
        &mut self,
        block: BlockID,
        new_parent: OperationID,
    ) {
        self.detach_block_from_ir(block);

        // Insert the block in the list
        let parent_blocks = self.get_op_data_mut(new_parent).blocks_mut();
        parent_blocks.insert(0, block);

        // And set the parent field.
        self.get_block_data_mut(block).set_parent(Some(new_parent));
    }

    // Move `block` at the end of `new_parent`.
    pub(crate) fn move_block_at_end_of_op_children(
        &mut self,
        block: BlockID,
        new_parent: OperationID,
    ) {
        self.detach_block_from_ir(block);

        // Insert the block in the list.
        let parent_blocks = self.get_op_data_mut(new_parent).blocks_mut();
        parent_blocks.push(block);

        // And set the parent field.
        self.get_block_data_mut(block).set_parent(Some(new_parent));
    }

    // Replace of uses of `old_val` in the IR by `new_val`
    pub(crate) fn replace_all_uses_of_value(&mut self, old_val: ValueID, new_val: ValueID) {
        // Nothing to to if the value is the same.
        if old_val == new_val {
            return;
        }

        // Update all old_users ops with new_val.
        let old_users: Vec<OperationID> = self
            .get_value_data(old_val)
            .users()
            .iter()
            .map(|x| *x)
            .collect();
        for user in &old_users {
            let op = self.get_op_data_mut(*user);
            for i in 0..op.inputs().len() {
                if op.get_input(i) == old_val {
                    op.set_input(i, new_val);
                }
            }
        }

        // Clear the users of old_val.
        self.get_value_data_mut(old_val).clear_users();

        // Add old_users to the users of new_val.
        let new_val = self.get_value_data_mut(new_val);
        for user in &old_users {
            new_val.add_user(*user);
        }
    }

    fn _erase_empty_op(&mut self, op: OperationID) {
        let op_data = self.get_operation_data(op);
        assert!(op_data.parent().is_none(), "parent must be none");
        for out_val in op_data.outputs() {
            assert!(
                self.get_value_data(*out_val).users().is_empty(),
                "op must not have any use"
            );
        }

        assert!(op_data.blocks().is_empty(), "op must not have any block");

        // Remove from the users of the operands
        let operands: Vec<ValueID> = op_data.inputs().iter().map(|v| *v).collect();
        for val in &operands {
            let val = self.get_value_data_mut(*val);
            val.erase_user(op);
        }

        // Then finally delete the op
        self._data_ops.remove(&op);
    }

    // Completely erase the op from the IR and the context.
    // op must not have any uses.
    pub(crate) fn erase_op(&mut self, op: OperationID) {
        self.detach_op_from_ir(op);

        // TODO: handle recursively deleting blocks.

        self._erase_empty_op(op);
    }
}

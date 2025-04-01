use std::marker::PhantomData;

use iostreams::location::Location;

use crate::{
    attributes::{Attribute, DictAttr, StringAttr},
    block::Block,
    ir_context::IRContext,
    ir_data::{BlockID, OperationID, ValueID},
    operation::{GenericOperation, OperationImpl},
    operation_type::{OperationTypeRef, OperationTypeUID},
    types::Type,
};

// Insertion point to indicate where to insert the next created op.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InsertionPoint {
    Detached, // Don't insert anywhere, just keep the op detached
    BeforeOp(OperationID),
    AfterOp(OperationID),
    AtBeginOf(BlockID),
    AtEndOf(BlockID),
}

// Helper class to manipulate and build the IR.
pub struct IRBuilder<'a> {
    ctx: &'a mut IRContext,
    ip: InsertionPoint,
}

impl<'a> IRBuilder<'a> {
    pub fn new(ctx: &'a mut IRContext) -> Self {
        Self {
            ctx,
            ip: InsertionPoint::Detached,
        }
    }

    // Returns the associated context for the object
    pub fn ctx(&mut self) -> &mut IRContext {
        self.ctx
    }

    // Returns the current insertion point.
    pub fn insertion_point(&self) -> InsertionPoint {
        self.ip
    }

    // Set the insertion point of the builder.
    pub fn set_insertion_point(&mut self, ip: InsertionPoint) {
        self.ip = ip;
    }

    // Create a new op from the builder state (but don't insert it yet)
    fn _create_op_from_builder<'x, T: OperationImpl<'x>>(
        &mut self,
        loc: Location,
        b: OpBuilderState<'x, T>,
    ) -> OperationID {
        // Get the op ref.
        let op_type_ref = if let Some(op_type_uid) = b.op_type_uid {
            OperationTypeRef::Registered(op_type_uid)
        } else if let Some(opname) = b.unregistered_opname {
            OperationTypeRef::Unknown(opname)
        } else {
            panic!("Missing opname value")
        };

        let inputs = b.inputs.unwrap_or_default();
        let outputs_types = b.outputs_types.unwrap_or_default();
        let attrs_dict = if let Some(attrs_vals) = b.attrs_vals {
            DictAttr::new(attrs_vals)
        } else {
            b.attrs_dict.unwrap_or(DictAttr::empty())
        };
        let blocks = b.blocks.unwrap_or_default();

        self.ctx
            ._make_operation(loc, op_type_ref, inputs, outputs_types, attrs_dict, blocks)
    }

    // Create a new empty block (but don't insert it yet)
    fn _create_block(&mut self, loc: Location, operands_types: Vec<Type>) -> BlockID {
        self.ctx._make_block(loc, operands_types, vec![])
    }

    // Move the op at `pos`
    fn _move_op_at(&mut self, op: OperationID, pos: InsertionPoint) {
        match pos {
            InsertionPoint::Detached => self.ctx.detach_op_from_ir(op),
            InsertionPoint::BeforeOp(pos) => self.ctx.move_op_before_in_block(pos, op),
            InsertionPoint::AfterOp(pos) => self.ctx.move_op_after_in_block(pos, op),
            InsertionPoint::AtBeginOf(block) => self.ctx.move_op_at_begin_of_block(block, op),
            InsertionPoint::AtEndOf(block) => self.ctx.move_op_at_end_of_block(block, op),
        }
    }

    // Create an operation without inserting it.
    pub fn create_detached_op<'b, T: OperationImpl<'b>>(
        &'b mut self,
        loc: Location,
        b: OpBuilderState<'b, T>,
    ) -> T {
        let op = self._create_op_from_builder(loc, b);
        T::make_from_data(self.ctx, self.ctx.get_operation_data(op))
    }

    // Create an operation and insert it before `pos`.
    pub fn create_op_before<'b, T: OperationImpl<'b>>(
        &'b mut self,
        pos: OperationID,
        loc: Location,
        b: OpBuilderState<'b, T>,
    ) -> T {
        let op = self._create_op_from_builder(loc, b);
        self.ctx.move_op_before_in_block(pos, op);
        T::make_from_data(self.ctx, self.ctx.get_operation_data(op))
    }

    // Create an operation and insert it after `pos`.
    pub fn create_op_after<'b, T: OperationImpl<'b>>(
        &'b mut self,
        pos: OperationID,
        loc: Location,
        b: OpBuilderState<'b, T>,
    ) -> T {
        let op = self._create_op_from_builder(loc, b);
        self.ctx.move_op_after_in_block(pos, op);
        T::make_from_data(self.ctx, self.ctx.get_operation_data(op))
    }

    // Create an operation and insert it at the beginning of `block`.
    pub fn create_op_at_begin<'b, T: OperationImpl<'b>>(
        &'b mut self,
        block: BlockID,
        loc: Location,
        b: OpBuilderState<'b, T>,
    ) -> T {
        let op = self._create_op_from_builder(loc, b);
        self.ctx.move_op_at_begin_of_block(block, op);
        T::make_from_data(self.ctx, self.ctx.get_operation_data(op))
    }

    // Create an operation and insert it at the end of `block`.
    pub fn create_op_at_end<'b, T: OperationImpl<'b>>(
        &'b mut self,
        block: BlockID,
        loc: Location,
        b: OpBuilderState<'b, T>,
    ) -> T {
        let op = self._create_op_from_builder(loc, b);
        self.ctx.move_op_at_end_of_block(block, op);
        T::make_from_data(self.ctx, self.ctx.get_operation_data(op))
    }

    // Create an operation and insert it at `pos`
    pub fn create_op_at<'b, T: OperationImpl<'b>>(
        &'b mut self,
        pos: InsertionPoint,
        loc: Location,
        b: OpBuilderState<'b, T>,
    ) -> T {
        let op = self._create_op_from_builder(loc, b);
        self._move_op_at(op, pos);
        T::make_from_data(self.ctx, self.ctx.get_operation_data(op))
    }

    // Create an operation and insert it at the current insertion point
    pub fn create_op<'b, T: OperationImpl<'b>>(
        &'b mut self,
        loc: Location,
        b: OpBuilderState<'b, T>,
    ) -> T {
        self.create_op_at(self.ip, loc, b)
    }

    // Create a detached block.
    pub fn create_block<T: Into<Vec<Type>>>(&mut self, loc: Location, operands_types: T) -> Block {
        let block = self._create_block(loc, operands_types.into());
        Block::make(&self.ctx, self.ctx.get_block_data(block))
    }

    // Create a block right at the beginning of the blocks list of `pos`.
    pub fn create_block_at_begin_of<T: Into<Vec<Type>>>(
        &mut self,
        pos: OperationID,
        loc: Location,
        operands_types: T,
    ) -> Block {
        let block = self._create_block(loc, operands_types.into());
        self.ctx.move_block_at_begin_of_op_children(block, pos);
        Block::make(&self.ctx, self.ctx.get_block_data(block))
    }

    // Create a block right at the end of the blocks list of `pos`.
    pub fn create_block_at_end_of<T: Into<Vec<Type>>>(
        &mut self,
        pos: OperationID,
        loc: Location,
        operands_types: T,
    ) -> Block {
        let block = self._create_block(loc, operands_types.into());
        self.ctx.move_block_at_end_of_op_children(block, pos);
        Block::make(&self.ctx, self.ctx.get_block_data(block))
    }

    // Splice the content of block at `pos`, leaving `block` empty
    pub fn splice_block_at(&mut self, block: BlockID, pos: InsertionPoint, replace_args: bool) {
        match pos {
            InsertionPoint::Detached => panic!("Invalid position"),
            InsertionPoint::BeforeOp(_pos) => todo!(),
            InsertionPoint::AfterOp(_pos) => todo!(),
            InsertionPoint::AtBeginOf(_pos) => todo!(),
            InsertionPoint::AtEndOf(pos) => {
                self.ctx
                    ._splice_block_content_at_end_of_block(block, pos, replace_args)
            }
        }
    }

    // Get the generic operation from the uid.
    pub fn get_operation(&self, uid: OperationID) -> GenericOperation {
        self.ctx.get_generic_operation(uid)
    }

    // Get the block from the uid.
    pub fn get_block(&self, uid: BlockID) -> Block {
        self.ctx.get_block(uid)
    }
}

// Helper class used to build a new operation.
pub struct OpBuilderState<'a, T: OperationImpl<'a>> {
    _phantom: PhantomData<&'a T>,
    op_type_uid: Option<OperationTypeUID>,
    unregistered_opname: Option<String>,
    inputs: Option<Vec<ValueID>>,
    outputs_types: Option<Vec<Type>>,
    attrs_dict: Option<DictAttr>,
    attrs_vals: Option<Vec<(Attribute, Attribute)>>,
    blocks: Option<Vec<BlockID>>,
}

impl<'a, T: OperationImpl<'a>> OpBuilderState<'a, T> {
    fn new() -> Self {
        Self {
            _phantom: PhantomData,
            op_type_uid: None,
            unregistered_opname: None,
            inputs: None,
            outputs_types: None,
            attrs_dict: None,
            attrs_vals: None,
            blocks: None,
        }
    }

    // Create a builder for an operation of type `T`.
    pub fn make() -> Self {
        let mut res = Self::new();
        res.set_op_type_uid(T::get_op_type_uid());
        res
    }

    // Create a builder for an operation of type `uid`.
    pub fn make_with_type_uid(uid: OperationTypeUID) -> Self {
        let mut res = Self::new();
        res.set_op_type_uid(uid);
        res
    }

    // Set the opname of the operation (for unregistered op)
    fn set_op_type_uid(&mut self, op_type_uid: OperationTypeUID) {
        assert!(self.op_type_uid.is_none());
        assert!(self.unregistered_opname.is_none());
        self.op_type_uid = Some(op_type_uid);
    }

    // Set the opname of the operation (for unregistered op)
    fn set_unregistered_opname<V: Into<String>>(&mut self, unregistered_opname: V) {
        assert!(self.op_type_uid.is_none());
        assert!(self.unregistered_opname.is_none());
        self.unregistered_opname = Some(unregistered_opname.into());
    }

    // Set the inputs of the operation.
    pub fn set_inputs<V: Into<Vec<ValueID>>>(&mut self, inputs: V) {
        assert!(self.inputs.is_none());
        self.inputs = Some(inputs.into());
    }

    // Set the outputs types of the operation.
    pub fn set_outputs_types<V: Into<Vec<Type>>>(&mut self, outputs_types: V) {
        assert!(self.outputs_types.is_none());
        self.outputs_types = Some(outputs_types.into());
    }

    // Set the attrs dict
    pub fn set_attrs_dict<V: Into<DictAttr>>(&mut self, attrs_dict: V) {
        assert!(self.attrs_dict.is_none());
        assert!(self.attrs_vals.is_none());
        self.attrs_dict = Some(attrs_dict.into());
    }

    // Set an attr value.
    pub fn set_attr<K: Into<String>, V: Into<Attribute>>(&mut self, key: K, val: V) {
        assert!(self.attrs_dict.is_none());
        if self.attrs_vals.is_none() {
            self.attrs_vals = Some(Vec::new());
        }
        self.attrs_vals
            .as_mut()
            .unwrap()
            .push((StringAttr::new(key.into()), val.into()));
    }

    // Set the blocks of the operation.
    pub fn set_blocks<V: Into<Vec<BlockID>>>(&mut self, blocks: V) {
        assert!(self.blocks.is_none());
        self.blocks = Some(blocks.into());
    }
}

impl<'a> OpBuilderState<'a, GenericOperation<'a>> {
    // Create a builder for an unregistered op.
    pub fn make_unregistered<V: Into<String>>(opname: V) -> Self {
        let mut res = Self::new();
        res.set_unregistered_opname(opname);
        res
    }
}

// Alias to define a clear interface for the op builders.
pub type OpImplBuilderState<T> = OpBuilderState<'static, T>;

#[cfg(test)]
mod tests {

    use crate::{ir_printer::IRPrintableObject, operation::OperationImpl, types::IntegerType};

    use super::*;

    // Create `custom() -> ty``
    fn make_custom(b: &mut IRBuilder, ty: Type) -> ValueID {
        let mut st = OpBuilderState::make_unregistered("custom");
        st.set_outputs_types(vec![ty]);
        b.create_op(Location::unknown_test_loc(), st).into()
    }

    // Try to return value directly.
    // Doesn't work because Value is bind to IRBuilder, and we can't reuse it anymore.
    // fn make_custom<'a>(b: &'a mut IRBuilder, ty: Type) -> Value<'a> {
    //     let mut st = OpBuilderState::make_unregistered("custom");
    //     st.set_outputs_types(vec![ty]);
    //     b.create_op(Location::unknown_test_loc(), st).into()
    // }

    // Create `mya_dd(x, y) -> type``
    fn make_my_add(b: &mut IRBuilder, lhs: ValueID, rhs: ValueID, out_type: Type) -> ValueID {
        let mut st = OpBuilderState::make_unregistered("my_add");
        st.set_inputs(vec![lhs, rhs]);
        st.set_outputs_types(vec![out_type]);
        b.create_op(Location::unknown_test_loc(), st).into()
    }

    // Create `my_fun() -> ()
    fn make_my_fun(b: &mut IRBuilder) -> OperationID {
        let st = OpBuilderState::make_unregistered("my_fun");
        b.create_op(Location::unknown_test_loc(), st).into()
    }

    #[test]
    fn test_make_val() {
        let val_ty = IntegerType::new(32, None);
        let mut ctx = IRContext::new();
        let mut b = IRBuilder::new(&mut ctx);

        let val = make_custom(&mut b, val_ty.clone());
        let op = ctx.get_value_data(val).get_opdef().unwrap();
        let op = ctx.get_generic_operation(op);
        assert_eq!(op.get_num_inputs(), 0);
        assert_eq!(op.get_num_outputs(), 1);
        assert_eq!(op.get_attrs_dict().size(), 0);
        assert_eq!(*op.get_output(0).get_type(), val_ty);
        assert_eq!(op.to_string_repr(), "%0 = \"custom\"() : () -> (i32)");
    }

    #[test]
    fn test_op_with_block() {
        let loc = Location::unknown_test_loc();
        let val_ty = IntegerType::new(32, None);
        let mut ctx = IRContext::new();
        let mut b = IRBuilder::new(&mut ctx);

        let my_fun = make_my_fun(&mut b);
        let block: BlockID = b.create_block_at_begin_of(my_fun, loc, vec![]).into();
        b.set_insertion_point(InsertionPoint::AtEndOf(block));

        let lhs = make_custom(&mut b, val_ty.clone());
        let rhs = make_custom(&mut b, val_ty.clone());
        make_my_add(&mut b, lhs, rhs, val_ty.clone());

        // Check it
        let fun_op = ctx.get_generic_operation(my_fun);
        assert_eq!(fun_op.get_num_blocks(), 1);
        assert_eq!(fun_op.to_string_repr(), "\"my_fun\"() : () -> () {\n    ^() {\n        %0 = \"custom\"() : () -> (i32)\n        %1 = \"custom\"() : () -> (i32)\n        %2 = \"my_add\"(%0, %1) : (i32, i32) -> (i32)\n    }\n    \n}");
    }
}

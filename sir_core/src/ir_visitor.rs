use crate::{
    block::Block,
    operation::{GenericOperation, OperationImpl},
};

pub trait IRVisitor<'a, T: OperationImpl<'a>> {
    // Function called every time we visit an op.
    fn visit_op(&self, _op: T) {}

    // Function called every time we visit a block.
    fn visit_block(&self, _op: Block<'a>) {}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WalkOlder {
    PreOrder,
    PostOrder,
}

pub fn walk_ir<'a, T: OperationImpl<'a>, Visitor: IRVisitor<'a, T>>(
    root: GenericOperation<'a>,
    order: WalkOlder,
    v: &Visitor,
) {
    _walk_op_rec(root, order, v);
}

fn _walk_op_rec<'a, T: OperationImpl<'a>, Visitor: IRVisitor<'a, T>>(
    op: GenericOperation<'a>,
    order: WalkOlder,
    v: &Visitor,
) {
    let visit_op = op.isa::<T>();

    if visit_op && order == WalkOlder::PreOrder {
        let t_op = T::make_from_data(op.get_context(), op.get_op_data());
        v.visit_op(t_op);
    }

    for block in op.get_blocks() {
        _walk_block_rec(block, order, v);
    }

    if visit_op && order == WalkOlder::PostOrder {
        let t_op = T::make_from_data(op.get_context(), op.get_op_data());
        v.visit_op(t_op);
    }
}

fn _walk_block_rec<'a, T: OperationImpl<'a>, Visitor: IRVisitor<'a, T>>(
    block: Block<'a>,
    order: WalkOlder,
    v: &Visitor,
) {
    if order == WalkOlder::PreOrder {
        v.visit_block(block);
    }

    for op in block.get_ops() {
        _walk_op_rec(op, order, v);
    }

    if order == WalkOlder::PostOrder {
        v.visit_block(block);
    }
}

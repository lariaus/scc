use std::marker::PhantomData;

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

pub struct IRVisitorClosureImpl<'a, T: OperationImpl<'a>, F: Fn(T)> {
    _phantom: &'a PhantomData<T>,
    func: F,
}

impl<'a, T: OperationImpl<'a>, F: Fn(T)> IRVisitor<'a, T> for IRVisitorClosureImpl<'a, T, F> {
    fn visit_op(&self, op: T) {
        (self.func)(op)
    }
}

pub fn make_ir_visitor<'a, T: OperationImpl<'a>, F: Fn(T)>(
    func: F,
) -> IRVisitorClosureImpl<'a, T, F> {
    IRVisitorClosureImpl {
        _phantom: &PhantomData,
        func,
    }
}

pub trait IRVisitorMut<'a, T: OperationImpl<'a>> {
    // Function called every time we visit an op.
    fn visit_op(&mut self, _op: T) {}

    // Function called every time we visit a block.
    fn visit_block(&mut self, _op: Block<'a>) {}
}

pub fn walk_ir_mut<'a, T: OperationImpl<'a>, Visitor: IRVisitorMut<'a, T>>(
    root: GenericOperation<'a>,
    order: WalkOlder,
    v: &mut Visitor,
) {
    _walk_op_mut_rec(root, order, v);
}

fn _walk_op_mut_rec<'a, T: OperationImpl<'a>, Visitor: IRVisitorMut<'a, T>>(
    op: GenericOperation<'a>,
    order: WalkOlder,
    v: &mut Visitor,
) {
    let visit_op = op.isa::<T>();

    if visit_op && order == WalkOlder::PreOrder {
        let t_op = T::make_from_data(op.get_context(), op.get_op_data());
        v.visit_op(t_op);
    }

    for block in op.get_blocks() {
        _walk_block_mut_rec(block, order, v);
    }

    if visit_op && order == WalkOlder::PostOrder {
        let t_op = T::make_from_data(op.get_context(), op.get_op_data());
        v.visit_op(t_op);
    }
}

fn _walk_block_mut_rec<'a, T: OperationImpl<'a>, Visitor: IRVisitorMut<'a, T>>(
    block: Block<'a>,
    order: WalkOlder,
    v: &mut Visitor,
) {
    if order == WalkOlder::PreOrder {
        v.visit_block(block);
    }

    for op in block.get_ops() {
        _walk_op_mut_rec(op, order, v);
    }

    if order == WalkOlder::PostOrder {
        v.visit_block(block);
    }
}

pub struct IRVisitorMutClosureImpl<'a, T: OperationImpl<'a>, F: FnMut(T)> {
    _phantom: &'a PhantomData<T>,
    func: F,
}

impl<'a, T: OperationImpl<'a>, F: FnMut(T)> IRVisitorMut<'a, T>
    for IRVisitorMutClosureImpl<'a, T, F>
{
    fn visit_op(&mut self, op: T) {
        (self.func)(op)
    }
}

pub fn make_ir_visitor_mut<'a, T: OperationImpl<'a>, F: FnMut(T)>(
    func: F,
) -> IRVisitorMutClosureImpl<'a, T, F> {
    IRVisitorMutClosureImpl {
        _phantom: &PhantomData,
        func,
    }
}

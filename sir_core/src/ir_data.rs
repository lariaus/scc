use iostreams::location::Location;

use crate::{attributes::DictAttr, operation_type::OperationTypeRef, types::Type};

// Represent the operand for a block in the IR.
pub struct BlockOperand {
    uid: ValueID,
    block: BlockID,
    arg_idx: usize,
    ty: Type,
    loc: Location,
}

impl BlockOperand {
    pub(crate) fn new(
        uid: ValueID,
        block: BlockID,
        arg_idx: usize,
        ty: Type,
        loc: Location,
    ) -> Self {
        Self {
            uid,
            block,
            arg_idx,
            ty,
            loc,
        }
    }

    pub fn as_id(&self) -> ValueID {
        self.uid
    }

    // Returns the corresponding block.
    pub fn block(&self) -> BlockID {
        self.block
    }

    // Returns the operand number for this block operand.
    pub fn arg_idx(&self) -> usize {
        self.arg_idx
    }

    pub fn get_type(&self) -> &Type {
        &self.ty
    }

    pub fn loc(&self) -> Location {
        self.loc
    }
}

// Represent the output of an operation in the IR.
pub struct OperationOutput {
    uid: ValueID,
    op: OperationID,
    output_idx: usize,
    ty: Type,
    loc: Location,
}

impl OperationOutput {
    pub(crate) fn new(
        uid: ValueID,
        op: OperationID,
        output_idx: usize,
        ty: Type,
        loc: Location,
    ) -> Self {
        Self {
            uid,
            op,
            output_idx,
            ty,
            loc,
        }
    }

    pub fn as_id(&self) -> ValueID {
        self.uid
    }

    // Returns the corresponding operation.
    pub fn op(&self) -> OperationID {
        self.op
    }

    // Returns the output index number of the operation.
    pub fn output_idx(&self) -> usize {
        self.output_idx
    }

    pub fn get_type(&self) -> &Type {
        &self.ty
    }

    pub fn loc(&self) -> Location {
        self.loc
    }
}

// Represent any value in the IR.
pub enum ValueData {
    BlockOperand(BlockOperand),
    OperationOutput(OperationOutput),
}

impl ValueData {
    pub fn loc(&self) -> Location {
        match self {
            ValueData::BlockOperand(v) => v.loc(),
            ValueData::OperationOutput(v) => v.loc(),
        }
    }

    pub fn as_id(&self) -> ValueID {
        match self {
            ValueData::BlockOperand(v) => v.as_id(),
            ValueData::OperationOutput(v) => v.as_id(),
        }
    }

    // Get the value type.
    pub fn get_type(&self) -> &Type {
        match self {
            ValueData::BlockOperand(v) => v.get_type(),
            ValueData::OperationOutput(v) => v.get_type(),
        }
    }

    // Get the operation that defined this value.
    // Or None for a block operand.
    pub fn get_opdef(&self) -> Option<OperationID> {
        match self {
            ValueData::BlockOperand(_) => None,
            ValueData::OperationOutput(v) => Some(v.op()),
        }
    }
}

// Unique identifier for a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueID(pub(crate) usize);

// Represent a block in the IR. (A block is a sequence of operations)
pub struct BlockData {
    uid: BlockID,
    loc: Location,
    args: Vec<ValueID>,
    ops: Vec<OperationID>,
    parent: Option<OperationID>,
}

impl BlockData {
    pub(crate) fn new(
        uid: BlockID,
        loc: Location,
        args: Vec<ValueID>,
        ops: Vec<OperationID>,
    ) -> Self {
        Self {
            uid,
            loc,
            args,
            ops,
            parent: None,
        }
    }

    pub fn as_id(&self) -> BlockID {
        self.uid
    }

    pub fn loc(&self) -> Location {
        self.loc
    }

    pub fn args(&self) -> &[ValueID] {
        &self.args
    }

    pub fn ops(&self) -> &[OperationID] {
        &self.ops
    }

    // Returns the parent operation of the block.
    // or none if it doesn't have one.
    pub fn parent(&self) -> Option<OperationID> {
        self.parent
    }

    pub(crate) fn ops_mut(&mut self) -> &mut Vec<OperationID> {
        &mut self.ops
    }

    pub(crate) fn set_parent(&mut self, parent: Option<OperationID>) {
        self.parent = parent;
    }
}

// Unique identifier for a block.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockID(pub(crate) usize);

// This is the raw data form of an operation, that can represent any kind of generic op.
pub struct OperationData {
    uid: OperationID,
    loc: Location,
    op_type: OperationTypeRef,
    inputs: Vec<ValueID>,
    outputs: Vec<ValueID>,
    attrs: DictAttr,
    blocks: Vec<BlockID>,
    parent: Option<BlockID>,
}

impl OperationData {
    pub(crate) fn new(
        uid: OperationID,
        loc: Location,
        op_type: OperationTypeRef,
        inputs: Vec<ValueID>,
        outputs: Vec<ValueID>,
        attrs: DictAttr,
        blocks: Vec<BlockID>,
    ) -> Self {
        Self {
            uid,
            loc,
            op_type,
            inputs,
            outputs,
            attrs,
            blocks,
            parent: None,
        }
    }

    pub fn as_id(&self) -> OperationID {
        self.uid
    }

    pub fn loc(&self) -> Location {
        self.loc
    }

    // Get informations about the operation.
    pub fn op_type(&self) -> &OperationTypeRef {
        &self.op_type
    }

    // Returns the inputs values of the operation.
    pub fn inputs(&self) -> &[ValueID] {
        &self.inputs
    }

    // Returns the outputs values of the operation.
    pub fn outputs(&self) -> &[ValueID] {
        &self.outputs
    }

    // Returns the attributes dictionnary of the operation.
    pub fn attrs(&self) -> &DictAttr {
        &self.attrs
    }

    // Returns the children blocks of the operation.
    pub fn blocks(&self) -> &[BlockID] {
        &self.blocks
    }

    // Returns the parent block of the op.
    // Or returns none if it doesn't have one.
    pub fn parent(&self) -> Option<BlockID> {
        self.parent
    }

    pub(crate) fn blocks_mut(&mut self) -> &mut Vec<BlockID> {
        &mut self.blocks
    }

    pub(crate) fn set_parent(&mut self, parent: Option<BlockID>) {
        self.parent = parent;
    }
}

// Unique identifier for an operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OperationID(pub(crate) usize);

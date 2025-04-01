use sir_core::op_tags::OperationTag;

// This tag indicates that an op should always be LowLevel.
pub const TAG_LOW_LEVEL_OP: OperationTag = OperationTag::make_from_name("LowLevelOp");

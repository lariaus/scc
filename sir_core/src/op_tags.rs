use const_fnv1a_hash::fnv1a_hash_str_64;

// Unique Tag Object
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OperationTag(u64);

impl OperationTag {
    pub const fn make_from_name(opname: &'static str) -> Self {
        Self(fnv1a_hash_str_64(opname))
    }
}

// This tag indicates that an op is a terminator.
pub const TAG_TERMINATOR_OP: OperationTag = OperationTag::make_from_name("Terminator");

// This tag indicates that the op is just a container of declarations.
pub const TAG_DECLS_BLOCK_OP: OperationTag = OperationTag::make_from_name("DeclsBlock");

// This tag indicates that this op has no side-effects and can be removed if unused.
pub const TAG_PURE_OP: OperationTag = OperationTag::make_from_name("Pure");

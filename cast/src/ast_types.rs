// Represent the types an object can take in the AST.

use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegerSignedness {
    Signed,
    Unsigned,
    Signless,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegerSize {
    Char,
    Short,
    Int,
    Long,
}

// Type used for all integers (short / chart / int / etc)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntegerType {
    size: IntegerSize,
    signedness: IntegerSignedness,
}

impl IntegerType {
    pub fn new(size: IntegerSize, signedness: IntegerSignedness) -> Self {
        Self { size, signedness }
    }

    pub fn size(&self) -> IntegerSize {
        self.size
    }

    pub fn signedness(&self) -> IntegerSignedness {
        self.signedness
    }
}

impl Display for IntegerType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.signedness == IntegerSignedness::Signed {
            write!(f, "signed ")?;
        } else if self.signedness == IntegerSignedness::Unsigned {
            write!(f, "unsigned ")?;
        }
        match self.size {
            IntegerSize::Char => write!(f, "char"),
            IntegerSize::Short => write!(f, "short"),
            IntegerSize::Int => write!(f, "int"),
            IntegerSize::Long => write!(f, "long"),
        }
    }
}

// Type used for all float types (float, double, etc).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatType {
    F32,
    F64,
}

impl Display for FloatType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FloatType::F32 => write!(f, "float"),
            FloatType::F64 => write!(f, "double"),
        }
    }
}

// Generate Type enum that regroup all possible C types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer(IntegerType),
    Float(FloatType),
    Void,
    Error,
}

impl Type {
    // Returns true if self is the void type.
    pub fn is_void(&self) -> bool {
        match self {
            Type::Void => true,
            _ => false,
        }
    }

    // Returns true if self is the error type.
    pub fn is_error(&self) -> bool {
        match self {
            Type::Error => true,
            _ => false,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Integer(ty) => ty.fmt(f),
            Type::Float(ty) => ty.fmt(f),
            Type::Void => write!(f, "void"),
            Type::Error => write!(f, "<<ERROR TYPE>>"),
        }
    }
}

// Names of the c types.
pub const TYPENAME_CHAR: &'static str = "char";
pub const TYPENAME_SIGNED_CHAR: &'static str = "signed char";
pub const TYPENAME_UNSIGNED_CHAR: &'static str = "unsigned char";
pub const TYPENAME_SHORT: &'static str = "short";
pub const TYPENAME_SIGNED_SHORT: &'static str = "signed short";
pub const TYPENAME_UNSIGNED_SHORT: &'static str = "unsigned short";
pub const TYPENAME_INT: &'static str = "int";
pub const TYPENAME_SIGNED_INT: &'static str = "signed int";
pub const TYPENAME_UNSIGNED_INT: &'static str = "unsigned int";
pub const TYPENAME_LONG: &'static str = "long";
pub const TYPENAME_SIGNED_LONG: &'static str = "signed long";
pub const TYPENAME_UNSIGNED_LONG: &'static str = "unsigned long";
pub const TYPENAME_FLOAT: &'static str = "float";
pub const TYPENAME_DOUBLE: &'static str = "double";
pub const TYPENAME_VOID: &'static str = "void";

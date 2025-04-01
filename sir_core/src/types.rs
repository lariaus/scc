// Represent any types possible in this IR.

use diagnostics::emit_error;
use parse::{lexer::TokenValue, parser::Parser};

use crate::{
    ir_parser::IRParsableObject,
    ir_printer::{IRPrintableObject, IRPrinter},
};

// Any integer type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntegerType {
    bitwidth: usize,
    signed: Option<bool>,
}

impl IntegerType {
    pub fn new(bitwidth: usize, signed: Option<bool>) -> Type {
        Type::Int(Self { bitwidth, signed })
    }

    pub fn bitwidth(&self) -> usize {
        self.bitwidth
    }

    pub fn signed(&self) -> Option<bool> {
        self.signed
    }
}

impl IRPrintableObject for IntegerType {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        if let Some(signed) = self.signed() {
            if signed {
                write!(printer.os(), "s")?;
            } else {
                write!(printer.os(), "u")?;
            }
        }
        write!(printer.os(), "i{}", self.bitwidth())
    }
}

impl IRParsableObject for IntegerType {
    fn parse(parser: &mut crate::ir_parser::IRParser) -> Option<Self> {
        let loc = parser.get_next_token_loc();
        let id = match parser.consume_identifier_or_error() {
            Some(tok) => tok.take_identifier().unwrap(),
            None => return None,
        };

        let (signed, val_str) = if id.starts_with("si") {
            (Some(true), &id[2..])
        } else if id.starts_with("ui") {
            (Some(false), &id[2..])
        } else if id.starts_with("i") {
            (None, &id[1..])
        } else {
            emit_error(parser, loc, format!("Expected an integer type"));
            return None;
        };

        let val = val_str.parse::<usize>();
        let bitwidth = match val {
            Ok(val) => val,
            Err(_) => {
                emit_error(
                    parser,
                    loc,
                    format!("Failed to parse integer bitwidth for integer type `{}`", id),
                );
                return None;
            }
        };
        Some(IntegerType { bitwidth, signed })
    }
}

// Any float type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatType {
    F32,
    F64,
}

impl IRParsableObject for FloatType {
    fn parse(parser: &mut crate::ir_parser::IRParser) -> Option<Self> {
        let loc = parser.get_next_token_loc();
        let id = match parser.consume_identifier_or_error() {
            Some(tok) => tok.take_identifier().unwrap(),
            None => return None,
        };
        if id == "f32" {
            Some(FloatType::F32)
        } else if id == "f64" {
            Some(FloatType::F64)
        } else {
            emit_error(
                parser,
                loc,
                format!("Invalid float type identifier `{}`", id),
            );
            None
        }
    }
}

impl IRPrintableObject for FloatType {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        match self {
            FloatType::F32 => write!(printer.os(), "f32"),
            FloatType::F64 => write!(printer.os(), "f64"),
        }
    }
}

// Any pointer type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PointerType {
    element: Box<Type>,
}

impl PointerType {
    pub fn new(element: Type) -> Type {
        Type::Ptr(Self {
            element: Box::new(element),
        })
    }

    pub fn element(&self) -> &Type {
        &self.element
    }
}

impl IRPrintableObject for PointerType {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        write!(printer.os(), "ptr<")?;
        self.element().print(printer)?;
        write!(printer.os(), ">")
    }
}

impl IRParsableObject for PointerType {
    fn parse(parser: &mut crate::ir_parser::IRParser) -> Option<Self> {
        parser.consume_identifier_val_or_error("ptr")?;
        parser.consume_sym_or_error(TokenValue::sym_lt())?;
        let element = Type::parse(parser)?;
        parser.consume_sym_or_error(TokenValue::sym_gt())?;
        Some(PointerType {
            element: Box::new(element),
        })
    }
}

// Any tuple types
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TupleType {
    elements: Vec<Type>,
}

impl TupleType {
    pub fn new(elements: Vec<Type>) -> Type {
        Type::Tuple(Self { elements })
    }

    pub fn size(&self) -> usize {
        self.elements.len()
    }

    pub fn elements(&self) -> &[Type] {
        &self.elements
    }
}

impl IRPrintableObject for TupleType {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        write!(printer.os(), "tuple<[")?;
        for (idx, elem) in self.elements().iter().enumerate() {
            elem.print(printer)?;
            if idx + 1 < self.elements().len() {
                write!(printer.os(), ", ")?;
            }
        }
        write!(printer.os(), "]>")
    }
}

impl IRParsableObject for TupleType {
    fn parse(parser: &mut crate::ir_parser::IRParser) -> Option<Self> {
        parser.consume_identifier_val_or_error("tuple")?;
        parser.consume_sym_or_error(TokenValue::sym_lt())?;
        parser.consume_sym_or_error(TokenValue::sym_lbracket())?;

        // Parse the elements.
        let mut elements = Vec::new();
        if !parser.next_token_is_sym(TokenValue::sym_rbracket()) {
            loop {
                elements.push(Type::parse(parser)?);
                if !parser.try_consume_sym(TokenValue::sym_comma()) {
                    break;
                }
            }
        }

        parser.consume_sym_or_error(TokenValue::sym_rbracket())?;
        parser.consume_sym_or_error(TokenValue::sym_gt())?;
        Some(TupleType { elements })
    }
}

// Any array types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayType {
    element: Box<Type>,
    size: usize,
}

impl ArrayType {
    pub fn new(element: Type, size: usize) -> Type {
        Type::Array(Self {
            element: Box::new(element),
            size,
        })
    }

    pub fn element(&self) -> &Type {
        &self.element
    }

    pub fn size(&self) -> usize {
        self.size
    }
}

impl IRPrintableObject for ArrayType {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        write!(printer.os(), "array<")?;
        self.element().print(printer)?;
        write!(printer.os(), "; {}>", self.size)
    }
}

impl IRParsableObject for ArrayType {
    fn parse(parser: &mut crate::ir_parser::IRParser) -> Option<Self> {
        parser.consume_identifier_val_or_error("array")?;
        parser.consume_sym_or_error(TokenValue::sym_lt())?;
        let element = Type::parse(parser)?;
        parser.consume_sym_or_error(TokenValue::sym_semi())?;
        let size = parser.consume_int_or_error()?.get_int().unwrap() as usize;
        parser.consume_sym_or_error(TokenValue::sym_gt())?;
        Some(ArrayType {
            element: Box::new(element),
            size,
        })
    }
}

// The base type class for any value in the IR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int(IntegerType),
    Float(FloatType),
    Str,
    Ptr(PointerType),
    Tuple(TupleType),
    Array(ArrayType),
}

impl Type {
    pub fn is_int(&self) -> bool {
        match self {
            Type::Int(_) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Type::Float(_) => true,
            _ => false,
        }
    }

    pub fn is_str(&self) -> bool {
        match self {
            Type::Str => true,
            _ => false,
        }
    }

    pub fn is_ptr(&self) -> bool {
        match self {
            Type::Ptr(_) => true,
            _ => false,
        }
    }

    pub fn is_tuple(&self) -> bool {
        match self {
            Type::Tuple(_) => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            Type::Array(_) => true,
            _ => false,
        }
    }

    // Construct an integer type.
    pub fn make_int(bitwidth: usize, signed: Option<bool>) -> Self {
        IntegerType::new(bitwidth, signed)
    }
}

impl IRPrintableObject for Type {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        match self {
            Type::Int(ty) => ty.print(printer),
            Type::Float(ty) => ty.print(printer),
            Type::Str => write!(printer.os(), "string"),
            Type::Ptr(ty) => ty.print(printer),
            Type::Tuple(ty) => ty.print(printer),
            Type::Array(ty) => ty.print(printer),
        }
    }
}

impl IRParsableObject for Type {
    fn parse(parser: &mut crate::ir_parser::IRParser) -> Option<Self> {
        // Look at the first token (must be an identifier).
        let tok = parser.peek_token().clone();
        let id = match tok.val() {
            TokenValue::Identifier(id) => id,
            _ => {
                parser.emit_bad_token_error(&tok, format!("type identifier"));
                return None;
            }
        };

        // From the identifier identify the type and dispatch to the right parser.
        if id.starts_with("si") || id.starts_with("ui") || id.starts_with("i") {
            Some(Type::Int(IntegerType::parse(parser)?))
        } else if id == "f32" || id == "f64" {
            Some(Type::Float(FloatType::parse(parser)?))
        } else if id == "string" {
            parser.get_token();
            Some(Type::Str)
        } else if id == "ptr" {
            Some(Type::Ptr(PointerType::parse(parser)?))
        } else if id == "tuple" {
            Some(Type::Tuple(TupleType::parse(parser)?))
        } else if id == "array" {
            Some(Type::Array(ArrayType::parse(parser)?))
        } else {
            parser.emit_bad_token_error(&tok, format!("type identifier"));
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_same_repr(ty_str: &str) {
        let ty = Type::from_string_repr(ty_str.to_owned());
        let ty_str_v2 = ty.to_string_repr();
        assert_eq!(ty_str, ty_str_v2);
    }

    #[test]
    fn test_int_print_parse() {
        let ty = IntegerType::new(32, None);
        assert_eq!(ty.to_string_repr(), "i32");
        assert_eq!(ty, Type::from_string_repr("i32".to_owned()));
        check_same_repr("i32");

        let ty = IntegerType::new(16, Some(true));
        assert_eq!(ty.to_string_repr(), "si16");
        assert_eq!(ty, Type::from_string_repr("si16".to_owned()));
        check_same_repr("si16");

        let ty = IntegerType::new(64, Some(false));
        assert_eq!(ty.to_string_repr(), "ui64");
        assert_eq!(ty, Type::from_string_repr("ui64".to_owned()));
        check_same_repr("ui64");
    }

    #[test]
    fn test_float_print_parse() {
        let ty = Type::Float(FloatType::F32);
        assert_eq!(ty.to_string_repr(), "f32");
        assert_eq!(ty, Type::from_string_repr("f32".to_owned()));
        check_same_repr("f32");

        let ty = Type::Float(FloatType::F64);
        assert_eq!(ty.to_string_repr(), "f64");
        assert_eq!(ty, Type::from_string_repr("f64".to_owned()));
        check_same_repr("f64");
    }

    #[test]
    fn test_string_print_parse() {
        let ty = Type::Str;
        assert_eq!(ty.to_string_repr(), "string");
        assert_eq!(ty, Type::from_string_repr("string".to_owned()));
        check_same_repr("string");
    }

    #[test]
    fn test_pointer_print_parse() {
        let i32_ty = IntegerType::new(32, None);
        let si64_ty = IntegerType::new(64, Some(true));
        let i32p_ty = PointerType::new(i32_ty.clone());
        let si64p_ty = PointerType::new(si64_ty.clone());
        let si64pp_ty = PointerType::new(si64p_ty.clone());

        assert_eq!(i32_ty.to_string_repr(), "i32");
        assert_eq!(i32_ty, Type::from_string_repr("i32".to_owned()));
        assert_eq!(si64_ty.to_string_repr(), "si64");
        assert_eq!(si64_ty, Type::from_string_repr("si64".to_owned()));
        assert_eq!(i32p_ty.to_string_repr(), "ptr<i32>");
        assert_eq!(i32p_ty, Type::from_string_repr("ptr<i32>".to_owned()));
        assert_eq!(si64p_ty.to_string_repr(), "ptr<si64>");
        assert_eq!(si64p_ty, Type::from_string_repr("ptr<si64>".to_owned()));
        assert_eq!(si64pp_ty.to_string_repr(), "ptr<ptr<si64>>");
        assert_eq!(
            si64pp_ty,
            Type::from_string_repr("ptr<ptr<si64>>".to_owned())
        );
        check_same_repr("ptr<ptr<si64>>");
    }

    #[test]
    fn test_tuple_print_parse() {
        let ty = TupleType::new(vec![]);
        assert_eq!(ty.to_string_repr(), "tuple<[]>");
        assert_eq!(ty, Type::from_string_repr("tuple<[]>".to_owned()));

        let ty = TupleType::new(vec![
            IntegerType::new(32, None),
            Type::Str,
            Type::Float(FloatType::F64),
        ]);
        assert_eq!(ty.to_string_repr(), "tuple<[i32, string, f64]>");
        assert_eq!(
            ty,
            Type::from_string_repr("tuple<[i32, string, f64]>".to_owned())
        );
        check_same_repr("tuple<[i32, string, f64]>");
    }

    #[test]
    fn test_array_print_parse() {
        let int_array_ty = ArrayType::new(IntegerType::new(32, None), 4);
        let int_array_array_ty = ArrayType::new(int_array_ty.clone(), 64);
        assert_eq!(int_array_ty.to_string_repr(), "array<i32; 4>");
        assert_eq!(
            int_array_ty,
            Type::from_string_repr("array<i32; 4>".to_owned())
        );
        assert_eq!(
            int_array_array_ty.to_string_repr(),
            "array<array<i32; 4>; 64>"
        );
        assert_eq!(
            int_array_array_ty,
            Type::from_string_repr("array<array<i32; 4>; 64>".to_owned())
        );
        check_same_repr("array<array<i32; 4>; 64>");
    }
}

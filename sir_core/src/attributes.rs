// Attributes are constant values defined in the IR, attached to each op.

use parse::{lexer::TokenValue, parser::Parser};
use utils::stringutils::encode_string_literal;

use crate::{
    ir_parser::IRParsableObject,
    ir_printer::{IRPrintableObject, IRPrinter},
    types::Type,
};

// Represent any constant integer value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegerAttr {
    val: u64,
    ty: Type,
}

impl IntegerAttr {
    pub fn new(val: u64, ty: Type) -> Attribute {
        assert!(ty.is_int(), "IntegerAttr must have an integer type");
        Attribute::Int(Self { val, ty })
    }

    pub fn raw_val(&self) -> u64 {
        self.val
    }
}

impl IRPrintableObject for IntegerAttr {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        // TODO: it's more complicated to print signed values ?
        write!(printer.os(), "{}: ", self.raw_val())?;
        printer.print(&self.ty)
    }
}

impl IRParsableObject for IntegerAttr {
    fn parse(parser: &mut crate::ir_parser::IRParser) -> Option<Self> {
        // TODO: Support parsing properly signed values.
        let val = parser.consume_int_or_error()?.get_int().unwrap();
        parser.consume_sym_or_error(TokenValue::sym_colon())?;
        let ty = Type::parse(parser)?;
        Some(IntegerAttr { val, ty })
    }
}

// Represent any constant float value.
#[derive(Debug, Clone, PartialEq)]
pub struct FloatAttr {
    val: f64,
    ty: Type,
}

impl FloatAttr {
    pub fn new(val: f64, ty: Type) -> Attribute {
        assert!(ty.is_float(), "FloatAttr must have a float type");
        Attribute::Float(Self { val, ty })
    }

    pub fn raw_val(&self) -> f64 {
        self.val
    }
}

impl IRPrintableObject for FloatAttr {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        write!(printer.os(), "{}: ", self.raw_val())?;
        printer.print(&self.ty)
    }
}

impl IRParsableObject for FloatAttr {
    fn parse(parser: &mut crate::ir_parser::IRParser) -> Option<Self> {
        let val = parser.consume_float_or_error()?.get_float().unwrap();
        parser.consume_sym_or_error(TokenValue::sym_colon())?;
        let ty = Type::parse(parser)?;
        Some(FloatAttr { val, ty })
    }
}

// Represent any constant string value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringAttr {
    val: String,
}

impl StringAttr {
    pub fn new(val: String) -> Attribute {
        Attribute::Str(Self { val })
    }

    pub fn val(&self) -> &str {
        &self.val
    }
}

impl IRPrintableObject for StringAttr {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        write!(printer.os(), "\"{}\"", encode_string_literal(&self.val))
    }
}

impl IRParsableObject for StringAttr {
    fn parse(parser: &mut crate::ir_parser::IRParser) -> Option<Self> {
        let val = parser
            .consume_string_literal_or_error()?
            .take_string_literal()
            .unwrap();
        Some(StringAttr { val })
    }
}

// Heterogenous untyped array of attributes.
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayAttr {
    elements: Vec<Attribute>,
}

impl ArrayAttr {
    pub fn new(elements: Vec<Attribute>) -> Attribute {
        Attribute::Array(Self { elements })
    }

    pub fn size(&self) -> usize {
        self.elements.len()
    }

    pub fn elements(&self) -> &[Attribute] {
        &self.elements
    }
}

impl IRPrintableObject for ArrayAttr {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        write!(printer.os(), "[")?;
        for (idx, element) in self.elements().iter().enumerate() {
            printer.print(element)?;
            if idx + 1 < self.size() {
                write!(printer.os(), ", ")?;
            }
        }
        write!(printer.os(), "]")
    }
}

impl IRParsableObject for ArrayAttr {
    fn parse(parser: &mut crate::ir_parser::IRParser) -> Option<Self> {
        parser.consume_sym_or_error(TokenValue::sym_lbracket())?;

        // Parse all elements.
        let mut elements = vec![];
        if !parser.next_token_is_sym(TokenValue::sym_rbracket()) {
            loop {
                elements.push(Attribute::parse(parser)?);
                if !parser.try_consume_sym(TokenValue::sym_comma()) {
                    break;
                }
            }
        }
        parser.consume_sym_or_error(TokenValue::sym_rbracket())?;
        Some(ArrayAttr { elements })
    }
}

// Heterogenous untyped dictionary of attributes with string keys.
#[derive(Debug, Clone, PartialEq)]
pub struct DictAttr {
    // Weird implementation because HashMap doesn't support Eq
    elements: Vec<(Attribute, Attribute)>,
}

impl DictAttr {
    // Create an empty dictionary.
    pub fn empty() -> Self {
        Self { elements: vec![] }
    }

    pub fn new(elements: Vec<(Attribute, Attribute)>) -> Self {
        for (key, _val) in &elements {
            assert!(key.is_string(), "the keys of a dict must be string");
        }
        Self { elements }
    }

    pub fn size(&self) -> usize {
        self.elements.len()
    }

    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }

    pub fn elements(&self) -> &[(Attribute, Attribute)] {
        &self.elements
    }
}

impl IRPrintableObject for DictAttr {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        write!(printer.os(), "{{")?;
        for (idx, (key, val)) in self.elements().iter().enumerate() {
            printer.print(key)?;
            write!(printer.os(), " = ")?;
            printer.print(val)?;
            if idx + 1 < self.size() {
                write!(printer.os(), ", ")?;
            }
        }
        write!(printer.os(), "}}")
    }
}

impl IRParsableObject for DictAttr {
    fn parse(parser: &mut crate::ir_parser::IRParser) -> Option<Self> {
        parser.consume_sym_or_error(TokenValue::sym_lcbracket())?;

        // Parse all elements.
        let mut elements = vec![];
        if !parser.next_token_is_sym(TokenValue::sym_rcbracket()) {
            loop {
                let key = Attribute::parse(parser)?;
                parser.consume_sym_or_error(TokenValue::sym_assign())?;
                let val = Attribute::parse(parser)?;
                elements.push((key, val));

                if !parser.try_consume_sym(TokenValue::sym_comma()) {
                    break;
                }
            }
        }

        parser.consume_sym_or_error(TokenValue::sym_rcbracket())?;
        Some(DictAttr { elements })
    }
}

// Represent all possible Attributes in the IR.
#[derive(Debug, Clone, PartialEq)]
pub enum Attribute {
    Int(IntegerAttr),
    Float(FloatAttr),
    Str(StringAttr),
    Array(ArrayAttr),
    Dict(DictAttr),
}

impl Attribute {
    pub fn is_int(&self) -> bool {
        match self {
            Attribute::Int(_) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Attribute::Float(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Attribute::Str(_) => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            Attribute::Array(_) => true,
            _ => false,
        }
    }

    pub fn is_dict(&self) -> bool {
        match self {
            Attribute::Dict(_) => true,
            _ => false,
        }
    }
}

impl IRPrintableObject for Attribute {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        match self {
            Attribute::Int(attr) => attr.print(printer),
            Attribute::Float(attr) => attr.print(printer),
            Attribute::Str(attr) => attr.print(printer),
            Attribute::Array(attr) => attr.print(printer),
            Attribute::Dict(attr) => attr.print(printer),
        }
    }
}

impl IRParsableObject for Attribute {
    fn parse(parser: &mut crate::ir_parser::IRParser) -> Option<Self> {
        // Figure out the attribute based on the first token.

        if parser.next_token_is_int() {
            Some(Attribute::Int(IntegerAttr::parse(parser)?))
        } else if parser.next_token_is_float() {
            Some(Attribute::Float(FloatAttr::parse(parser)?))
        } else if parser.next_token_is_string_literal() {
            Some(Attribute::Str(StringAttr::parse(parser)?))
        } else if parser.next_token_is_sym(TokenValue::sym_lbracket()) {
            Some(Attribute::Array(ArrayAttr::parse(parser)?))
        } else if parser.next_token_is_sym(TokenValue::sym_lcbracket()) {
            Some(Attribute::Dict(DictAttr::parse(parser)?))
        } else {
            let err_tok = parser.peek_token().clone();
            parser.emit_bad_token_error(&err_tok, format!("attribute value"));
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::types::{FloatType, IntegerType};

    use super::*;

    fn check_same_repr(attr_str: &str) {
        let attr = Attribute::from_string_repr(attr_str.to_owned());
        let attr_str_v2 = attr.to_string_repr();
        assert_eq!(attr_str, attr_str_v2);
    }

    #[test]
    fn test_int_print_parse() {
        // TODO: Add more complex tests.
        let attr = IntegerAttr::new(42, IntegerType::new(32, None));
        assert_eq!(attr.to_string_repr(), "42: i32");
        assert_eq!(attr, Attribute::from_string_repr("42: i32".to_owned()));
        check_same_repr("42: si32");
    }

    #[test]
    fn test_float_print_parse() {
        // TODO: Add more complex tests.
        let attr = FloatAttr::new(2.5, Type::Float(FloatType::F32));
        assert_eq!(attr.to_string_repr(), "2.5: f32");
        assert_eq!(attr, Attribute::from_string_repr("2.5: f32".to_owned()));
        check_same_repr("2.5: f32");
    }

    #[test]
    fn test_string_print_parse() {
        let attr = StringAttr::new("Hello !".to_string());
        assert_eq!(attr.to_string_repr(), "\"Hello !\"");
        assert_eq!(attr, Attribute::from_string_repr("\"Hello !\"".to_string()));
        check_same_repr("\"Hello !\"");

        let attr = StringAttr::new("".to_string());
        assert_eq!(attr.to_string_repr(), "\"\"");
        assert_eq!(attr, Attribute::from_string_repr("\"\"".to_string()));
        check_same_repr("\"\"");
    }

    #[test]
    fn test_array_print_parse() {
        let attr = ArrayAttr::new(vec![
            StringAttr::new("foo".to_string()),
            FloatAttr::new(4.5, Type::Float(FloatType::F64)),
        ]);
        assert_eq!(attr.to_string_repr(), "[\"foo\", 4.5: f64]");
        assert_eq!(
            attr,
            Attribute::from_string_repr("[\"foo\", 4.5: f64]".to_owned())
        );
        check_same_repr("[\"foo\", 4.5: f64]");

        let attr = ArrayAttr::new(vec![]);
        assert_eq!(attr.to_string_repr(), "[]");
        assert_eq!(attr, Attribute::from_string_repr("[]".to_owned()));
        check_same_repr("[]");
    }

    #[test]
    fn test_dict_print_parser() {
        let attr = Attribute::Dict(DictAttr::new(vec![
            (
                StringAttr::new("foo".to_string()),
                FloatAttr::new(4.5, Type::Float(FloatType::F64)),
            ),
            (
                StringAttr::new("bar".to_string()),
                FloatAttr::new(6.5, Type::Float(FloatType::F32)),
            ),
        ]));
        assert_eq!(
            attr.to_string_repr(),
            "{\"foo\" = 4.5: f64, \"bar\" = 6.5: f32}"
        );
        assert_eq!(
            attr,
            Attribute::from_string_repr("{\"foo\" = 4.5: f64, \"bar\" = 6.5: f32}".to_owned())
        );
        check_same_repr("{\"foo\" = 4.5: f64, \"bar\" = 6.5: f32}");

        let attr = Attribute::Dict(DictAttr::empty());
        assert_eq!(attr.to_string_repr(), "{}");
        assert_eq!(attr, Attribute::from_string_repr("{}".to_owned()));
        check_same_repr("{}");
    }
}

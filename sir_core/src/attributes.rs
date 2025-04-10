// Attributes are constant values defined in the IR, attached to each op.

use std::hash::Hash;

use parse::{lexer::TokenValue, parser::Parser};
use utils::stringutils::encode_string_literal;

use crate::{
    ir_parser::IRParsableObject,
    ir_printer::{IRPrintableObject, IRPrinter},
    types::{token_is_start_of_type, IntegerType, Type},
};

// Trait implemented by all concrete attribute struct.
pub trait AttributeSubClass {
    // Returns true if attr is a Self.
    fn is_of_type(attr: &Attribute) -> bool;

    // Downcast attr to Self (only if it's a Self)
    fn downcast_to_self<'a>(attr: &'a Attribute) -> Option<&'a Self>;

    // Returns a typename for the Attribute object.
    fn get_typename() -> &'static str;

    // Returns the type of the attribute, or none if it has no type.
    fn get_type(&self) -> Option<&Type> {
        None
    }
}

// Represent any constant integer value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    // Returns the bitwidth of the type
    pub fn bitwidth(&self) -> usize {
        self.ty.cast::<IntegerType>().unwrap().bitwidth()
    }
}

impl AttributeSubClass for IntegerAttr {
    fn is_of_type(attr: &Attribute) -> bool {
        match attr {
            Attribute::Int(_) => true,
            _ => false,
        }
    }

    fn downcast_to_self<'a>(attr: &'a Attribute) -> Option<&'a Self> {
        match attr {
            Attribute::Int(attr) => Some(attr),
            _ => None,
        }
    }

    fn get_typename() -> &'static str {
        "Integer"
    }

    fn get_type(&self) -> Option<&Type> {
        Some(&self.ty)
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

// Is this actually safe to Hash a float number this way ?
impl Hash for FloatAttr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.val.to_bits().hash(state);
        self.ty.hash(state);
    }
}

impl AttributeSubClass for FloatAttr {
    fn is_of_type(attr: &Attribute) -> bool {
        match attr {
            Attribute::Float(_) => true,
            _ => false,
        }
    }

    fn downcast_to_self<'a>(attr: &'a Attribute) -> Option<&'a Self> {
        match attr {
            Attribute::Float(attr) => Some(attr),
            _ => None,
        }
    }

    fn get_typename() -> &'static str {
        "Float"
    }

    fn get_type(&self) -> Option<&Type> {
        Some(&self.ty)
    }
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

impl AttributeSubClass for StringAttr {
    fn is_of_type(attr: &Attribute) -> bool {
        match attr {
            Attribute::Str(_) => true,
            _ => false,
        }
    }

    fn downcast_to_self<'a>(attr: &'a Attribute) -> Option<&'a Self> {
        match attr {
            Attribute::Str(attr) => Some(attr),
            _ => None,
        }
    }

    fn get_typename() -> &'static str {
        "String"
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

impl Into<StringAttr> for String {
    fn into(self) -> StringAttr {
        StringAttr { val: self }
    }
}

impl Into<StringAttr> for &str {
    fn into(self) -> StringAttr {
        StringAttr {
            val: self.to_owned(),
        }
    }
}

// Heterogenous untyped array of attributes.
#[derive(Debug, Clone, PartialEq, Hash)]
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

impl AttributeSubClass for ArrayAttr {
    fn is_of_type(attr: &Attribute) -> bool {
        match attr {
            Attribute::Array(_) => true,
            _ => false,
        }
    }

    fn downcast_to_self<'a>(attr: &'a Attribute) -> Option<&'a Self> {
        match attr {
            Attribute::Array(attr) => Some(attr),
            _ => None,
        }
    }

    fn get_typename() -> &'static str {
        "Array"
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
#[derive(Debug, Clone, PartialEq, Hash)]
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

    // Find the attribute with key `key`.
    // Returns None if not found.
    pub fn get_with_str_key(&self, key: &str) -> Option<&Attribute> {
        Some(
            &self
                .elements()
                .iter()
                .find(|(elem_key, _elem_val)| {
                    let elem_key = match elem_key {
                        Attribute::Str(s) => s.val(),
                        _ => return false,
                    };
                    elem_key == key
                })?
                .1,
        )
    }
}

impl AttributeSubClass for DictAttr {
    fn is_of_type(attr: &Attribute) -> bool {
        match attr {
            Attribute::Dict(_) => true,
            _ => false,
        }
    }

    fn downcast_to_self<'a>(attr: &'a Attribute) -> Option<&'a Self> {
        match attr {
            Attribute::Dict(attr) => Some(attr),
            _ => None,
        }
    }
    fn get_typename() -> &'static str {
        "Dict"
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

// Attribute that refers to a type.
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct TypeAttr {
    val: Type,
}

impl TypeAttr {
    pub fn new(val: Type) -> Attribute {
        Attribute::Type(Self { val })
    }

    pub fn val(&self) -> &Type {
        &self.val
    }
}

impl AttributeSubClass for TypeAttr {
    fn is_of_type(attr: &Attribute) -> bool {
        match attr {
            Attribute::Type(_) => true,
            _ => false,
        }
    }

    fn downcast_to_self<'a>(attr: &'a Attribute) -> Option<&'a Self> {
        match attr {
            Attribute::Type(attr) => Some(attr),
            _ => None,
        }
    }

    fn get_typename() -> &'static str {
        "TypeAttr"
    }
}

impl IRPrintableObject for TypeAttr {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        printer.print(self.val())
    }
}

impl IRParsableObject for TypeAttr {
    fn parse(parser: &mut crate::ir_parser::IRParser) -> Option<Self> {
        let val = Type::parse(parser)?;
        Some(Self { val })
    }
}

// Represent any constant pointer value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PointerAttr {
    val: usize,
    ty: Type,
}

impl PointerAttr {
    pub fn new(val: usize, ty: Type) -> Attribute {
        assert!(ty.is_ptr(), "PointerAttr must have a pointer type");
        Attribute::Ptr(Self { val, ty })
    }

    pub fn val(&self) -> usize {
        self.val
    }
}

impl AttributeSubClass for PointerAttr {
    fn is_of_type(attr: &Attribute) -> bool {
        match attr {
            Attribute::Ptr(_) => true,
            _ => false,
        }
    }

    fn downcast_to_self<'a>(attr: &'a Attribute) -> Option<&'a Self> {
        match attr {
            Attribute::Ptr(attr) => Some(attr),
            _ => None,
        }
    }

    fn get_typename() -> &'static str {
        "Pointer"
    }

    fn get_type(&self) -> Option<&Type> {
        Some(&self.ty)
    }
}

impl IRPrintableObject for PointerAttr {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        write!(printer.os(), "#ptr<0x{:x}>: ", self.val())?;
        printer.print(&self.ty)
    }
}

impl IRParsableObject for PointerAttr {
    fn parse(parser: &mut crate::ir_parser::IRParser) -> Option<Self> {
        parser.consume_sym_or_error(TokenValue::sym_hash())?;
        parser.consume_identifier_val_or_error("ptr")?;
        parser.consume_sym_or_error(TokenValue::sym_lt())?;

        let ptr_val = parser.consume_int_or_error()?.get_int().unwrap() as usize;
        parser.consume_sym_or_error(TokenValue::sym_gt())?;
        parser.consume_sym_or_error(TokenValue::sym_colon())?;
        let ty = Type::parse(parser)?;
        Some(PointerAttr { val: ptr_val, ty })
    }
}

// Represent any constant pointer value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RegisterAttr {
    name: String,
    vreg_idx: Option<usize>,
}

impl RegisterAttr {
    /// Build a new real register.
    pub fn new(name: String) -> Attribute {
        Attribute::Register(Self {
            name,
            vreg_idx: None,
        })
    }

    /// Build a new real register.
    pub fn new_vreg(name: String, idx: usize) -> Attribute {
        Attribute::Register(Self {
            name,
            vreg_idx: Some(idx),
        })
    }

    /// Returns the register name.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the register idx if it's a virtual register.
    /// Or returns none.
    pub fn vreg_idx(&self) -> Option<usize> {
        self.vreg_idx
    }

    /// Return true if this is a real register.
    pub fn is_real(&self) -> bool {
        self.vreg_idx.is_none()
    }

    /// Return true if this is a virtual register.
    pub fn is_virtual(&self) -> bool {
        self.vreg_idx.is_some()
    }
}

impl AttributeSubClass for RegisterAttr {
    fn is_of_type(attr: &Attribute) -> bool {
        match attr {
            Attribute::Register(_) => true,
            _ => false,
        }
    }

    fn downcast_to_self<'a>(attr: &'a Attribute) -> Option<&'a Self> {
        match attr {
            Attribute::Register(attr) => Some(attr),
            _ => None,
        }
    }

    fn get_typename() -> &'static str {
        "Register"
    }

    fn get_type(&self) -> Option<&Type> {
        None
    }
}

impl IRPrintableObject for RegisterAttr {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        write!(printer.os(), "#reg<{}", self.name())?;
        if let Some(idx) = self.vreg_idx() {
            write!(printer.os(), ":{}", idx)?;
        }
        write!(printer.os(), ">")
    }
}

impl IRParsableObject for RegisterAttr {
    fn parse(parser: &mut crate::ir_parser::IRParser) -> Option<Self> {
        parser.consume_sym_or_error(TokenValue::sym_hash())?;
        parser.consume_identifier_val_or_error("reg")?;
        parser.consume_sym_or_error(TokenValue::sym_lt())?;

        let name = parser
            .consume_identifier_or_error()?
            .take_identifier()
            .unwrap();
        let vreg_idx = if parser.try_consume_sym(TokenValue::sym_colon()) {
            Some(parser.consume_int_or_error()?.get_int().unwrap() as usize)
        } else {
            None
        };

        parser.consume_sym_or_error(TokenValue::sym_gt())?;
        Some(RegisterAttr { name, vreg_idx })
    }
}

// Represent all possible Attributes in the IR.
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Attribute {
    Int(IntegerAttr),
    Float(FloatAttr),
    Str(StringAttr),
    Array(ArrayAttr),
    Dict(DictAttr),
    Type(TypeAttr),
    Ptr(PointerAttr),
    Register(RegisterAttr),
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

    pub fn is_type(&self) -> bool {
        match self {
            Attribute::Type(_) => true,
            _ => false,
        }
    }

    pub fn is_ptr(&self) -> bool {
        match self {
            Attribute::Ptr(_) => true,
            _ => false,
        }
    }

    /// Returns the type of the attribute.
    /// Or None if the type isn't typed
    pub fn get_type(&self) -> Option<&Type> {
        match self {
            Attribute::Int(attr) => attr.get_type(),
            Attribute::Float(attr) => attr.get_type(),
            Attribute::Str(attr) => attr.get_type(),
            Attribute::Array(attr) => attr.get_type(),
            Attribute::Dict(attr) => attr.get_type(),
            Attribute::Type(attr) => attr.get_type(),
            Attribute::Ptr(attr) => attr.get_type(),
            Attribute::Register(attr) => attr.get_type(),
        }
    }

    // Returns true if `self` is of type T.
    pub fn isa<T: AttributeSubClass>(&self) -> bool {
        T::is_of_type(self)
    }

    // Try to cast self to T.
    // Returns None if it's not a T.
    pub fn cast<T: AttributeSubClass>(&self) -> Option<&T> {
        T::downcast_to_self(self)
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
            Attribute::Type(attr) => attr.print(printer),
            Attribute::Ptr(attr) => attr.print(printer),
            Attribute::Register(attr) => attr.print(printer),
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
        } else if token_is_start_of_type(parser.peek_token()) {
            Some(Attribute::Type(TypeAttr::parse(parser)?))
        } else if parser.next_token_is_sym(TokenValue::sym_hash()) {
            // Get the attr name and restore the state.
            let st = parser.save_parser_state();
            parser.consume_sym_or_error(TokenValue::sym_hash());
            let attr_id = parser
                .consume_identifier_or_error()?
                .get_identifier()
                .unwrap()
                .to_owned();
            parser.restore_parser_state(st);

            if attr_id == "ptr" {
                Some(Attribute::Ptr(PointerAttr::parse(parser)?))
            } else if attr_id == "reg" {
                Some(Attribute::Register(RegisterAttr::parse(parser)?))
            } else {
                let err_tok = parser.peek_token().clone();
                parser.emit_bad_token_error(&err_tok, format!("attribute value"));
                None
            }
        } else {
            let err_tok = parser.peek_token().clone();
            parser.emit_bad_token_error(&err_tok, format!("attribute value"));
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::types::{ArrayType, FloatType, IntegerType, PointerType};

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

    #[test]
    fn test_type_print_parse() {
        let i32_ty = IntegerType::new(32, None);
        let attr = TypeAttr::new(i32_ty.clone());
        assert_eq!(attr.to_string_repr(), "i32");
        assert_eq!(attr, Attribute::from_string_repr("i32".to_owned()));
        check_same_repr("i32");

        let array_ty = ArrayType::new(i32_ty.clone(), 42);
        let attr = TypeAttr::new(array_ty);
        assert_eq!(attr.to_string_repr(), "array<i32; 42>");
        assert_eq!(
            attr,
            Attribute::from_string_repr("array<i32; 42>".to_owned())
        );
        check_same_repr("array<i32; 42>");
    }

    #[test]
    fn test_ptr_print_parse() {
        let ptr_ty = PointerType::new(IntegerType::new(32, None));
        let attr = PointerAttr::new(43, ptr_ty);
        assert_eq!(attr.to_string_repr(), "#ptr<0x2b>: ptr<i32>");
        assert_eq!(
            attr,
            Attribute::from_string_repr("#ptr<0x2b>: ptr<i32>".to_owned())
        );
        check_same_repr("#ptr<0x33fb23>: ptr<i32>");
    }

    #[test]
    fn test_reg_print_parse() {
        let attr = RegisterAttr::new("sp".to_owned());
        assert_eq!(attr.to_string_repr(), "#reg<sp>");
        assert_eq!(attr, Attribute::from_string_repr("#reg<sp>".to_owned()));
        check_same_repr("#reg<mylong_reg_name>");

        let attr = RegisterAttr::new_vreg("mk_fp".to_owned(), 6);
        assert_eq!(attr.to_string_repr(), "#reg<mk_fp:6>");
        assert_eq!(
            attr,
            Attribute::from_string_repr("#reg<mk_fp:6>".to_owned())
        );
        check_same_repr("#reg<my_virtual_reg:345>");
    }
}

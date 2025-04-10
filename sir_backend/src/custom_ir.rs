use parse::lexer::TokenValue;
use parse::parser::Parser;
use sir_core::{
    attributes::{Attribute, IntegerAttr, RegisterAttr},
    ir_parser::IRParser,
    ir_printer::IRPrinter,
    types::IntegerType,
};

// Parse a register in it's short form (without the #reg prefix).
pub fn parse_short_form_register(parser: &mut IRParser) -> Option<Attribute> {
    let name = parser
        .consume_identifier_or_error()?
        .take_identifier()
        .unwrap();
    if !parser.try_consume_sym(TokenValue::sym_colon()) {
        return Some(RegisterAttr::new(name));
    }

    let vreg_idx = parser.consume_int_or_error()?.get_int().unwrap() as usize;
    Some(RegisterAttr::new_vreg(name, vreg_idx))
}

// Parse a register or immediate in it's short form (without the #reg prefix).
pub fn parse_short_form_register_or_immediate(parser: &mut IRParser) -> Option<Attribute> {
    // immediate: '#' <int>
    if parser.try_consume_sym(TokenValue::sym_hash()) {
        let val = parser.consume_int_or_error()?.get_int().unwrap();
        return Some(IntegerAttr::new(val, IntegerType::index_type()));
    }

    parse_short_form_register(parser)
}

// Print a register in it's short form (without the #reg prefix).
pub fn print_short_form_register(
    attr: &RegisterAttr,
    printer: &mut IRPrinter,
) -> Result<(), std::io::Error> {
    if let Some(idx) = attr.vreg_idx() {
        write!(printer.os(), "{}:{}", attr.name(), idx)
    } else {
        write!(printer.os(), "{}", attr.name())
    }
}

// Print a register or immediate in it's short form (without the #reg prefix).
pub fn print_short_form_register_or_immediate(
    attr: &Attribute,
    printer: &mut IRPrinter,
) -> Result<(), std::io::Error> {
    if let Some(int_attr) = attr.cast::<IntegerAttr>() {
        return write!(printer.os(), "#{}", int_attr.raw_val());
    }

    let reg_attr = attr.cast::<RegisterAttr>().unwrap();
    print_short_form_register(reg_attr, printer)
}

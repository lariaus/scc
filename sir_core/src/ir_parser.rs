use std::collections::HashSet;

use diagnostics::{
    diagnostics::{emit_error, CompilerDiagnostics, CompilerDiagnosticsEmitter, CompilerInputs},
    result::CompilerResult,
};
use iostreams::{
    location::Location, source_stream::SourceStream, source_streams_set::SourceStreamsSet,
};
use parse::{
    lexer::{Lexer, TokenValue},
    parser::{self, Parser},
};
use utils::scoped_map::ScopedMap;

use crate::{
    attributes::{Attribute, DictAttr, StringAttr},
    ir_context::IRContext,
    ir_data::{BlockID, OperationID, ValueID},
    ir_printer::IRPrintableObject,
    operation::OperationImpl,
    operation_type::{OperationTypeRef, OperationTypeUID},
    types::Type,
};

// Options for the parser
pub struct IRParserOpts {
    pub accept_unregistred_ops: bool,
    pub only_use_generic_form: bool,
}

impl IRParserOpts {
    // Build the default options for the parser.
    pub fn new() -> Self {
        Self {
            accept_unregistred_ops: true,
            only_use_generic_form: false,
        }
    }

    pub fn accept_unregistred_ops(&self) -> bool {
        self.accept_unregistred_ops
    }

    // If set to true, the parser will accept unregistred ops.
    pub fn set_accept_unregistred_ops(&mut self, accept_unregistred_ops: bool) {
        self.accept_unregistred_ops = accept_unregistred_ops;
    }
}

// Parser for the
pub struct IRParser {
    opts: IRParserOpts,
    lex: Lexer,
    diagnostics: CompilerDiagnostics,
    values_map: ScopedMap<String, ValueID>,
}

impl IRParser {
    // Create a new parser.
    pub fn new(opts: IRParserOpts, ifs: Box<dyn SourceStream>) -> Self {
        let mut res = Self {
            opts,
            lex: Lexer::new(ifs),
            diagnostics: CompilerDiagnostics::new(),
            values_map: ScopedMap::new(),
        };
        res.values_map.open_scope();
        res._setup_lexer();
        res
    }

    pub fn opts(&self) -> &IRParserOpts {
        &self.opts
    }

    // Setup the lexer properly;
    fn _setup_lexer(&mut self) {
        self.lex.set_skip_error_tokens(true);
        self.lex.set_support_c_inline_comment(true);
        self.lex.set_support_c_multiline_comment(true);
        self.lex.set_skip_comment_tokens(true);
        self.lex.set_support_single_quote_strings(true);
        self.lex.set_support_double_quote_strings(true);
        self.lex.set_decode_tok_strings(true);
        self.lex.set_single_quote_literal_is_char(true);

        self._setup_lexer_symbols();
    }

    // Add the symbols needed by the parser.
    fn _setup_lexer_symbols(&mut self) {
        self.lex.add_symbol_val(TokenValue::sym_lt());
        self.lex.add_symbol_val(TokenValue::sym_gt());
        self.lex.add_symbol_val(TokenValue::sym_lbracket());
        self.lex.add_symbol_val(TokenValue::sym_rbracket());
        self.lex.add_symbol_val(TokenValue::sym_comma());
        self.lex.add_symbol_val(TokenValue::sym_semi());
        self.lex.add_symbol_val(TokenValue::sym_colon());
        self.lex.add_symbol_val(TokenValue::sym_lcbracket());
        self.lex.add_symbol_val(TokenValue::sym_rcbracket());
        self.lex.add_symbol_val(TokenValue::sym_assign());
        self.lex.add_symbol_val(TokenValue::sym_percent());
        self.lex.add_symbol_val(TokenValue::sym_lparen());
        self.lex.add_symbol_val(TokenValue::sym_rparen());
        self.lex.add_symbol_val(TokenValue::sym_deref());
        self.lex.add_symbol_val(TokenValue::sym_xor());
        self.lex.add_symbol_val(TokenValue::sym_dot());
        self.lex.add_symbol_val(TokenValue::sym_at());
    }

    // Call every time you start parsing a block.
    // This is called automatically for you when using helper methods like `make_block_from_state`.
    pub fn start_parsing_block(&mut self) {
        self.values_map.open_scope();
    }

    // Call every time you're finished parsing a block.
    // This is called automatically for you when using helper methods like `make_block_from_state`.
    pub fn end_parsing_block(&mut self) {
        self.values_map.close_scope();
    }

    // Helper function to builder an operation from its state.
    pub fn make_op_from_state(
        &mut self,
        state: OperationParserState,
        ctx: &mut IRContext,
    ) -> Option<OperationID> {
        let loc = Location::join(state.beg_loc.unwrap(), self.get_last_token_loc());
        let op_infos = if let Some(raw_opname) = state.raw_opname {
            // If we can't find the opname, create an unknown op.
            match ctx.find_op_type_infos_from_opname(&raw_opname) {
                Some(infos) => OperationTypeRef::Registered(infos.uid()),
                None => {
                    if !self.opts.accept_unregistred_ops {
                        emit_error(
                            self,
                            &loc,
                            format!("No registered op found with opname `{}`", &raw_opname),
                        );
                    }
                    OperationTypeRef::Unknown(raw_opname)
                }
            }
        } else if let Some(op_type_uid) = state.op_type_uid {
            OperationTypeRef::Registered(op_type_uid)
        } else {
            panic!("Missing `raw_opname` or `op_type_uid` value to find the op identity");
        };

        // Check the inputs
        let inputs_names = state.inputs_names.unwrap_or(vec![]);
        let inputs_types = state.inputs_types.unwrap_or(vec![]);
        let mut inputs_vals = vec![];
        if inputs_names.len() != inputs_types.len() {
            emit_error(
                self,
                &loc,
                format!(
                    "Op has {} inputs but type signature has {} inputs",
                    inputs_names.len(),
                    inputs_types.len()
                ),
            );
            return None;
        }
        for (idx, (in_name, in_sig_ty)) in inputs_names.iter().zip(&inputs_types).enumerate() {
            let in_val = match self.values_map.get(in_name) {
                Some(uid) => ctx.get_value_data(*uid),
                None => {
                    emit_error(
                        self,
                        &loc,
                        format!("Op input #{} (%{}) isn't defined", idx, in_name),
                    );
                    return None;
                }
            };
            inputs_vals.push(in_val.as_id());
            let in_val_ty = in_val.get_type();
            if *in_val_ty != *in_sig_ty {
                emit_error(
                    self,
                    &loc,
                    format!(
                        "Op input #{} (%{}) has type {}, but op signature expects type {}",
                        idx,
                        in_name,
                        in_val_ty.to_string_repr(),
                        in_sig_ty.to_string_repr()
                    ),
                );
                return None;
            }
        }

        // Checks the outputs.
        let outputs_names = state.outputs_names.unwrap_or(vec![]);
        let outputs_types = state.outputs_types.unwrap_or(vec![]);
        if outputs_names.len() != outputs_names.len() {
            emit_error(
                self,
                &loc,
                format!(
                    "Op has {} outputs but type signature has {} outputs",
                    outputs_types.len(),
                    outputs_types.len()
                ),
            );
            return None;
        }
        let mut names_set = HashSet::new();
        for name in &outputs_names {
            if !names_set.insert(name) {
                emit_error(
                    self,
                    &loc,
                    format!("Op has multiple outputs with the same name (%{})", name),
                );
                return None;
            }
            if self.values_map.contains(name) {
                emit_error(
                    self,
                    &loc,
                    format!("Op redefines an existing output value (%{})", name),
                );
                return None;
            }
        }

        let attrs_dict = if let Some(attrs_vals) = state.attrs_vals {
            DictAttr::new(attrs_vals)
        } else {
            state.attrs_dict.unwrap_or(DictAttr::empty())
        };

        let blocks = state.blocks.unwrap_or(vec![]);

        // Now we have everything to build the op.
        let op_id = ctx._make_operation(
            loc,
            op_infos,
            inputs_vals,
            outputs_types,
            attrs_dict,
            blocks,
        );

        // Add the outputs to the map.
        let op = ctx.get_generic_operation(op_id);
        for (out_name, out_val) in outputs_names.into_iter().zip(op.get_outputs()) {
            self.values_map.insert(out_name, out_val.as_id());
        }

        Some(op_id)
    }

    // Helper function to builder an operation from its state.
    pub fn make_block_from_state(
        &mut self,
        state: BlockParserState,
        ctx: &mut IRContext,
        end_block_sym: TokenValue,
    ) -> Option<BlockID> {
        let loc = state.loc.unwrap();

        // Check the operands.
        let operands_names = state.operands_names.unwrap_or(vec![]);
        let operands_types = state.operands_types.unwrap_or(vec![]);
        if operands_names.len() != operands_types.len() {
            emit_error(
                self,
                &loc,
                format!(
                    "Block has {} operands but type signature has {} operands",
                    operands_names.len(),
                    operands_types.len()
                ),
            );
            return None;
        }
        let mut names_set = HashSet::new();
        for name in &operands_names {
            if !names_set.insert(name) {
                emit_error(
                    self,
                    &loc,
                    format!("Block has multiple operands with the same name (%{})", name),
                );
                return None;
            }
            if self.values_map.contains(name) {
                emit_error(
                    self,
                    &loc,
                    format!("Block operand redefines an existing value (%{})", name),
                );
                return None;
            }
        }

        self.start_parsing_block();

        // Now we have everything to build the block.
        let block_id = ctx._make_block(loc, operands_types, vec![]);

        // Add the operands to the map.
        for (in_name, in_val) in operands_names
            .into_iter()
            .zip(ctx.get_block(block_id).get_operands())
        {
            self.values_map.insert(in_name, in_val.as_id());
        }

        // Now parse the rest of the block.
        while !self.next_token_is_sym(end_block_sym.clone()) {
            let op = OperationID::parse_with_context(self, ctx)?;
            ctx.move_op_at_end_of_block(block_id, op);
        }
        self.consume_sym_or_error(end_block_sym)?;

        self.end_parsing_block();

        Some(block_id)
    }

    // Helper method to parse a value ref (eg `%arg0`).
    pub fn parse_value_ref(&mut self) -> Option<String> {
        self.consume_sym_or_error(TokenValue::sym_percent())?;
        if let Some(int_val) = self.try_consume_int() {
            // An int might also be used as an identifier.
            Some(int_val.get_int().unwrap().to_string())
        } else {
            self.consume_identifier_or_error()?.take_identifier()
        }
    }

    // Parse an return an object.
    // In case of error, returns None.
    // Specific error messages are passed through diagnostics.
    pub fn parse<T: IRParsableObject>(&mut self) -> Option<T> {
        T::parse(self)
    }

    // Wrapper around `parse`, but ensures the whole stream is used.
    // Should be called by users of IRParser.
    pub fn parse_all<T: IRParsableObject>(mut self) -> CompilerResult<T> {
        let res = self.parse();
        if !res.is_none() {
            self.consume_eof_or_error();
        }
        CompilerResult::make(self.diagnostics, res)
    }

    // Parse identifier separated with `.`
    pub fn parse_scoped_identifier(&mut self) -> Option<(String, Location)> {
        let mut res = String::new();
        let beg_loc = self.get_next_token_loc();
        let mut end_loc;

        loop {
            end_loc = self.get_next_token_loc();
            res.push_str(
                self.consume_identifier_or_error()?
                    .get_identifier()
                    .unwrap(),
            );
            if !self.try_consume_sym(TokenValue::sym_dot()) {
                break;
            }
            res.push('.');
        }

        Some((res, Location::join(beg_loc, end_loc)))
    }

    // Parse an return an object.
    // In case of error, returns None.
    // Specific error messages are passed through diagnostics.
    pub fn parse_with_context<T: IRParsableObjectWithContext>(
        &mut self,
        ctx: &mut IRContext,
    ) -> Option<T> {
        T::parse_with_context(self, ctx)
    }

    // Wrapper around `parse_with_context`, but ensures the whole stream is used.
    // Should be called by users of IRParser.
    pub fn parse_all_with_context<T: IRParsableObjectWithContext>(
        mut self,
        ctx: &mut IRContext,
    ) -> CompilerResult<T> {
        let res = self.parse_with_context(ctx);
        if !res.is_none() {
            self.consume_eof_or_error();
        }
        CompilerResult::make(self.diagnostics, res)
    }
}

impl parser::Parser for IRParser {
    fn get_lexer_mut(&mut self) -> &mut Lexer {
        &mut self.lex
    }
}

impl CompilerDiagnosticsEmitter for IRParser {
    fn get_diagnostics_emitter_mut(&mut self) -> &mut CompilerDiagnostics {
        &mut self.diagnostics
    }

    fn get_diagnostics_emitter_name(&self) -> &str {
        "SIR Parser"
    }
}

// Implement this trait for an object to be parsed by the IR
pub trait IRParsableObject: Sized {
    // Parse object from the IR
    fn parse(parser: &mut IRParser) -> Option<Self>;

    // Returns the object from the string repr.
    // Panic in case of error.
    fn from_string_repr(ir: String) -> Self {
        // Prepare stream.
        let mut set = SourceStreamsSet::new();
        let rid = set.add_raw_source_string(ir);
        let ss = set.open_stream(rid);

        // Parse the ir.
        let opts = IRParserOpts::new();
        let parser = IRParser::new(opts, ss);
        let res = parser.parse_all::<Self>();
        res.resolve(CompilerInputs::Sources(&set))
            .expect("Parsing failure")
    }
}

// Implement this trait for an object to be parsed by the IR using an IRContext.
pub trait IRParsableObjectWithContext: Sized {
    // Parse object from the IR
    fn parse_with_context(parser: &mut IRParser, ctx: &mut IRContext) -> Option<Self>;

    // Returns the object from the string repr.
    // Panic in case of error.
    fn from_string_repr_with_context(ir: String, ctx: &mut IRContext) -> Self {
        // Prepare stream.
        let mut set = SourceStreamsSet::new();
        let rid = set.add_raw_source_string(ir);
        let ss = set.open_stream(rid);

        // Parse the ir.
        let opts = IRParserOpts::new();
        let parser = IRParser::new(opts, ss);
        let res = parser.parse_all_with_context::<Self>(ctx);
        res.resolve(CompilerInputs::Sources(&set))
            .expect("Parsing failure")
    }
}

// Helper class to parse and build an operation
pub struct OperationParserState {
    beg_loc: Option<Location>,
    raw_opname: Option<String>,
    op_type_uid: Option<OperationTypeUID>,
    inputs_names: Option<Vec<String>>,
    outputs_names: Option<Vec<String>>,
    inputs_types: Option<Vec<Type>>,
    outputs_types: Option<Vec<Type>>,
    attrs_dict: Option<DictAttr>,
    attrs_vals: Option<Vec<(Attribute, Attribute)>>,
    blocks: Option<Vec<BlockID>>,
}

impl OperationParserState {
    // Create a new builder without any informations.
    pub fn new() -> Self {
        Self {
            beg_loc: None,
            raw_opname: None,
            op_type_uid: None,
            inputs_names: None,
            outputs_names: None,
            inputs_types: None,
            outputs_types: None,
            attrs_dict: None,
            attrs_vals: None,
            blocks: None,
        }
    }

    // Set the beginning location for the current op in the IR.
    pub fn set_beg_loc(&mut self, beg_loc: Location) {
        assert!(self.beg_loc.is_none());
        self.beg_loc = Some(beg_loc);
    }

    // Get the operation opname.
    pub fn get_raw_opname(&self) -> Option<&String> {
        self.raw_opname.as_ref()
    }

    // Set the operation opname.
    pub fn set_raw_opname(&mut self, raw_opname: String) {
        assert!(self.raw_opname.is_none());
        assert!(self.op_type_uid.is_none());
        self.raw_opname = Some(raw_opname);
    }

    // Set the OperationTypeUID.
    pub fn set_op_type_uid(&mut self, op_type_uid: OperationTypeUID) {
        assert!(self.raw_opname.is_none());
        assert!(self.op_type_uid.is_none());
        self.op_type_uid = Some(op_type_uid);
    }

    // Set the operation inputs types.
    pub fn set_inputs_names(&mut self, inputs_names: Vec<String>) {
        assert!(self.inputs_names.is_none());
        self.inputs_names = Some(inputs_names);
    }

    // Set the operation outputs types.
    pub fn set_outputs_names(&mut self, outputs_names: Vec<String>) {
        assert!(self.outputs_names.is_none());
        self.outputs_names = Some(outputs_names);
    }

    // Set the operation inputs types.
    pub fn set_inputs_types(&mut self, inputs_types: Vec<Type>) {
        assert!(self.inputs_types.is_none());
        self.inputs_types = Some(inputs_types);
    }

    // Set the operation outputs types.
    pub fn set_outputs_types(&mut self, outputs_types: Vec<Type>) {
        assert!(self.outputs_types.is_none());
        self.outputs_types = Some(outputs_types);
    }

    // Set the operation attributes dict.
    pub fn set_attrs_dict(&mut self, attrs_dict: DictAttr) {
        assert!(self.attrs_dict.is_none());
        assert!(self.attrs_vals.is_none());
        self.attrs_dict = Some(attrs_dict);
    }

    // Set an attribute.
    pub fn set_attr(&mut self, key: &str, val: Attribute) {
        assert!(self.attrs_dict.is_none());
        if self.attrs_vals.is_none() {
            self.attrs_vals = Some(Vec::new());
        }
        self.attrs_vals
            .as_mut()
            .unwrap()
            .push((StringAttr::new(key.to_owned()), val));
    }

    // Set the operation blocks.
    pub fn set_blocks(&mut self, blocks: Vec<BlockID>) {
        assert!(self.blocks.is_none());
        self.blocks = Some(blocks);
    }
}

// Helper class to parse and build a block
pub struct BlockParserState {
    loc: Option<Location>,
    operands_names: Option<Vec<String>>,
    operands_types: Option<Vec<Type>>,
}

impl BlockParserState {
    // Create a new builder without any informations.
    pub fn new() -> Self {
        Self {
            loc: None,
            operands_names: None,
            operands_types: None,
        }
    }

    // Set the location for the current op in the IR.
    pub fn set_loc(&mut self, loc: Location) {
        assert!(self.loc.is_none());
        self.loc = Some(loc);
    }

    // Set the operation inputs types.
    pub fn set_operands_names(&mut self, operands_names: Vec<String>) {
        assert!(self.operands_names.is_none());
        self.operands_names = Some(operands_names);
    }

    // Set the operation inputs types.
    pub fn set_operands_types(&mut self, operands_types: Vec<Type>) {
        assert!(self.operands_types.is_none());
        self.operands_types = Some(operands_types);
    }
}

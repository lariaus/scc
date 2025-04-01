use diagnostics::{CompilerDiagnostics, CompilerDiagnosticsEmitter};
use iostreams::{location::Location, source_stream::SourceStream};
use parse::{
    lexer::{Lexer, TokenValue},
    parser::Parser,
};

use crate::{
    ast::{
        ASTNode, ASTNodeBinopExpr, ASTNodeBinopExprKind, ASTNodeBlockStatement, ASTNodeCharLiteral,
        ASTNodeDeclsList, ASTNodeFloatLiteral, ASTNodeFunctionDecl, ASTNodeImpl, ASTNodeIntLiteral,
        ASTNodeLabelExpr, ASTNodeLabelType, ASTNodeReturnStatement, ASTNodeStringLiteral,
        ASTNodeVarDeclStatement,
    },
    ast_types::{self, IntegerSignedness},
};

pub struct CParser {
    lexer: Lexer,
    diagnostics: CompilerDiagnostics,
}

// All C Keywords.
//
const KW_AUTO: &'static str = "auto";
const V_KW_AUTO: TokenValue = TokenValue::Keyword(KW_AUTO);
const KW_BREAK: &'static str = "break";
const V_KW_BREAK: TokenValue = TokenValue::Keyword(KW_BREAK);
const KW_CASE: &'static str = "case";
const V_KW_CASE: TokenValue = TokenValue::Keyword(KW_CASE);
const KW_CHAR: &'static str = "char";
const V_KW_CHAR: TokenValue = TokenValue::Keyword(KW_CHAR);
const KW_CONST: &'static str = "const";
const V_KW_CONST: TokenValue = TokenValue::Keyword(KW_CONST);
const KW_CONTINUE: &'static str = "continue";
const V_KW_CONTINUE: TokenValue = TokenValue::Keyword(KW_CONTINUE);
const KW_DEFAULT: &'static str = "default";
const V_KW_DEFAULT: TokenValue = TokenValue::Keyword(KW_DEFAULT);
const KW_DO: &'static str = "do";
const V_KW_DO: TokenValue = TokenValue::Keyword(KW_DO);
const KW_DOUBLE: &'static str = "double";
const V_KW_DOUBLE: TokenValue = TokenValue::Keyword(KW_DOUBLE);
const KW_ELSE: &'static str = "else";
const V_KW_ELSE: TokenValue = TokenValue::Keyword(KW_ELSE);
const KW_ENUM: &'static str = "enum";
const V_KW_ENUM: TokenValue = TokenValue::Keyword(KW_ENUM);
const KW_EXTERN: &'static str = "extern";
const V_KW_EXTERN: TokenValue = TokenValue::Keyword(KW_EXTERN);
const KW_FLOAT: &'static str = "float";
const V_KW_FLOAT: TokenValue = TokenValue::Keyword(KW_FLOAT);
const KW_FOR: &'static str = "for";
const V_KW_FOR: TokenValue = TokenValue::Keyword(KW_FOR);
const KW_GOTO: &'static str = "goto";
const V_KW_GOTO: TokenValue = TokenValue::Keyword(KW_GOTO);
const KW_IF: &'static str = "if";
const V_KW_IF: TokenValue = TokenValue::Keyword(KW_IF);
const KW_INT: &'static str = "int";
const V_KW_INT: TokenValue = TokenValue::Keyword(KW_INT);
const KW_LONG: &'static str = "long";
const V_KW_LONG: TokenValue = TokenValue::Keyword(KW_LONG);
const KW_REGISTER: &'static str = "register";
const V_KW_REGISTER: TokenValue = TokenValue::Keyword(KW_REGISTER);
const KW_RETURN: &'static str = "return";
const V_KW_RETURN: TokenValue = TokenValue::Keyword(KW_RETURN);
const KW_SHORT: &'static str = "short";
const V_KW_SHORT: TokenValue = TokenValue::Keyword(KW_SHORT);
const KW_SIGNED: &'static str = "signed";
const V_KW_SIGNED: TokenValue = TokenValue::Keyword(KW_SIGNED);
const KW_SIZEOF: &'static str = "sizeof";
const V_KW_SIZEOF: TokenValue = TokenValue::Keyword(KW_SIZEOF);
const KW_STATIC: &'static str = "static";
const V_KW_STATIC: TokenValue = TokenValue::Keyword(KW_STATIC);
const KW_STRUCT: &'static str = "struct";
const V_KW_STRUCT: TokenValue = TokenValue::Keyword(KW_STRUCT);
const KW_SWITCH: &'static str = "switch";
const V_KW_SWITCH: TokenValue = TokenValue::Keyword(KW_SWITCH);
const KW_TYPEDEF: &'static str = "typedef";
const V_KW_TYPEDEF: TokenValue = TokenValue::Keyword(KW_TYPEDEF);
const KW_UNION: &'static str = "union";
const V_KW_UNION: TokenValue = TokenValue::Keyword(KW_UNION);
const KW_UNSIGNED: &'static str = "unsigned";
const V_KW_UNSIGNED: TokenValue = TokenValue::Keyword(KW_UNSIGNED);
const KW_VOID: &'static str = "void";
const V_KW_VOID: TokenValue = TokenValue::Keyword(KW_VOID);
const KW_VOLATILE: &'static str = "volatile";
const V_KW_VOLATILE: TokenValue = TokenValue::Keyword(KW_VOLATILE);
const KW_WHILE: &'static str = "while";
const V_KW_WHILE: TokenValue = TokenValue::Keyword(KW_WHILE);

fn add_c_keywords(lex: &mut Lexer) {
    lex.add_keyword_val(V_KW_AUTO);
    lex.add_keyword_val(V_KW_BREAK);
    lex.add_keyword_val(V_KW_CASE);
    lex.add_keyword_val(V_KW_CHAR);
    lex.add_keyword_val(V_KW_CONST);
    lex.add_keyword_val(V_KW_CONTINUE);
    lex.add_keyword_val(V_KW_DEFAULT);
    lex.add_keyword_val(V_KW_DO);
    lex.add_keyword_val(V_KW_DOUBLE);
    lex.add_keyword_val(V_KW_ELSE);
    lex.add_keyword_val(V_KW_ENUM);
    lex.add_keyword_val(V_KW_EXTERN);
    lex.add_keyword_val(V_KW_FLOAT);
    lex.add_keyword_val(V_KW_FOR);
    lex.add_keyword_val(V_KW_GOTO);
    lex.add_keyword_val(V_KW_IF);
    lex.add_keyword_val(V_KW_INT);
    lex.add_keyword_val(V_KW_LONG);
    lex.add_keyword_val(V_KW_REGISTER);
    lex.add_keyword_val(V_KW_RETURN);
    lex.add_keyword_val(V_KW_SHORT);
    lex.add_keyword_val(V_KW_SIGNED);
    lex.add_keyword_val(V_KW_SIZEOF);
    lex.add_keyword_val(V_KW_STATIC);
    lex.add_keyword_val(V_KW_STRUCT);
    lex.add_keyword_val(V_KW_SWITCH);
    lex.add_keyword_val(V_KW_TYPEDEF);
    lex.add_keyword_val(V_KW_UNION);
    lex.add_keyword_val(V_KW_UNSIGNED);
    lex.add_keyword_val(V_KW_VOID);
    lex.add_keyword_val(V_KW_VOLATILE);
    lex.add_keyword_val(V_KW_WHILE);
}

impl CompilerDiagnosticsEmitter for CParser {
    fn get_diagnostics_emitter_mut(&mut self) -> &mut CompilerDiagnostics {
        &mut self.diagnostics
    }

    fn get_diagnostics_emitter_name(&self) -> &str {
        "Parser"
    }
}

impl Parser for CParser {
    fn get_lexer_mut(&mut self) -> &mut Lexer {
        &mut self.lexer
    }
}

impl CParser {
    // Create a new parser for the input stream `ifs`
    pub fn new(ifs: Box<dyn SourceStream>) -> Self {
        let mut lexer = Lexer::new(ifs);
        // Setup the options for the C lexer.
        lexer.set_skip_error_tokens(true);
        lexer.set_support_c_inline_comment(true);
        lexer.set_support_c_multiline_comment(true);
        lexer.set_skip_comment_tokens(true);
        lexer.set_support_single_quote_strings(true);
        lexer.set_support_double_quote_strings(true);
        lexer.set_decode_tok_strings(true);
        lexer.set_single_quote_literal_is_char(true);

        // Setup the C operators / synbols.
        lexer.add_symbol_val(TokenValue::sym_plus());
        lexer.add_symbol_val(TokenValue::sym_minus());
        lexer.add_symbol_val(TokenValue::sym_multiply());
        lexer.add_symbol_val(TokenValue::sym_divide());
        lexer.add_symbol_val(TokenValue::sym_modulo());
        lexer.add_symbol_val(TokenValue::sym_lparen());
        lexer.add_symbol_val(TokenValue::sym_rparen());
        lexer.add_symbol_val(TokenValue::sym_lbracket());
        lexer.add_symbol_val(TokenValue::sym_rbracket());
        lexer.add_symbol_val(TokenValue::sym_lcbracket());
        lexer.add_symbol_val(TokenValue::sym_rcbracket());
        lexer.add_symbol_val(TokenValue::sym_semi());
        lexer.add_symbol_val(TokenValue::sym_comma());
        lexer.add_symbol_val(TokenValue::sym_assign());

        // Setup the C keywords.
        add_c_keywords(&mut lexer);

        // Setups diagnostics.
        let mut diagnostics = CompilerDiagnostics::new();
        // TODO: We should fix parser instead of using this.
        diagnostics.enable_log_only_one_error();

        Self { lexer, diagnostics }
    }

    // Parse the whole file and return the AST.
    pub fn parse(&mut self) -> ASTNode {
        let root = self._r_root();
        self.consume_eof_or_error();

        // Move the lexer diagnostics to the parser ones.
        let lex_diagnostics = self.lexer.take_diagnostics();
        self.extend_diagnostics(lex_diagnostics);

        root
    }

    // Entry point of the file
    fn _r_root(&mut self) -> ASTNode {
        self._r_decls_list()
    }

    // Base function to parse a decl.
    // TODO: This should always return a decl
    fn _r_decl(&mut self) -> Option<ASTNode> {
        self._r_function_decl()
    }

    // Base function to parse a decls list
    fn _r_decls_list(&mut self) -> ASTNode {
        let mut decls = Vec::new();
        while !self.next_token_is_eof() {
            // TODO: We shouldn't stop for an error, just keep going.
            let decl = match self._r_decl() {
                Some(node) => node,
                None => break,
            };
            decls.push(decl);
        }

        let loc = if decls.is_empty() {
            self.get_next_token_loc()
        } else {
            Location::join(
                decls.first().unwrap().get_loc(),
                decls.last().unwrap().get_loc(),
            )
        };

        ASTNodeDeclsList::new(loc, decls)
    }

    // Base function to parse a decl
    // <type> <id> '(' (<type> <id>)* ','? ')' (';'|<statements-block>)
    fn _r_function_decl(&mut self) -> Option<ASTNode> {
        // Parse the return type.
        let beg_loc = self.get_next_token_loc();
        let ret_type = self._r_type();

        // Parse the function name.
        let label = self
            .consume_identifier_or_error()?
            .get_identifier()
            .unwrap()
            .to_string();

        // Parse the arguments.
        let mut args = Vec::new();
        self.consume_sym_or_error(TokenValue::sym_lparen());
        if !self.next_token_is_sym(TokenValue::sym_rparen()) {
            loop {
                let arg_ty = self._r_type();
                let arg_name = self
                    .consume_identifier_or_error()?
                    .get_identifier()
                    .unwrap()
                    .to_string();
                args.push((arg_name, arg_ty));
                if !self.try_consume_sym(TokenValue::sym_comma()) {
                    break;
                }
            }
        }
        self.consume_sym_or_error(TokenValue::sym_rparen());

        // Parse the optional body.
        let mut body = None;
        let mut end_loc = self.get_next_token_loc();
        if !self.try_consume_sym(TokenValue::sym_semi()) {
            body = Some(self._r_statement_block());
            end_loc = body.as_ref().unwrap().get_loc();
        }

        Some(ASTNodeFunctionDecl::new(
            Location::join(beg_loc, end_loc),
            label,
            args,
            ret_type,
            body,
        ))
    }

    // Base function to parse a statement.
    fn _r_statement(&mut self) -> ASTNode {
        // '{' ... '}'
        if self.next_token_is_sym(TokenValue::sym_lcbracket()) {
            return self._r_statement_block();
        }

        // <var decl>
        if self._next_is_type() {
            return self._r_statement_vardecl();
        }

        // 'return' <expr> ';'
        if let Some(beg_tok) = self.try_consume_keyword(V_KW_RETURN) {
            let val = self._r_expr();
            let end_loc = self.get_next_token_loc();
            self.consume_sym_or_error(TokenValue::sym_semi());
            return ASTNodeReturnStatement::new(Location::join(beg_tok.loc(), end_loc), val);
        }

        let err_tok = self.get_token();
        self.emit_bad_token_error(&err_tok, "expected a statement".to_string());
        ASTNodeBlockStatement::new(err_tok.loc(), vec![])
    }

    // '{' (<statement> ';')* '}'
    fn _r_statement_block(&mut self) -> ASTNode {
        let beg_loc = self.get_next_token_loc();
        self.consume_sym_or_error(TokenValue::sym_lcbracket());
        let mut statements = Vec::new();

        while !self.next_token_is_sym(TokenValue::sym_rcbracket()) {
            statements.push(self._r_statement());
        }

        let end_loc = self.get_next_token_loc();
        self.consume_sym_or_error(TokenValue::sym_rcbracket());
        ASTNodeBlockStatement::new(Location::join(beg_loc, end_loc), statements)
    }

    // '<typename> (<id> '=' <init-expr> ','?)+ ('[' <size-expr '])* ';';
    fn _r_statement_vardecl(&mut self) -> ASTNode {
        // Parse the type.
        let type_node = self._r_type();

        // Parse the declarations.
        let mut var_decls = vec![];

        loop {
            // Parse var name
            let var_id = match self.consume_identifier_or_error() {
                Some(tok) => tok.get_identifier().unwrap().to_string(),
                None => break,
            };

            // Parse optional init value.
            let var_init = if self.try_consume_sym(TokenValue::sym_assign()) {
                Some(self._r_expr())
            } else {
                None
            };

            // Parse optional array sizes.
            let mut sizes = vec![];
            while self.try_consume_sym(TokenValue::sym_lbracket()) {
                sizes.push(self._r_expr());
                if self
                    .consume_sym_or_error(TokenValue::sym_rbracket())
                    .is_none()
                {
                    break;
                }
            }

            var_decls.push((var_id, var_init, sizes));

            // Stop if there is no ','
            if !self.try_consume_sym(TokenValue::sym_comma()) {
                break;
            }
        }

        let end_loc = self.get_next_token_loc();
        self.consume_sym_or_error(TokenValue::sym_semi());
        let loc = Location::join(type_node.get_loc(), end_loc);

        ASTNodeVarDeclStatement::new(loc, type_node, var_decls)
    }

    // Base function to parse an expression.
    fn _r_expr(&mut self) -> ASTNode {
        match self._r_expr_p1() {
            Some(node) => node,
            None => {
                // TODO: We should probably return a special ASTNodeErrorExp.
                ASTNodeIntLiteral::new(self.get_next_token_loc(), 0)
            }
        }
    }

    // Only supports `+` operation for now.
    // TODO: Add all C operators with the right precedence.
    fn _r_expr_p1(&mut self) -> Option<ASTNode> {
        let mut acc = match self._r_expr_atom() {
            Some(acc) => acc,
            None => return None,
        };

        loop {
            if !self.try_consume_sym(TokenValue::sym_plus()) {
                break;
            }

            let rhs = match self._r_expr_atom() {
                Some(acc) => acc,
                None => return Some(acc),
            };

            let loc = Location::join(acc.get_loc(), rhs.get_loc());
            acc = ASTNodeBinopExpr::new(loc, ASTNodeBinopExprKind::Add, acc, rhs);
        }

        Some(acc)
    }

    // '(' <expr ')'
    // or literal exprs / label
    fn _r_expr_atom(&mut self) -> Option<ASTNode> {
        // '(' <expr> ')
        if self.try_consume_sym(TokenValue::sym_lparen()) {
            let res = self._r_expr();
            self.consume_sym_or_error(TokenValue::sym_rparen());
            return Some(res);
        }

        // <int>
        if let Some(tok) = self.try_consume_int() {
            return Some(ASTNodeIntLiteral::new(tok.loc(), tok.get_int().unwrap()));
        }

        // <float>
        if let Some(tok) = self.try_consume_float() {
            return Some(ASTNodeFloatLiteral::new(
                tok.loc(),
                tok.get_float().unwrap(),
            ));
        }

        // <char>
        if let Some(tok) = self.try_consume_char_literal() {
            return Some(ASTNodeCharLiteral::new(
                tok.loc(),
                tok.get_char_literal().unwrap(),
            ));
        }

        // <string>
        if let Some(tok) = self.try_consume_string_literal() {
            return Some(ASTNodeStringLiteral::new(
                tok.loc(),
                tok.get_string_literal().unwrap().to_string(),
            ));
        }

        // <id>
        if let Some(tok) = self.try_consume_identifier() {
            return Some(ASTNodeLabelExpr::new(
                tok.loc(),
                tok.get_identifier().unwrap().to_string(),
            ));
        }

        let err_tok = self.get_token();
        self.emit_bad_token_error(&err_tok, "expected constant expr or var name".to_string());
        None
    }

    // Check if the next token starts a type.
    // We need this to detect a few things such as variable decl vs expr.
    fn _next_is_type(&mut self) -> bool {
        let types_kw = &[
            KW_UNSIGNED,
            KW_SIGNED,
            KW_CHAR,
            KW_SHORT,
            KW_INT,
            KW_LONG,
            KW_FLOAT,
            KW_DOUBLE,
            KW_VOID,
            KW_STRUCT,
            KW_ENUM,
        ];
        match self.peek_token().val() {
            TokenValue::Keyword(kw) => types_kw.contains(kw),
            _ => false,
        }
    }

    // Check if the next token is a type qualifier. (const, register, etc)
    // We need this to detect a few things such as variable decl vs expr.
    fn _next_is_type_qualifier(&mut self) -> bool {
        let kws = &[KW_REGISTER, KW_CONST];
        match self.peek_token().val() {
            TokenValue::Keyword(kw) => kws.contains(kw),
            _ => false,
        }
    }

    // Base function to parse a type
    fn _r_type(&mut self) -> ASTNode {
        let beg_loc = self.get_next_token_loc();
        let signed = if let Some(_tok) = self.try_consume_keyword(V_KW_SIGNED) {
            IntegerSignedness::Signed
        } else if let Some(_tok) = self.try_consume_keyword(V_KW_UNSIGNED) {
            IntegerSignedness::Unsigned
        } else {
            IntegerSignedness::Signless
        };

        // "char"
        if let Some(tok) = self.try_consume_keyword(V_KW_CHAR) {
            let loc = Location::join(beg_loc, tok.loc());
            let typename = match signed {
                IntegerSignedness::Signed => ast_types::TYPENAME_SIGNED_CHAR,
                IntegerSignedness::Unsigned => ast_types::TYPENAME_UNSIGNED_CHAR,
                IntegerSignedness::Signless => ast_types::TYPENAME_CHAR,
            };
            return ASTNodeLabelType::new(loc, typename.to_string());
        }

        // "short"
        if let Some(tok) = self.try_consume_keyword(V_KW_SHORT) {
            let loc = Location::join(beg_loc, tok.loc());
            let typename = match signed {
                IntegerSignedness::Signed => ast_types::TYPENAME_SIGNED_SHORT,
                IntegerSignedness::Unsigned => ast_types::TYPENAME_UNSIGNED_SHORT,
                IntegerSignedness::Signless => ast_types::TYPENAME_SHORT,
            };
            return ASTNodeLabelType::new(loc, typename.to_string());
        }

        // "int"
        if let Some(tok) = self.try_consume_keyword(V_KW_INT) {
            let loc = Location::join(beg_loc, tok.loc());
            let typename = match signed {
                IntegerSignedness::Signed => ast_types::TYPENAME_SIGNED_INT,
                IntegerSignedness::Unsigned => ast_types::TYPENAME_UNSIGNED_INT,
                IntegerSignedness::Signless => ast_types::TYPENAME_INT,
            };
            return ASTNodeLabelType::new(loc, typename.to_string());
        }

        // "long"
        if let Some(tok) = self.try_consume_keyword(V_KW_LONG) {
            let loc = Location::join(beg_loc, tok.loc());
            let typename = match signed {
                IntegerSignedness::Signed => ast_types::TYPENAME_SIGNED_LONG,
                IntegerSignedness::Unsigned => ast_types::TYPENAME_UNSIGNED_LONG,
                IntegerSignedness::Signless => ast_types::TYPENAME_LONG,
            };
            return ASTNodeLabelType::new(loc, typename.to_string());
        }

        if signed == IntegerSignedness::Signless {
            // "void"
            if let Some(tok) = self.try_consume_keyword(V_KW_VOID) {
                return ASTNodeLabelType::new(tok.loc(), ast_types::TYPENAME_VOID.to_string());
            }

            // "float"
            if let Some(tok) = self.try_consume_keyword(V_KW_FLOAT) {
                return ASTNodeLabelType::new(tok.loc(), ast_types::TYPENAME_FLOAT.to_string());
            }

            // "double"
            if let Some(tok) = self.try_consume_keyword(V_KW_DOUBLE) {
                return ASTNodeLabelType::new(tok.loc(), ast_types::TYPENAME_DOUBLE.to_string());
            }

            // <id>
            if let Some(tok) = self.try_consume_identifier() {
                return ASTNodeLabelType::new(tok.loc(), tok.get_identifier().unwrap().to_string());
            }
        }

        // TODO: We should probably return a special ASTNodeErrorType.
        let err_tok = self.peek_token().clone();
        self.emit_bad_token_error(&err_tok, "expected type".to_string());
        ASTNodeLabelType::new(err_tok.loc(), "".to_string())
    }
}

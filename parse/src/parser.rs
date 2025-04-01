use diagnostics::{emit_error, CompilerDiagnosticsEmitter};
use iostreams::location::Location;

use crate::lexer::{Lexer, Token, TokenValue};

// This trait must be implemented by all parser methods.
// Define lots of convenient helper methods for parsing.
pub trait Parser: CompilerDiagnosticsEmitter {
    // Returns the current lexer of the parser.
    fn get_lexer_mut(&mut self) -> &mut Lexer;

    // Look at the next token.
    fn peek_token(&mut self) -> &Token {
        self.get_lexer_mut().peek_token()
    }

    // Consume the next token.
    fn get_token(&mut self) -> Token {
        self.get_lexer_mut().get_token()
    }

    // Get the location of the next token
    fn get_next_token_loc(&mut self) -> Location {
        self.peek_token().loc()
    }

    // Emit an error when the wrong token is received.
    fn emit_bad_token_error(&mut self, tok: &Token, expected_msg: String) {
        emit_error(
            self,
            tok.loc(),
            format!("Expected {} but got `{}`", expected_msg, tok),
        );
    }

    // Returns true if the next token is `sym``
    fn next_token_is_sym(&mut self, sym: TokenValue) -> bool {
        let sym = match sym {
            TokenValue::Symbol(sym) => sym,
            _ => panic!("Expected a value symbol, got {:?}", sym),
        };
        let tok = self.peek_token().get_symbol();
        tok.is_some() && tok.unwrap() == sym
    }

    // Look at the next token. Returns true and consume it if it's `sym`.
    // Otherwise returns false.
    fn try_consume_sym(&mut self, sym: TokenValue) -> bool {
        let sym = match sym {
            TokenValue::Symbol(sym) => sym,
            _ => panic!("Expected a value symbol, got {:?}", sym),
        };
        let tok = self.peek_token().get_symbol();
        if tok.is_some() && tok.unwrap() == sym {
            self.get_token();
            true
        } else {
            false
        }
    }

    // Consume the next token only if it's equal to `sym`
    // If it's not, emit an error and returns None.
    // Returns the consumed token.
    fn consume_sym_or_error(&mut self, sym: TokenValue) -> Option<Token> {
        let sym_str = match sym {
            TokenValue::Symbol(sym) => sym,
            _ => panic!("Expected a value symbol, got {:?}", sym),
        };
        let tok = self.peek_token();
        if !tok.is_symbol() || tok.get_symbol().unwrap() != sym_str {
            let err_tok = tok.clone();
            self.emit_bad_token_error(&err_tok, format!("symbol `{}`", sym));
            return None;
        }
        Some(self.get_token())
    }

    // Look at the next token. Consume and reutrns it if it's `kw`.
    // Otherwise returns None.
    fn try_consume_keyword(&mut self, kw: TokenValue) -> Option<Token> {
        let kw = match kw {
            TokenValue::Keyword(kw) => kw,
            _ => panic!("Expected a value keyword, got {:?}", kw),
        };
        let tok = self.peek_token().get_keyword();
        if tok.is_some() && tok.unwrap() == kw {
            Some(self.get_token())
        } else {
            None
        }
    }

    // Consume the next token only if it's equal to `kw`
    // If it's not, emit an error and returns None
    // Returns the consumed token.
    fn consume_keyword_or_error(&mut self, kw: TokenValue) -> Option<Token> {
        let kw_str = match kw {
            TokenValue::Keyword(kw) => kw,
            _ => panic!("Expected a value keyword, got {:?}", kw),
        };
        let tok = self.peek_token();
        if !tok.is_keyword() || tok.get_keyword().unwrap() != kw_str {
            let err_tok = tok.clone();
            self.emit_bad_token_error(&err_tok, format!("keyword `{}`", kw));
            return None;
        }
        Some(self.get_token())
    }

    // Look at the next token. Returns true and consume it if it's an int.
    // Otherwise returns false.
    fn try_consume_int(&mut self) -> Option<Token> {
        let tok = self.peek_token();
        if tok.is_int() {
            Some(self.get_token())
        } else {
            None
        }
    }

    // Consume the next token only if it's an Int.
    // If it's not, emit an error.
    // Returns the consumed token.
    fn consume_int_or_error(&mut self) -> Option<Token> {
        let tok = self.peek_token();
        if !tok.is_int() {
            let err_tok = tok.clone();
            self.emit_bad_token_error(&err_tok, "int".to_string());
            return None;
        }
        Some(self.get_token())
    }

    // Returns true if the next token is an integer.
    fn next_token_is_int(&mut self) -> bool {
        self.peek_token().is_int()
    }

    // Look at the next token. Returns true and consume it if it's a float.
    // Otherwise returns false.
    fn try_consume_float(&mut self) -> Option<Token> {
        let tok = self.peek_token();
        if tok.is_float() {
            Some(self.get_token())
        } else {
            None
        }
    }

    // Consume the next token only if it's a Float.
    // If it's not, emit an error.
    // Returns the consumed token.
    fn consume_float_or_error(&mut self) -> Option<Token> {
        let tok = self.peek_token();
        if !tok.is_float() {
            let err_tok = tok.clone();
            self.emit_bad_token_error(&err_tok, "float".to_string());
            return None;
        }
        Some(self.get_token())
    }

    // Returns true if the next token is a float.
    fn next_token_is_float(&mut self) -> bool {
        self.peek_token().is_float()
    }

    // Look at the next token. Returns true and consume it if it's a string literal.
    // Otherwise returns false.
    fn try_consume_string_literal(&mut self) -> Option<Token> {
        let tok = self.peek_token();
        if tok.is_string_literal() {
            Some(self.get_token())
        } else {
            None
        }
    }

    // Consume the next token only if it's a string literal.
    // If it's not, emit an error.
    // Returns the consumed token.
    fn consume_string_literal_or_error(&mut self) -> Option<Token> {
        let tok = self.peek_token();
        if !tok.is_string_literal() {
            let err_tok = tok.clone();
            self.emit_bad_token_error(&err_tok, "string literal".to_string());
            return None;
        }
        Some(self.get_token())
    }

    // Returns true if the next token is a string literal.
    fn next_token_is_string_literal(&mut self) -> bool {
        self.peek_token().is_string_literal()
    }

    // Look at the next token. Returns true and consume it if it's a char literal.
    // Otherwise returns false.
    fn try_consume_char_literal(&mut self) -> Option<Token> {
        let tok = self.peek_token();
        if tok.is_char_literal() {
            Some(self.get_token())
        } else {
            None
        }
    }

    // Look at the next token. Returns true and consume it if it's an identifier
    // Otherwise returns false.
    fn try_consume_identifier(&mut self) -> Option<Token> {
        let tok = self.peek_token();
        if tok.is_identifier() {
            Some(self.get_token())
        } else {
            None
        }
    }

    // Consume the next token only if it's an identifier.
    // If it's not, emit an error.
    // Returns the consumed token.
    fn consume_identifier_or_error(&mut self) -> Option<Token> {
        let tok = self.peek_token();
        if !tok.is_identifier() {
            let err_tok = tok.clone();
            self.emit_bad_token_error(&err_tok, "identifier".to_string());
            return None;
        }
        Some(self.get_token())
    }

    // Consume the next token only if it's an identifier.
    // If it's not, emit an error.
    // Returns the consumed token.
    fn consume_identifier_val_or_error(&mut self, val: &str) -> Option<Token> {
        let tok = self.peek_token();
        if tok.get_identifier() != Some(val) {
            let err_tok = tok.clone();
            self.emit_bad_token_error(&err_tok, format!("identifier `{}`", val));
            return None;
        }
        Some(self.get_token())
    }

    // Returns true if the next token is `sym``
    fn next_token_is_identifier_val(&mut self, val: &str) -> bool {
        let tok = self.peek_token().get_identifier();
        tok == Some(val)
    }

    // Returns true if the next token is `sym``
    fn next_token_is_any_identifier(&mut self) -> bool {
        self.peek_token().is_identifier()
    }

    // Returns true if the next token is EOF.
    fn next_token_is_eof(&mut self) -> bool {
        self.peek_token().is_eof()
    }

    // Consume the next token only if it's an EOF.
    // If it's not, emit an error.
    // Returns the consumed token.
    fn consume_eof_or_error(&mut self) -> Option<Token> {
        let tok = self.peek_token();
        if !tok.is_eof() {
            let err_tok = tok.clone();
            self.emit_bad_token_error(&err_tok, "End of File".to_string());
            return None;
        }
        Some(self.get_token())
    }
}

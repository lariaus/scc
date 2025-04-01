use std::{collections::HashSet, fmt::Display};

use diagnostics::{emit_error, CompilerDiagnostics, CompilerDiagnosticsEmitter};
use iostreams::{
    location::{Location, RawFilePosition},
    source_stream::SourceStream,
};
use utils::{stringutils::decode_string_literal, trie::DenseTrie};

// Class used to get / restore the position in the lexer.
#[derive(Debug, Clone)]
pub struct LexerPostion {
    row: usize,
    col: usize,
    pos: RawFilePosition,
    next_tok: Option<Token>,
}

// Token enum value.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    Int(u64),
    Float(f64),
    Identifier(String),
    Keyword(&'static str),
    Symbol(&'static str),
    CharLiteral(char),
    StringLiteral(String),
    Comment(String),
    Eof,
    Error,
}

impl TokenValue {
    pub fn sym_plus() -> Self {
        Self::Symbol("+")
    }

    pub fn sym_minus() -> Self {
        Self::Symbol("-")
    }

    pub fn sym_multiply() -> Self {
        Self::Symbol("*")
    }

    pub fn sym_divide() -> Self {
        Self::Symbol("/")
    }

    pub fn sym_modulo() -> Self {
        Self::Symbol("%")
    }

    pub fn sym_lparen() -> Self {
        Self::Symbol("(")
    }

    pub fn sym_rparen() -> Self {
        Self::Symbol(")")
    }

    pub fn sym_lbracket() -> Self {
        Self::Symbol("[")
    }

    pub fn sym_rbracket() -> Self {
        Self::Symbol("]")
    }

    pub fn sym_lcbracket() -> Self {
        Self::Symbol("{")
    }

    pub fn sym_rcbracket() -> Self {
        Self::Symbol("}")
    }

    pub fn sym_semi() -> Self {
        Self::Symbol(";")
    }

    pub fn sym_comma() -> Self {
        Self::Symbol(",")
    }

    pub fn sym_assign() -> Self {
        Self::Symbol("=")
    }

    pub fn sym_lt() -> Self {
        Self::Symbol("<")
    }

    pub fn sym_le() -> Self {
        Self::Symbol("<=")
    }

    pub fn sym_gt() -> Self {
        Self::Symbol(">")
    }

    pub fn sym_ge() -> Self {
        Self::Symbol(">=")
    }

    pub fn sym_colon() -> Self {
        Self::Symbol(":")
    }

    pub fn sym_percent() -> Self {
        Self::Symbol("%")
    }

    pub fn sym_deref() -> Self {
        Self::Symbol("->")
    }

    pub fn sym_xor() -> Self {
        Self::Symbol("^")
    }

    pub fn sym_dot() -> Self {
        Self::Symbol(".")
    }
}

impl Display for TokenValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            TokenValue::Int(x) => write!(f, "{}", x),
            TokenValue::Float(x) => write!(f, "{}", x),
            TokenValue::Identifier(x) => write!(f, "{}", x),
            TokenValue::Keyword(x) => write!(f, "{}", x),
            TokenValue::Symbol(x) => write!(f, "{}", x),
            TokenValue::Comment(x) => write!(f, "/*{}*/", x),
            TokenValue::CharLiteral(x) => write!(f, "'{}'", x),
            TokenValue::StringLiteral(x) => write!(f, "\"{}\"", x),
            TokenValue::Eof => write!(f, "<<EOF>>"),
            TokenValue::Error => write!(f, "<<ERR>>"),
        }
    }
}

// Token instance produced by the lexer.
#[derive(Debug, Clone)]
pub struct Token {
    val: TokenValue,
    loc: Location,
}

impl Token {
    pub fn loc(&self) -> Location {
        self.loc
    }

    pub fn val(&self) -> &TokenValue {
        &self.val
    }

    pub fn is_int(&self) -> bool {
        match self.val {
            TokenValue::Int(_) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self.val {
            TokenValue::Float(_) => true,
            _ => false,
        }
    }

    pub fn is_identifier(&self) -> bool {
        match self.val {
            TokenValue::Identifier(_) => true,
            _ => false,
        }
    }

    pub fn is_keyword(&self) -> bool {
        match self.val {
            TokenValue::Keyword(_) => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self.val {
            TokenValue::Symbol(_) => true,
            _ => false,
        }
    }

    pub fn is_comment(&self) -> bool {
        match self.val {
            TokenValue::Comment(_) => true,
            _ => false,
        }
    }

    pub fn is_char_literal(&self) -> bool {
        match self.val {
            TokenValue::CharLiteral(_) => true,
            _ => false,
        }
    }

    pub fn is_string_literal(&self) -> bool {
        match self.val {
            TokenValue::StringLiteral(_) => true,
            _ => false,
        }
    }

    pub fn is_eof(&self) -> bool {
        match self.val {
            TokenValue::Eof => true,
            _ => false,
        }
    }

    pub fn is_error(&self) -> bool {
        match self.val {
            TokenValue::Error => true,
            _ => false,
        }
    }

    pub fn get_int(&self) -> Option<u64> {
        match self.val {
            TokenValue::Int(x) => Some(x),
            _ => None,
        }
    }

    pub fn get_float(&self) -> Option<f64> {
        match self.val {
            TokenValue::Float(x) => Some(x),
            _ => None,
        }
    }

    pub fn get_identifier(&self) -> Option<&str> {
        match &self.val {
            TokenValue::Identifier(x) => Some(x),
            _ => None,
        }
    }

    pub fn take_identifier(self) -> Option<String> {
        match self.val {
            TokenValue::Identifier(x) => Some(x),
            _ => None,
        }
    }

    pub fn get_keyword(&self) -> Option<&'static str> {
        match &self.val {
            TokenValue::Keyword(x) => Some(&x),
            _ => None,
        }
    }

    pub fn get_symbol(&self) -> Option<&'static str> {
        match &self.val {
            TokenValue::Symbol(x) => Some(&x),
            _ => None,
        }
    }

    pub fn get_comment(&self) -> Option<&str> {
        match &self.val {
            TokenValue::Comment(x) => Some(x),
            _ => None,
        }
    }

    pub fn get_char_literal(&self) -> Option<char> {
        match &self.val {
            TokenValue::CharLiteral(x) => Some(*x),
            _ => None,
        }
    }

    pub fn get_string_literal(&self) -> Option<&str> {
        match &self.val {
            TokenValue::StringLiteral(x) => Some(x),
            _ => None,
        }
    }

    pub fn take_string_literal(self) -> Option<String> {
        match self.val {
            TokenValue::StringLiteral(x) => Some(x),
            _ => None,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val())
    }
}

// The Lexer convert a stream of characters into a stream of tokens.
pub struct Lexer {
    ifs: Box<dyn SourceStream>,
    diagnostics: CompilerDiagnostics,
    row: usize,
    col: usize,
    next_tok: Option<Token>,

    // Lexer config.
    // If true, always skip the error tokens (but still emit errors).
    skip_error_tokens: bool,
    // If true, supports "// xxx" comments.
    support_c_inline_comment: bool,
    // if true, supports "/*xxx*/" comments.
    support_c_multiline_comment: bool,
    // If true, always skip the comment tokens.
    skip_comment_tokens: bool,
    // If true, supports 'xxxx' strings.
    support_single_quote_strings: bool,
    // If true, supports "xxxx" strings.
    support_double_quote_strings: bool,
    // If true, decode token strings in the lexer (eg transform '\n' into <newline>).
    decode_tok_strings: bool,
    // If true, single quote literals are considered as a char.
    single_quote_literal_is_char: bool,
    keywords_set: HashSet<&'static str>,
    symbols_trie: DenseTrie,
    symbols_set: HashSet<&'static str>,
}

impl Lexer {
    pub fn new(ifs: Box<dyn SourceStream>) -> Self {
        Self {
            ifs,
            diagnostics: CompilerDiagnostics::new(),
            row: 1,
            col: 1,
            next_tok: None,
            skip_error_tokens: false,
            support_c_inline_comment: false,
            support_c_multiline_comment: false,
            skip_comment_tokens: true,
            support_single_quote_strings: true,
            support_double_quote_strings: true,
            decode_tok_strings: false,
            single_quote_literal_is_char: false,
            keywords_set: HashSet::new(),
            symbols_trie: DenseTrie::new(),
            symbols_set: HashSet::new(),
        }
    }

    // Add a keyword recognized by the lexer.
    pub fn add_keyword(&mut self, s: &'static str) {
        self.keywords_set.insert(s);
    }

    // Add a keyword recognized by the lexer.
    pub fn add_keyword_val(&mut self, val: TokenValue) {
        match val {
            TokenValue::Keyword(s) => self.add_keyword(s),
            _ => panic!("Expected a value keyword, got {:?}", val),
        }
    }

    // Add a symbol recognized by the lexer.
    pub fn add_symbol(&mut self, s: &'static str) {
        self.symbols_trie.insert(s);
        self.symbols_set.insert(s);
    }

    // Add a symbol recognized by the lexer.
    pub fn add_symbol_val(&mut self, val: TokenValue) {
        match val {
            TokenValue::Symbol(s) => self.add_symbol(s),
            _ => panic!("Expected a value symbol, got {:?}", val),
        }
    }

    pub fn skip_error_tokens(&self) -> bool {
        self.skip_error_tokens
    }

    pub fn set_skip_error_tokens(&mut self, skip_error_tokens: bool) {
        self.skip_error_tokens = skip_error_tokens;
    }

    pub fn support_c_inline_comment(&self) -> bool {
        self.support_c_inline_comment
    }

    pub fn set_support_c_inline_comment(&mut self, support_c_inline_comment: bool) {
        self.support_c_inline_comment = support_c_inline_comment;
    }

    pub fn support_c_multiline_comment(&self) -> bool {
        self.support_c_multiline_comment
    }

    pub fn set_support_c_multiline_comment(&mut self, support_c_multiline_comment: bool) {
        self.support_c_multiline_comment = support_c_multiline_comment;
    }

    pub fn skip_comment_tokens(&self) -> bool {
        self.skip_comment_tokens
    }

    pub fn set_skip_comment_tokens(&mut self, skip_comment_tokens: bool) {
        self.skip_comment_tokens = skip_comment_tokens;
    }

    pub fn support_single_quote_strings(&self) -> bool {
        self.support_single_quote_strings
    }

    pub fn set_support_single_quote_strings(&mut self, support_single_quote_strings: bool) {
        self.support_single_quote_strings = support_single_quote_strings;
    }

    pub fn support_double_quote_strings(&self) -> bool {
        self.support_double_quote_strings
    }

    pub fn set_support_double_quote_strings(&mut self, support_double_quote_strings: bool) {
        self.support_double_quote_strings = support_double_quote_strings;
    }

    pub fn decode_tok_strings(&self) -> bool {
        self.decode_tok_strings
    }

    pub fn set_decode_tok_strings(&mut self, decode_tok_strings: bool) {
        self.decode_tok_strings = decode_tok_strings;
    }

    pub fn single_quote_literal_is_char(&self) -> bool {
        self.single_quote_literal_is_char
    }

    pub fn set_single_quote_literal_is_char(&mut self, single_quote_literal_is_char: bool) {
        self.single_quote_literal_is_char = single_quote_literal_is_char;
    }

    // Save the current position
    pub fn get_position(&self) -> LexerPostion {
        LexerPostion {
            row: self.row,
            col: self.col,
            pos: self.ifs.get_position(),
            next_tok: self.next_tok.clone(),
        }
    }

    // Restore the position
    pub fn set_position(&mut self, pos: LexerPostion) {
        self.row = pos.row;
        self.col = pos.col;
        self.ifs.set_position(pos.pos);
        self.next_tok = pos.next_tok;
    }

    fn _peekc(&mut self) -> Option<char> {
        self.ifs.peekc()
    }

    fn _getc(&mut self) -> Option<char> {
        // TODO: I don't think it's the right update here
        let res = self.ifs.getc();
        if let Some(c) = res {
            if c == '\n' {
                self.row += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        };
        res
    }

    // Return the token at the current position
    pub fn peek_token(&mut self) -> &Token {
        if self.next_tok.is_none() {
            self.next_tok = Some(self._read_next_tok());
        }
        self.next_tok.as_ref().unwrap()
    }

    // Get the token at the current position and move to the end
    pub fn get_token(&mut self) -> Token {
        // Read the current token.
        self.peek_token();
        // Just clone when reaching EOF.
        if self.next_tok.as_ref().unwrap().is_eof() {
            return self.next_tok.as_ref().unwrap().clone();
        }

        // Otherwhise consume it.
        let mut res = None;
        std::mem::swap(&mut res, &mut self.next_tok);
        res.unwrap()
    }

    // Returns true if we reached the end of file.
    pub fn is_eof(&mut self) -> bool {
        self.peek_token().is_eof()
    }

    // Returns a Location object for the current postion.
    fn _current_loc(&self) -> Location {
        Location::new(
            self.ifs.get_position(),
            self.ifs.get_position(),
            self.ifs.get_file_id(),
            self.row,
            self.row,
            self.col,
            self.col,
        )
    }

    // Build an eof token.
    fn _tok_eof(&self) -> Token {
        Token {
            val: TokenValue::Eof,
            loc: self._current_loc(),
        }
    }

    // Build an error token and emit an error.
    fn _tok_err(&mut self, loc: Location, message: String) -> Token {
        emit_error(self, loc, message);
        Token {
            val: TokenValue::Error,
            loc,
        }
    }

    // Read the next tokens, doing some specific transforms before returning it.
    fn _read_next_tok(&mut self) -> Token {
        loop {
            let tok = self._read_next_raw_token();
            if tok.is_error() && self.skip_error_tokens {
                continue;
            }
            if tok.is_comment() && self.skip_comment_tokens {
                continue;
            }
            return tok;
        }
    }

    // Read the next raw token from the stream.
    fn _read_next_raw_token(&mut self) -> Token {
        // Start by skipping whitespaces. (and quit if we reached EOF)
        loop {
            let c = self._peekc();
            if c.is_none() {
                return self._tok_eof();
            }
            if !c.unwrap().is_whitespace() {
                break;
            }
            self._getc();
        }

        let c = self._peekc().unwrap();
        if let Some(tok) = self._read_next_tok_comment() {
            tok
        } else if let Some(tok) = self._read_next_tok_symbol() {
            tok
        } else if c.is_numeric() || c == '.' {
            self._read_next_tok_number()
        } else if c.is_alphabetic() || c == '_' {
            self._read_next_tok_identifier()
        } else if (c == '\'' && self.support_single_quote_strings)
            || (c == '"' && self.support_double_quote_strings)
        {
            self._read_next_tok_string()
        } else {
            // Generate an error token.
            let tok = self._tok_err(self._current_loc(), format!("Unknow character '{}'", c));
            // Then move to the next character.
            self._getc();
            tok
        }
    }

    fn _read_next_tok_number(&mut self) -> Token {
        let beg_line = self.row;
        let beg_col = self.col;
        let beg_pos = self.ifs.get_position();

        let mut end_line = self.row;
        let mut end_col = self.col;
        let mut end_pos = self.ifs.get_position();

        let mut val: u64 = 0;
        let mut has_decimal = false;
        let mut val_decimal: u64 = 0;
        let mut decimal_div: u64 = 1;

        loop {
            let c = match self._peekc() {
                Some(c) => c,
                None => break,
            };
            if !(c.is_numeric() || (c == '.' && !has_decimal)) {
                break;
            }

            // Move to the next char.
            end_line = self.row;
            end_col = self.col;
            end_pos = self.ifs.get_position();
            self._getc();

            if c == '.' {
                assert!(!has_decimal);
                has_decimal = true;
            } else {
                let digit = c.to_string().parse::<u64>().unwrap();
                if has_decimal {
                    val_decimal = 10 * val_decimal + digit;
                    decimal_div *= 10;
                } else {
                    val = 10 * val + digit;
                }
            }
        }

        let loc = Location::new(
            beg_pos,
            end_pos,
            self.ifs.get_file_id(),
            beg_line,
            end_line,
            beg_col,
            end_col,
        );

        if has_decimal {
            let val = (val as f64) + (val_decimal as f64) / (decimal_div as f64);
            Token {
                val: TokenValue::Float(val),
                loc,
            }
        } else {
            Token {
                val: TokenValue::Int(val),
                loc,
            }
        }
    }

    fn _read_next_tok_identifier(&mut self) -> Token {
        let beg_line = self.row;
        let beg_col = self.col;
        let beg_pos = self.ifs.get_position();

        let mut end_line = self.row;
        let mut end_col = self.col;
        let mut end_pos = self.ifs.get_position();

        let mut identifier = String::new();

        loop {
            let c = match self._peekc() {
                Some(c) => c,
                None => break,
            };
            if !(c.is_alphanumeric() || c == '_') {
                break;
            }

            // Move to the next char.
            end_line = self.row;
            end_col = self.col;
            end_pos = self.ifs.get_position();
            self._getc();
            identifier.push(c);
        }

        let loc = Location::new(
            beg_pos,
            end_pos,
            self.ifs.get_file_id(),
            beg_line,
            end_line,
            beg_col,
            end_col,
        );
        if let Some(kw_val) = self.keywords_set.get(identifier.as_str()) {
            Token {
                val: TokenValue::Keyword(kw_val),
                loc,
            }
        } else {
            Token {
                val: TokenValue::Identifier(identifier),
                loc,
            }
        }
    }

    fn _read_next_tok_symbol(&mut self) -> Option<Token> {
        // Shorcut: Don't bother dong anything if you can already the first chaarcter can't be the beginning of a symbol.
        let first_c = self._peekc().unwrap();
        if !self.symbols_trie.any_starts_with(first_c) {
            return None;
        }

        let beg_line = self.row;
        let beg_col = self.col;
        let beg_pos = self.ifs.get_position();

        // Initialize the symbol info.
        self.symbols_trie.reset_path();
        let mut symbol = Vec::new();
        let mut last_loc = None;
        let mut last_size = 0;

        loop {
            let c = match self._peekc() {
                Some(c) => c,
                None => break,
            };

            // Special case to avoid capturing `.` for numbers starting with `.`.
            if symbol.len() == 1 && symbol[0] == '.' && c.is_numeric() {
                last_loc = None;
                break;
            }

            let is_end = match self.symbols_trie.try_advance(c) {
                Some(is_end) => is_end,
                None => break,
            };

            // Extend the full symbol string
            symbol.push(c);

            // Mark it if we curently have a valid symbol.
            if is_end {
                last_loc = Some(Location::new(
                    beg_pos,
                    self.ifs.get_position(),
                    self.ifs.get_file_id(),
                    beg_line,
                    self.row,
                    beg_col,
                    self.col,
                ));
                last_size = symbol.len();
            }

            // Move to the next char.
            self._getc();
        }

        if last_loc.is_none() {
            // No symbol were found.
            // Go back to the initial position.
            self.row = beg_line;
            self.col = beg_col;
            self.ifs.set_position(beg_pos);
            return None;
        }

        let last_loc = last_loc.unwrap();

        if last_size < symbol.len() {
            // We have visited extra characters which are not symbol, we need to go back a few characters.
            // First go back to the last character of the symbol.
            self.row = last_loc.end_line();
            self.col = last_loc.end_col();
            self.ifs.set_position(last_loc.end_pos());
            // Then move to the next one.
            self._getc();
        }

        let symbols_str: String = symbol[0..last_size].iter().collect();
        Some(Token {
            val: TokenValue::Symbol(self.symbols_set.get(symbols_str.as_str()).unwrap()),
            loc: last_loc,
        })
    }

    fn _read_next_tok_comment(&mut self) -> Option<Token> {
        if !self.support_c_inline_comment && !self.support_c_multiline_comment {
            return None;
        }

        // Must always start with '/'
        if self._peekc().unwrap() != '/' {
            return None;
        }

        let beg_line = self.row;
        let beg_col = self.col;
        let beg_pos = self.ifs.get_position();
        self._getc();

        // Start by trying to match the beginning
        let is_multiline = match self._getc() {
            Some(c)
                if (c == '/' && self.support_c_inline_comment)
                    || (c == '*' && self.support_c_multiline_comment) =>
            {
                c == '*'
            }
            _ => {
                // This is not a comment, revert to the previous char.
                self.row = beg_line;
                self.col = beg_col;
                self.ifs.set_position(beg_pos);
                return None;
            }
        };

        let mut end_line;
        let mut end_col;
        let mut end_pos;
        let mut comment = String::new();
        let mut last_char = 'x';

        loop {
            end_line = self.row;
            end_col = self.col;
            end_pos = self.ifs.get_position();

            // Get and move to the nect char
            let c = match self._getc() {
                Some(c) => c,
                None => {
                    // Reached unfinished comment, return an error
                    return Some(self._tok_err(
                        Location::new(
                            beg_pos,
                            end_pos,
                            self.ifs.get_file_id(),
                            beg_line,
                            end_line,
                            beg_col,
                            end_col,
                        ),
                        "unfinished comment".to_string(),
                    ));
                }
            };

            // End of single line comment
            if c == '\n' && !is_multiline {
                break;
            }

            // End of multiline comment
            if c == '/' && last_char == '*' && is_multiline {
                comment.pop();
                break;
            }

            comment.push(c);
            last_char = c;
        }

        let loc = Location::new(
            beg_pos,
            end_pos,
            self.ifs.get_file_id(),
            beg_line,
            end_line,
            beg_col,
            end_col,
        );
        Some(Token {
            val: TokenValue::Comment(comment),
            loc,
        })
    }

    fn _read_next_tok_string(&mut self) -> Token {
        let beg_line = self.row;
        let beg_col = self.col;
        let beg_pos = self.ifs.get_position();
        let is_single_quote = self._getc().unwrap() == '\'';

        let mut end_line;
        let mut end_col;
        let mut end_pos;
        let mut is_escaped = false;
        let mut s = String::new();

        loop {
            end_line = self.row;
            end_col = self.col;
            end_pos = self.ifs.get_position();

            // Get and move to the nect char
            let c = match self._getc() {
                Some(c) => c,
                None => {
                    // Reached unfinished string, return an error
                    return self._tok_err(
                        Location::new(
                            beg_pos,
                            end_pos,
                            self.ifs.get_file_id(),
                            beg_line,
                            end_line,
                            beg_col,
                            end_col,
                        ),
                        "unfinished comment".to_string(),
                    );
                }
            };

            // Reached end of 'xxx' string
            if !is_escaped && is_single_quote && c == '\'' {
                break;
            }

            // Reached end of "xxx" string
            if !is_escaped && !is_single_quote && c == '"' {
                break;
            }

            is_escaped = !is_escaped && c == '\\';
            s.push(c);
        }

        let loc = Location::new(
            beg_pos,
            end_pos,
            self.ifs.get_file_id(),
            beg_line,
            end_line,
            beg_col,
            end_col,
        );

        let s = if self.decode_tok_strings {
            decode_string_literal(&s)
        } else {
            s
        };

        if is_single_quote && self.single_quote_literal_is_char {
            if s.len() != 1 {
                return self._tok_err(
                    loc,
                    "a char literal must have a single character".to_string(),
                );
            }
            Token {
                val: TokenValue::CharLiteral(s.chars().next().unwrap()),
                loc,
            }
        } else {
            Token {
                val: TokenValue::StringLiteral(s),
                loc,
            }
        }
    }
}

impl CompilerDiagnosticsEmitter for Lexer {
    fn get_diagnostics_emitter_mut(&mut self) -> &mut CompilerDiagnostics {
        &mut self.diagnostics
    }

    fn get_diagnostics_emitter_name(&self) -> &str {
        "Lexer"
    }
}

#[cfg(test)]
mod tests {
    use iostreams::{
        location::RawFileIdentifier, source_stream::StringSourceStream,
        source_streams_set::SourceStreamsSet,
    };

    use super::*;
    use std::fmt::Write;

    fn lexer_from_str(s: &str) -> Lexer {
        Lexer::new(Box::new(StringSourceStream::from_str(
            s,
            RawFileIdentifier(0),
        )))
    }

    fn loc_fmt(loc: Location) -> String {
        format!(
            "{}:{}[{}]-{}:{}[{}]",
            loc.beg_line(),
            loc.beg_col(),
            loc.beg_pos().0,
            loc.end_line(),
            loc.end_col(),
            loc.end_pos().0
        )
    }

    fn lexer_get_tokens_str(l: &mut Lexer, beg_pos: LexerPostion) -> String {
        // Save the posiion.
        let old_pos = l.get_position();
        l.set_position(beg_pos);

        // Generate the tokens.
        let mut res = String::new();
        loop {
            let tok = l.get_token();
            if res.len() > 0 {
                write!(res, " ").unwrap();
            }
            write!(res, "{}", tok).unwrap();
            if tok.is_eof() {
                break;
            }
        }

        // Restore the position.
        l.set_position(old_pos);
        res
    }

    #[test]
    fn test_lex_ints() {
        let mut l = lexer_from_str(" 78   7  245\n6\t\t 23  49");
        let pos_beg = l.get_position();

        let tok = l.get_token();
        assert_eq!(tok.get_int(), Some(78));
        assert_eq!(loc_fmt(tok.loc), "1:2[1]-1:3[2]");

        let tok = l.get_token();
        assert_eq!(tok.get_int(), Some(7));
        assert_eq!(loc_fmt(tok.loc), "1:7[6]-1:7[6]");

        let tok = l.get_token();
        assert_eq!(tok.get_int(), Some(245));
        assert_eq!(loc_fmt(tok.loc), "1:10[9]-1:12[11]");

        assert_eq!(
            lexer_get_tokens_str(&mut l, pos_beg),
            "78 7 245 6 23 49 <<EOF>>"
        );

        let tok = l.get_token();
        assert_eq!(tok.get_int(), Some(6));
        assert_eq!(loc_fmt(tok.loc), "2:1[13]-2:1[13]");

        let tok = l.get_token();
        assert_eq!(tok.get_int(), Some(23));
        assert_eq!(loc_fmt(tok.loc), "2:5[17]-2:6[18]");

        let tok = l.get_token();
        assert_eq!(tok.get_int(), Some(49));
        assert_eq!(loc_fmt(tok.loc), "2:9[21]-2:10[22]");

        let tok = l.get_token();
        assert!(tok.is_eof());
        assert_eq!(loc_fmt(tok.loc), "2:11[23]-2:11[23]");
    }

    #[test]
    fn test_lex_floats() {
        let mut l = lexer_from_str("  23.750 42.0\n.125 \t.005");
        assert_eq!(l.get_token().get_float(), Some(23.75));
        assert_eq!(l.get_token().get_float(), Some(42.0));
        assert_eq!(l.get_token().get_float(), Some(0.125));
        assert_eq!(l.get_token().get_float(), Some(0.005));
        assert!(l.get_token().is_eof());
    }

    #[test]
    fn test_lex_identifier() {
        let mut l = lexer_from_str("  bar bol \n foo\t\tfoox nxun bor45 _v_x_");
        l.add_keyword("foo");

        assert_eq!(l.get_token().get_identifier(), Some("bar"));
        assert_eq!(l.get_token().get_identifier(), Some("bol"));
        assert_eq!(l.get_token().get_keyword(), Some("foo"));
        assert_eq!(l.get_token().get_identifier(), Some("foox"));
        assert_eq!(l.get_token().get_identifier(), Some("nxun"));
        assert_eq!(l.get_token().get_identifier(), Some("bor45"));
        assert_eq!(l.get_token().get_identifier(), Some("_v_x_"));
        assert!(l.get_token().is_eof());
    }

    #[test]
    fn test_lex_symbols() {
        let mut l = lexer_from_str("+ =- &foo&&foo >= *foo . .5");
        l.set_skip_error_tokens(false);
        l.add_symbol_val(TokenValue::sym_plus());
        l.add_symbol("=");
        l.add_symbol("==");
        l.add_symbol("-");
        l.add_symbol("&");
        l.add_symbol("&foo&");
        l.add_symbol("*foo*");
        l.add_symbol(".");

        let tok = l.get_token();
        assert_eq!(tok.val, TokenValue::sym_plus());
        assert_eq!(loc_fmt(tok.loc), "1:1[0]-1:1[0]");

        let tok = l.get_token();
        assert_eq!(tok.get_symbol(), Some("="));
        assert_eq!(loc_fmt(tok.loc), "1:3[2]-1:3[2]");

        let tok = l.get_token();
        assert_eq!(tok.get_symbol(), Some("-"));
        assert_eq!(loc_fmt(tok.loc), "1:4[3]-1:4[3]");

        let tok = l.get_token();
        assert_eq!(tok.get_symbol(), Some("&foo&"));
        assert_eq!(loc_fmt(tok.loc), "1:6[5]-1:10[9]");

        let tok = l.get_token();
        assert_eq!(tok.get_symbol(), Some("&"));
        assert_eq!(loc_fmt(tok.loc), "1:11[10]-1:11[10]");

        let tok = l.get_token();
        assert_eq!(tok.get_identifier(), Some("foo"));
        assert_eq!(loc_fmt(tok.loc), "1:12[11]-1:14[13]");

        let tok = l.get_token();
        assert!(tok.is_error());
        assert_eq!(loc_fmt(tok.loc), "1:16[15]-1:16[15]");

        let tok = l.get_token();
        assert_eq!(tok.get_symbol(), Some("="));
        assert_eq!(loc_fmt(tok.loc), "1:17[16]-1:17[16]");

        let tok = l.get_token();
        assert!(tok.is_error());
        assert_eq!(loc_fmt(tok.loc), "1:19[18]-1:19[18]");

        let tok = l.get_token();
        assert_eq!(tok.get_identifier(), Some("foo"));
        assert_eq!(loc_fmt(tok.loc), "1:20[19]-1:22[21]");

        let tok = l.get_token();
        assert_eq!(tok.get_symbol(), Some("."));
        assert_eq!(loc_fmt(tok.loc), "1:24[23]-1:24[23]");

        let tok = l.get_token();
        assert_eq!(loc_fmt(tok.loc), "1:26[25]-1:27[26]");

        let tok = l.get_token();
        assert!(tok.is_eof());
        assert_eq!(loc_fmt(tok.loc), "1:28[27]-1:28[27]");
    }

    #[test]
    fn test_lex_c_comments() {
        let mut l = lexer_from_str("/*abc*/  /**/ //blop\n//\n// foot");
        l.set_skip_comment_tokens(false);
        l.set_support_c_inline_comment(true);
        l.set_support_c_multiline_comment(true);

        let tok = l.get_token();
        assert_eq!(tok.get_comment(), Some("abc"));
        assert_eq!(loc_fmt(tok.loc), "1:1[0]-1:7[6]");

        let tok = l.get_token();
        assert_eq!(tok.get_comment(), Some(""));
        assert_eq!(loc_fmt(tok.loc), "1:10[9]-1:13[12]");

        let tok = l.get_token();
        assert_eq!(tok.get_comment(), Some("blop"));
        assert_eq!(loc_fmt(tok.loc), "1:15[14]-1:21[20]");

        let tok = l.get_token();
        assert_eq!(tok.get_comment(), Some(""));
        assert_eq!(loc_fmt(tok.loc), "2:1[21]-2:3[23]");

        let tok = l.get_token();
        assert!(tok.is_error());
        assert_eq!(loc_fmt(tok.loc), "3:1[24]-3:8[31]");

        let tok = l.get_token();
        assert!(tok.is_eof());
        assert_eq!(loc_fmt(tok.loc), "3:8[31]-3:8[31]");
    }

    #[test]
    fn test_lex_c_char() {
        let ss = SourceStreamsSet::with_unique_raw_source_string(
            " 'x' 'bd' '\\n' '\\\\' '\\'' 'x".to_string(),
        );
        let mut l = Lexer::new(ss.open_main_stream());
        l.get_diagnostics_emitter_mut().enable_reduced_log();
        l.set_support_single_quote_strings(true);
        l.set_support_double_quote_strings(true);
        l.set_single_quote_literal_is_char(true);
        l.set_decode_tok_strings(true);

        let tok = l.get_token();
        assert_eq!(tok.get_char_literal(), Some('x'));
        assert_eq!(loc_fmt(tok.loc), "1:2[1]-1:4[3]");

        let tok = l.get_token();
        assert!(tok.is_error());
        assert_eq!(loc_fmt(tok.loc), "1:6[5]-1:9[8]");
        assert_eq!(
            l.get_diagnostics_emitter_mut().logs_to_string(&ss),
            "Lexer: Error: a char literal must have a single character at (source):1:6-9: `'bd'`\n"
        );

        let tok = l.get_token();
        assert_eq!(tok.get_char_literal(), Some('\n'));
        assert_eq!(loc_fmt(tok.loc), "1:11[10]-1:14[13]");

        let tok = l.get_token();
        assert_eq!(tok.get_char_literal(), Some('\\'));
        assert_eq!(loc_fmt(tok.loc), "1:16[15]-1:19[18]");

        let tok = l.get_token();
        assert_eq!(tok.get_char_literal(), Some('\''));
        assert_eq!(loc_fmt(tok.loc), "1:21[20]-1:24[23]");

        let tok = l.get_token();
        assert!(tok.is_error());
        assert_eq!(loc_fmt(tok.loc), "1:26[25]-1:28[27]");
        assert_eq!(
            l.get_diagnostics_emitter_mut().logs_to_string(&ss),
            "Lexer: Error: unfinished comment at (source):1:26-28: `'x`\n"
        );

        let tok = l.get_token();
        assert!(tok.is_eof());
        assert_eq!(loc_fmt(tok.loc), "1:28[27]-1:28[27]");
    }

    #[test]
    fn test_lex_c_string() {
        let ss = SourceStreamsSet::with_unique_raw_source_string(
            " \"foo\" \"\" \"\\\\x\\n\\\"\" \"".to_string(),
        );
        let mut l = Lexer::new(ss.open_main_stream());
        l.get_diagnostics_emitter_mut().enable_reduced_log();
        l.set_support_single_quote_strings(true);
        l.set_support_double_quote_strings(true);
        l.set_single_quote_literal_is_char(true);
        l.set_decode_tok_strings(true);

        let tok = l.get_token();
        assert_eq!(tok.get_string_literal(), Some("foo"));
        assert_eq!(loc_fmt(tok.loc), "1:2[1]-1:6[5]");

        let tok = l.get_token();
        assert_eq!(tok.get_string_literal(), Some(""));
        assert_eq!(loc_fmt(tok.loc), "1:8[7]-1:9[8]");

        let tok = l.get_token();
        assert_eq!(tok.get_string_literal(), Some("\\x\n\""));
        assert_eq!(loc_fmt(tok.loc), "1:11[10]-1:19[18]");

        let tok = l.get_token();
        assert!(tok.is_error());
        assert_eq!(loc_fmt(tok.loc), "1:21[20]-1:22[21]");
        assert_eq!(
            l.get_diagnostics_emitter_mut().logs_to_string(&ss),
            "Lexer: Error: unfinished comment at (source):1:21-22: `\"`\n"
        );

        let tok = l.get_token();
        assert!(tok.is_eof());
        assert_eq!(loc_fmt(tok.loc), "1:22[21]-1:22[21]");
    }
}

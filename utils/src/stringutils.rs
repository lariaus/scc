// Encode a normal string to a string literal.
pub fn encode_string_literal(s: &str) -> String {
    let mut res = String::new();
    for c in s.chars() {
        if c == '\n' {
            res.push('\\');
            res.push('n');
        } else if c == '\t' {
            res.push('\\');
            res.push('t');
        } else if c == '\\' {
            res.push('\\');
            res.push('\\');
        } else if c == '"' {
            res.push('\\');
            res.push('"');
        } else {
            res.push(c);
        }
    }

    res
}

// Decode a string literal to the represented string.
pub fn decode_string_literal(s: &str) -> String {
    let mut is_escaped = false;
    let mut res = String::new();

    for c in s.chars() {
        if is_escaped {
            is_escaped = false;
            if c == 'n' {
                res.push('\n');
            } else if c == 't' {
                res.push('\t');
            } else if c == '\\' {
                res.push('\\');
            } else if c == '\'' {
                res.push('\'');
            } else if c == '"' {
                res.push('"');
            }
        } else if c == '\\' {
            is_escaped = true;
        } else {
            res.push(c);
        }
    }

    assert!(!is_escaped, "end of string with escaped char");
    res
}

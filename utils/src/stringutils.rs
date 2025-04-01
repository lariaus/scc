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

// Split string at each whitespace, but also support commas.
pub fn split_with_commas(s: &str) -> Vec<&str> {
    let mut res = Vec::new();

    let mut in_word = false;
    let mut word_is_string = false;
    let mut word_start = 0;

    for (bidx, c) in s.char_indices() {
        if !in_word {
            // Looking for next word
            if !c.is_ascii_whitespace() {
                in_word = true;
                if c == '"' {
                    word_is_string = true;
                    word_start = bidx + 1;
                } else {
                    word_is_string = false;
                    word_start = bidx;
                }
            }
            continue;
        }

        if word_is_string {
            // Looking for string end `"`.
            if c == '"' {
                res.push(&s[word_start..bidx]);
                in_word = false;
            }
            continue;
        }

        // Looking for word end
        if c.is_ascii_whitespace() {
            res.push(&s[word_start..bidx]);
            in_word = false;
        }
    }

    if in_word {
        // Add the last current word.
        assert!(!word_is_string, "unfinished string");
        res.push(&s[word_start..]);
    }

    res
}

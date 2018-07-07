use std::collections::hash_map::*;

use super::token::{ReservedWord, Token};

#[derive(PartialEq)]
enum TokenState {
    WORD,
    OPERATOR,
}

fn is_op_prefix(c: char) -> bool {
    match c {
        '&' | '|' | '<' | '>' | ';' | '(' | ')' => true,
        _ => false,
    }
}

fn add_token(reserved_word_table: &HashMap<&str, ReservedWord>, tokens: &mut Vec<Token>, s: &str) {
    if s.len() > 0 {
        if let Some(keyword) = reserved_word_table.get(s) {
            tokens.push(Token::Reserved(*keyword));
        } else {
            tokens.push(Token::Word(s.to_owned()));
        }
    }
}

/// tokenize the input string
/// if `delimited` is true then tokenize one delimited structure, instead of multiple words
pub fn tokenize(input: &str, delimited: bool) -> (Vec<Token>, bool) {
    let operator_table: HashMap<&str, Token> = hash_map!{
        "<" => Token::LESS,
        "<<" => Token::DLESS,
        "<<-" => Token::DLESSDASH,
        "<&" => Token::LESSAND,
        "<>" => Token::LESSGREAT,
        ">" => Token::GREAT,
        ">>" => Token::DGREAT,
        ">&" => Token::GREATAND,
        ">|" => Token::CLOBBER,
        "&&" => Token::AND,
        "||" => Token::OR,
        ";;" => Token::DSEMI,
        // Non-operators, but prefixes
        "|" => Token::PIPE,
        "&" => Token::ASYNC,
        ";" => Token::SEMI,
        "(" => Token::LPAREN,
        ")" => Token::RPAREN,
    };
    let reserved_word_table: HashMap<&str, ReservedWord> = hash_map!{
        "if" => ReservedWord::IF,
        "elif" => ReservedWord::ELIF,
        "then" => ReservedWord::THEN,
        "else" => ReservedWord::ELSE,
        "fi" => ReservedWord::FI,
        "do" => ReservedWord::DO,
        "done" => ReservedWord::DONE,
        "case" => ReservedWord::CASE,
        "esac" => ReservedWord::ESAC,
        "while" => ReservedWord::WHILE,
        "until" => ReservedWord::UNTIL,
        "for" => ReservedWord::FOR,
        "{" => ReservedWord::LBRACE,
        "}" => ReservedWord::RBRACE,
        "!" => ReservedWord::BANG,
        "in" => ReservedWord::IN,
    };

    let mut chars = input.chars().peekable();
    let mut token = String::new();
    let mut tokens: Vec<Token> = Vec::new();
    let mut state = TokenState::WORD;

    let mut skip_char;
    let mut unterminated = false;
    let mut param_nesting = 0;
    let mut command_nesting = 0;
    let mut arithmetic_nesting = 0;
    let mut quote_nesting = 0;

    loop {
        skip_char = false;
        let mut c;
        if let Some(new_c) = chars.next() {
            c = new_c;
        } else {
            break;
        }
        if state == TokenState::OPERATOR {
            let mut new_op = token.clone();
            new_op.push(c);

            // Rule 3
            if !operator_table.contains_key(new_op.as_str()) {
                let op: Token = (*operator_table.get(token.as_str()).unwrap()).clone();
                tokens.push(op);
                token.clear();

                // Fall-through and parse the current character
                state = TokenState::WORD;
            }
        }
        if state == TokenState::WORD {
            if c == '\\' {
                let lookahead = chars.peek().map(|c| *c);
                if let Some(next) = lookahead {
                    match next {
                        '$' | '`' | '"' | '\\' | '\n' | '\'' => {
                            token.push(c);
                            chars.next();
                            c = next;
                        }
                        '0' | 'a' | 'b' | 't' | 'n' | 'v' | 'f' | 'r' => {
                            chars.next();
                            c = match next {
                                '0' => '\0',
                                'a' => '\x07',
                                'b' => '\x08',
                                't' => '\t',
                                'n' => '\n',
                                'v' => '\x0B',
                                'f' => '\x0C',
                                'r' => '\r',
                                _ => unreachable!(),
                            };
                        }
                        // Rule 4a
                        _ => {
                            chars.next();
                            c = next;
                        }
                    }
                } else {
                    unterminated = true;
                    skip_char = true;
                }
            }
            // Rule 4b
            else if c == '\"' {
                quote_nesting ^= 1;
            }
            // Rule 4c
            else if c == '\'' {
                token.push(c);
                unterminated = true;
                while let Some(next) = chars.next() {
                    if next == '\'' {
                        unterminated = false;
                        c = next;
                        break;
                    }
                    token.push(next);
                }
            }
            // Rule 5
            else if c == '$' {
                let lookahead1 = chars.peek().map(|c| *c);
                if let Some(next1) = lookahead1 {
                    if next1 == '{' {
                        token.push(c);
                        chars.next();
                        c = next1;
                        param_nesting += 1;
                    } else if next1 == '(' {
                        token.push(c);
                        chars.next();
                        c = next1;
                        let lookahead2 = chars.peek().map(|c| *c);
                        if let Some(next2) = lookahead2 {
                            if next2 == '(' {
                                token.push(next1);
                                chars.next();
                                c = next2;
                                arithmetic_nesting += 1;
                            } else {
                                command_nesting += 1;
                            }
                        } else {
                            command_nesting += 1;
                        }
                    }
                }
            } else if c == '}' && param_nesting > 0 {
                param_nesting -= 1;
            } else if c == ')' && (arithmetic_nesting > 0 || command_nesting > 0) {
                let lookahead = chars.peek().map(|c| *c);
                if let Some(next) = lookahead {
                    if next == ')' {
                        if arithmetic_nesting > 0 {
                            arithmetic_nesting -= 1;
                        }
                        token.push(c);
                        chars.next();
                        c = next;
                    } else if command_nesting > 0 {
                        command_nesting -= 1;
                    }
                } else if command_nesting > 0 {
                    command_nesting -= 1;
                }
            }
            // Rule 6
            else if is_op_prefix(c)
                && arithmetic_nesting + command_nesting + param_nesting + quote_nesting == 0
            {
                if c == '>' || c == '<' {
                    if let Ok(n) = token.parse::<i32>() {
                        tokens.push(Token::IONumber(n));
                    } else {
                        add_token(&reserved_word_table, &mut tokens, &token);
                    }
                } else {
                    add_token(&reserved_word_table, &mut tokens, &token);
                }
                token.clear();
                state = TokenState::OPERATOR;
            }
            // Rule 7
            else if c == '\n'
                && arithmetic_nesting + command_nesting + param_nesting + quote_nesting == 0
            {
                add_token(&reserved_word_table, &mut tokens, &token);
                tokens.push(Token::LINEBREAK);
                token.clear();
                skip_char = true;
            }
            // Rule 8
            else if c.is_whitespace()
                && arithmetic_nesting + command_nesting + param_nesting + quote_nesting == 0
            {
                add_token(&reserved_word_table, &mut tokens, &token);
                token.clear();
                skip_char = true;
            }
            // Rule 10
            else if c == '#' {
                add_token(&reserved_word_table, &mut tokens, &token);
                token.clear();

                while let Some(next) = chars.next() {
                    if next == '\n' {
                        break;
                    }
                }
                tokens.push(Token::LINEBREAK);
                skip_char = true;
            }
        }
        if !skip_char {
            token.push(c);
        }
        if delimited && arithmetic_nesting + command_nesting + param_nesting + quote_nesting == 0 {
            break;
        }
    }
    if state == TokenState::OPERATOR {
        let op = operator_table.get(token.as_str()).unwrap().clone();
        tokens.push(op);
    } else {
        add_token(&reserved_word_table, &mut tokens, &token);
    }

    unterminated |= arithmetic_nesting + command_nesting + param_nesting + quote_nesting > 0;
    (tokens, unterminated)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn word(s: &str) -> Token {
        Token::Word(s.to_string())
    }

    #[test]
    fn tokenize_single_word() {
        let (tokens, _) = tokenize("cmd", false);
        assert_eq!(tokens, vec![word("cmd")]);
    }

    #[test]
    fn tokenize_multiple_words() {
        let (tokens, _) = tokenize("hello world", false);
        assert_eq!(tokens, vec![word("hello"), word("world")]);
    }

    #[test]
    fn tokenize_leading_ws() {
        let (tokens, _) = tokenize("    cmd", false);
        assert_eq!(tokens, vec![word("cmd")]);
    }

    #[test]
    fn tokenize_trailing_ws() {
        let (tokens, _) = tokenize("cmd    ", false);
        assert_eq!(tokens, vec![word("cmd")]);
    }

    #[test]
    fn tokenize_escaped_ws() {
        let (tokens, _) = tokenize(r#"hello\ world"#, false);
        assert_eq!(tokens, vec![word("hello world")]);
    }

    #[test]
    fn tokenize_empty_cmd() {
        let (tokens, _) = tokenize("", false);
        assert_eq!(tokens, Vec::new());
    }

    #[test]
    fn tokenize_double_quote() {
        let input = r#""hello world""#;
        let (t, _) = tokenize(input, false);
        assert_eq!(t, vec![word(input)]);
    }

    #[test]
    fn tokenize_string_with_escaped() {
        let input = r#""hello \"""#;
        let (t, _) = tokenize(input, false);
        assert_eq!(t, vec![word(input)]);
    }

    #[test]
    fn tokenize_unterminated_double_quote() {
        let (_, unterminated) = tokenize(r#""hello"#, false);
        assert!(unterminated);
    }

    #[test]
    fn tokenize_literal_string() {
        let input = "'hello world'";
        let (t, _) = tokenize(input, false);
        assert_eq!(t, vec![word(input)]);
    }

    #[test]
    fn tokenize_untermianted_single_quote() {
        let (_, unterminated) = tokenize("'hello", false);
        assert!(unterminated);
    }

    #[test]
    fn tokenize_var() {
        let input = "$ABC";
        let (t, _) = tokenize(input, false);
        assert_eq!(t, vec![word(input)]);
    }

    #[test]
    fn tokenize_escaped_var() {
        let input = r#"\$ABC"#;
        let (t, _) = tokenize(input, false);
        assert_eq!(t, vec![word(input)]);
    }

    #[test]
    fn tokenize_escaped_string() {
        let input = r#"\"hello"#;
        let (t, _) = tokenize(input, false);
        assert_eq!(t, vec![word(input)]);
    }

    #[test]
    fn tokenize_number() {
        let (t, _) = tokenize("123", false);
        assert_eq!(t, vec![word("123")]);
    }

    #[test]
    fn tokenize_redirects() {
        let (t, _) = tokenize(">", false);
        assert_eq!(t, vec![Token::GREAT]);

        let (t, _) = tokenize(">>", false);
        assert_eq!(t, vec![Token::DGREAT]);

        let (t, _) = tokenize(">&", false);
        assert_eq!(t, vec![Token::GREATAND]);

        let (t, _) = tokenize(">|", false);
        assert_eq!(t, vec![Token::CLOBBER]);

        let (t, _) = tokenize("<", false);
        assert_eq!(t, vec![Token::LESS]);

        let (t, _) = tokenize("<<", false);
        assert_eq!(t, vec![Token::DLESS]);

        let (t, _) = tokenize("<<-", false);
        assert_eq!(t, vec![Token::DLESSDASH]);

        let (t, _) = tokenize("<&", false);
        assert_eq!(t, vec![Token::LESSAND]);

        let (t, _) = tokenize("<>", false);
        assert_eq!(t, vec![Token::LESSGREAT]);
    }

    #[test]
    fn tokenize_numbered_redirect() {
        let (t, _) = tokenize("3<", false);
        assert_eq!(t, vec![Token::IONumber(3), Token::LESS]);
    }

    #[test]
    fn tokenize_parameter() {
        let (t, _) = tokenize("A${B}C", false);
        assert_eq!(t, vec![word("A${B}C")]);
    }

    #[test]
    fn tokenize_unterminated_parameter() {
        let (_, unterminated) = tokenize("${A", false);
        assert!(unterminated);
    }

    #[test]
    fn tokenize_command() {
        let input = "$(echo \"foo\")";
        let (t, _) = tokenize(input, false);
        assert_eq!(t, vec![word(input)]);
    }

    #[test]
    fn tokenize_unterminated_command() {
        let (_, unterminated) = tokenize("$(A", false);
        assert!(unterminated);
    }

    #[test]
    fn tokenize_arithmetic() {
        let input = "$((1 + 1))";
        let (t, _) = tokenize(input, false);
        assert_eq!(t, vec![word(input)]);
    }

    #[test]
    fn tokenize_unterminated_arithmetic() {
        let (_, unterminated) = tokenize("$((1", false);
        assert!(unterminated);
    }

    #[test]
    fn tokenize_parameter_in_command() {
        let input = "$(echo ${A} B)";
        let (t, _) = tokenize(input, false);
        assert_eq!(t, vec![word(input)]);
    }

    #[test]
    fn tokenize_comment() {
        let input = "# abc";
        let (t, _) = tokenize(input, false);
        assert_eq!(t, vec![Token::LINEBREAK]);
    }

    #[test]
    fn tokenize_line_continuation() {
        let input = r#"cmd \"#;
        let (t, unterminated) = tokenize(input, false);
        assert!(unterminated);
        assert_eq!(t, vec![word("cmd")]);
    }

    #[test]
    fn tokenize_special_character() {
        let input = r#"a\nb"#;
        let (t, _) = tokenize(input, false);

        assert_eq!(t, vec![word("a\nb")]);
    }
}

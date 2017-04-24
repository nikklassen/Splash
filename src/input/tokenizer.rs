use super::token::*;

macro_rules! accept {
    ( $reader: ident, $ret_val: expr ) => {
        { $reader.advance(); return Ok(Some($ret_val)); }
    }
}


struct CharReader {
    source: Vec<char>,
    pos: usize,
    current: Option<char>
}

impl CharReader {
    fn new(s: String) -> Self {
        let s_chars: Vec<_> = s.chars().collect();
        let mut first = None;
        if !s_chars.is_empty() {
            first = Some(s_chars[0]);
        }

        CharReader {
            source: s_chars,
            pos: 0,
            current: first,
        }
    }

    fn advance(&mut self) {
        if self.pos == self.source.len() - 1 {
            self.current = None
        } else {
            self.pos += 1;
            self.current = Some(self.source[self.pos]);
        }
    }

    fn backtrack(&mut self, back: usize) {
        if back > self.pos {
            panic!("Cannot backtrack forwards");
        }
        self.pos = self.pos;
        self.current = Some(self.source[self.pos]);
    }
}

fn ws_tok(reader: &mut CharReader) -> TokenResult {
    let mut has_ws = false;
    loop {
        match reader.current {
            Some('\n') => accept!(reader, Token::LineBreak),
            Some(c) if c.is_whitespace() => {
                has_ws = true;
                reader.advance();
            }
            _ => {
                return if has_ws {
                    Ok(Some(Token::Whitespace))
                } else {
                    Ok(None)
                }
            }
        }
    }
}

fn char_tok(reader: &mut CharReader, token: char, tt: Token) -> TokenResult {
    if let Some(c) = reader.current {
        if token == c {
            reader.advance();
            return Ok(Some(tt));
        }
    }
    Ok(None)
}

fn eql_tok(reader: &mut CharReader) -> TokenResult {
    char_tok(reader, '=', Token::Eql)
}

fn semi_tok(reader: &mut CharReader) -> TokenResult {
    char_tok(reader, ';', Token::Semi)
}

fn redir_tok(reader: &mut CharReader) -> TokenResult {
    match reader.current {
        Some('<') => {
            reader.advance();
            match reader.current {
                Some('<') => {
                    reader.advance();
                    match reader.current {
                        Some('-') => accept!(reader, Token::Redir(None, RedirOp::DLESSDASH)),
                        _ => Ok(Some(Token::Redir(None, RedirOp::DLESS))),
                    }
                },
                Some('>') => accept!(reader, Token::Redir(None, RedirOp::LESSGREAT)),
                Some('&') => accept!(reader, Token::Redir(None, RedirOp::LESSAND)),
                _ => Ok(Some(Token::Redir(None, RedirOp::LESS))),
            }
        },
        Some('>') => {
            reader.advance();
            match reader.current {
                Some('>') => accept!(reader, Token::Redir(None, RedirOp::DGREAT)),
                Some('|') => accept!(reader, Token::Redir(None, RedirOp::CLOBBER)),
                Some('&') => accept!(reader, Token::Redir(None, RedirOp::GREATAND)),
                _ => Ok(Some(Token::Redir(None, RedirOp::GREAT))),
            }
        },
        _ => Ok(None),
    }
}

fn amp_tok(reader: &mut CharReader) -> TokenResult {
    if !is_match!(reader.current, Some('&')) {
        return Ok(None);
    }

    reader.advance();
    match reader.current {
        Some('&') => accept!(reader, Token::And),
        _ => {
            return Ok(Some(Token::Async))
        }
    }
}

fn pipe_tok(reader: &mut CharReader) -> TokenResult {
    if !is_match!(reader.current, Some('|')) {
        return Ok(None);
    }

    reader.advance();
    match reader.current {
        Some('|') => accept!(reader, Token::Or),
        _ => {
            return Ok(Some(Token::Pipe))
        }
    }
}

fn escaped_tok(reader: &mut CharReader) -> TokenResult {
    if let Some('\\') = reader.current {
        let prev: usize = reader.pos;
        reader.advance();
        if let Some(c) = reader.current {
            reader.advance();
            return Ok(Some(Token::String(c.to_string())));
        } else {
            reader.backtrack(prev);
            return Ok(None);
        }
    }
    Ok(None)
}

fn lit_string_tok(reader: &mut CharReader) -> TokenResult {
    if !is_match!(reader.current, Some('\'')) {
        return Ok(None)
    }

    let mut contents = String::new();
    loop {
        reader.advance();
        if let Some(c) = reader.current {
            if c == '\'' {
                reader.advance();
                return Ok(Some(Token::String(contents)));
            }
            contents.push(c);
        } else {
            return Err(TokenError::Unterminated);
        }
    }
}

fn quotemark_tok(reader: &mut CharReader) -> TokenResult {
    if !is_match!(reader.current, Some('"')) {
        return Ok(None);
    }

    reader.advance();
    let tokenizers: Vec<_> = vec![escaped_tok as fn(&mut CharReader) -> TokenResult, var_tok];
    let tokens = try!(tokenize_loop(reader, tokenizers, |reader| {
        reader.current.and_then(|c| {
            if c == '"' {
                None
            } else {
                Some(c)
            }
        })
    }));

    if let Some('"') = reader.current {
        reader.advance();
        Ok(Some(Token::Quoted(tokens)))
    } else {
        Err(TokenError::Unterminated)
    }
}

fn var_tok(reader: &mut CharReader) -> TokenResult {
    if !is_match!(reader.current, Some('$')) {
        return Ok(None);
    }

    let mut ident = String::new();
    loop {
        reader.advance();
        match reader.current {
            Some(c) if c.is_alphanumeric() => {
                ident.push(c);
            },
            _ => {
                return if ident.is_empty() {
                    Ok(Some(Token::String("$".to_string())))
                } else {
                    Ok(Some(Token::Var(ident)))
                }
            },
        }
    }
}

fn num_tok(reader: &mut CharReader) -> TokenResult {
    if !is_match!(reader.current, Some(_)) {
        return Ok(None);
    }
    let c = reader.current.unwrap();
    if !c.is_digit(10) {
        return Ok(None);
    }

    let mut num = String::new();
    num.push(c);
    loop {
        reader.advance();
        let current = reader.current;
        match current {
            Some(c) if c.is_digit(10) => {
                num.push(c);
            },
            Some('<') | Some('>') => {
                let redir = try!(redir_tok(reader));
                let n = num.parse::<i32>().unwrap();
                return match redir {
                    Some(Token::Redir(_, op)) => Ok(Some(Token::Redir(Some(n), op))),
                    _ => unreachable!(),
                };
            },
            _ => {
                return Ok(Some(Token::String(num)));
            },
        }
    }
}

fn tokenize_loop<F: Fn(&mut CharReader) -> Option<char>>(
    reader: &mut CharReader,
    tokenizers: Vec<fn(&mut CharReader) -> TokenResult>,
    loop_cond: F)
-> Result<Vec<Token>, TokenError> {

    let mut tokens = Vec::<Token>::new();
    let mut word = String::new();

    while let Some(c) = loop_cond(reader) {
        let mut token = None;
        for tokenizer in tokenizers.iter() {
            token = try!(tokenizer(reader));
            if token.is_some() {
                break;
            }
        }

        if token.is_none() {
            word.push(c);
            reader.advance();
            continue;
        } else if !word.is_empty() {
            tokens.push(Token::String(word.clone()));
            word.clear();
        }
        tokens.push(token.unwrap());
    }
    if !word.is_empty() {
        tokens.push(Token::String(word.clone()));
    }

    Ok(tokens)
}

pub fn tokenize(s: &str) -> Result<Vec<Token>, TokenError> {
    let mut reader = CharReader::new(s.to_string());
    let tokenizers: Vec<_> = vec!(
        ws_tok as fn(&mut CharReader) -> TokenResult,
        escaped_tok,
        lit_string_tok,
        quotemark_tok,
        redir_tok,
        num_tok,
        var_tok,
        eql_tok,
        pipe_tok,
        amp_tok,
        semi_tok,
    );

    tokenize_loop(&mut reader, tokenizers, |reader| reader.current)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn to_string(s: &str) -> Token {
        Token::String(s.to_string())
    }

    #[test]
    fn parse_single_word() {
        let word = tokenize("cmd").unwrap();
        assert_eq!(word, vec![to_string("cmd")]);
    }

    #[test]
    fn parse_multiple_words() {
        let word = tokenize("hello world").unwrap();
        assert_eq!(word, vec![to_string("hello"), Token::Whitespace, to_string("world")]);
    }

    #[test]
    fn parse_leading_ws() {
        let word = tokenize("    cmd").unwrap();
        assert_eq!(word, vec![Token::Whitespace, to_string("cmd")]);
    }

    #[test]
    fn parse_trailing_ws() {
        let word = tokenize("cmd    ").unwrap();
        assert_eq!(word, vec![to_string("cmd"), Token::Whitespace]);
    }

    #[test]
    fn parse_empty_cmd() {
        let word = tokenize("").unwrap();
        assert_eq!(word, Vec::new());
    }

    #[test]
    fn parse_quotemark() {
        let t = tokenize(r#""hello world""#).unwrap();
        assert_eq!(t, vec![Token::Quoted(
                vec![to_string("hello world")]
                )]);
    }

    #[test]
    fn parse_string_with_escaped() {
        let t = tokenize(r#""hello \"""#).unwrap();
        assert_eq!(t, vec![Token::Quoted(
                vec![to_string("hello "), to_string("\"")]
                )]);
    }

    #[test]
    fn parse_unterminated_string() {
        let t = tokenize(r#""hello"#);
        assert!(t.is_err());
    }

    #[test]
    fn parse_literal_string() {
        let t = tokenize("'hello world'").unwrap();
        assert_eq!(t, vec![to_string("hello world")]);
    }

    #[test]
    fn parse_literal_string_unclosed_fails() {
        let t = tokenize("'hello");
        assert!(t.is_err());
    }

    #[test]
    fn parse_var() {
        let t = tokenize("$ABC").unwrap();
        assert_eq!(t, vec![Token::Var("ABC".to_string())]);
    }

    #[test]
    fn parse_escaped_var() {
        let t = tokenize(r#"\$ABC"#).unwrap();
        assert_eq!(t, vec![to_string("$"), to_string("ABC")]);
    }

    #[test]
    fn parse_escaped_string() {
        let t = tokenize(r#"\"hello"#).unwrap();
        assert_eq!(t, vec![to_string("\""), to_string("hello")]);
    }

    #[test]
    fn parse_number() {
        let t = tokenize("123").unwrap();
        assert_eq!(t, vec![to_string("123")]);
    }

    #[test]
    fn parse_redirects() {
        let mut t = tokenize(">").unwrap();
        assert_eq!(t, vec![Token::Redir(None, RedirOp::GREAT)]);

        t = tokenize(">>").unwrap();
        assert_eq!(t, vec![Token::Redir(None, RedirOp::DGREAT)]);

        t = tokenize(">&").unwrap();
        assert_eq!(t, vec![Token::Redir(None, RedirOp::GREATAND)]);

        t = tokenize(">|").unwrap();
        assert_eq!(t, vec![Token::Redir(None, RedirOp::CLOBBER)]);

        t = tokenize("<").unwrap();
        assert_eq!(t, vec![Token::Redir(None, RedirOp::LESS)]);

        t = tokenize("<<").unwrap();
        assert_eq!(t, vec![Token::Redir(None, RedirOp::DLESS)]);

        t = tokenize("<<-").unwrap();
        assert_eq!(t, vec![Token::Redir(None, RedirOp::DLESSDASH)]);

        t = tokenize("<&").unwrap();
        assert_eq!(t, vec![Token::Redir(None, RedirOp::LESSAND)]);

        t = tokenize("<>").unwrap();
        assert_eq!(t, vec![Token::Redir(None, RedirOp::LESSGREAT)]);
    }

    #[test]
    fn parse_numbered_redirect() {
        let t = tokenize("3<").unwrap();
        assert_eq!(t, vec![Token::Redir(Some(3), RedirOp::LESS)]);
    }
}

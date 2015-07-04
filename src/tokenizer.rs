#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum AST {
    Whitespace,
    String(String),
    Quoted(Vec<AST>),
    Var(String),
}

type ASTResult = Result<Option<AST>, String>;

struct CharReader {
    source: Vec<char>,
    pos: usize,
    current: Option<char>
}

impl CharReader {
    fn new(s: String) -> Self {
        let s_chars: Vec<_> = s.chars().collect();
        println!("chars: {:?}", s_chars);
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

fn ws_tok(reader: &mut CharReader) -> ASTResult {
    let mut has_ws = false;
    loop {
        match reader.current {
            Some(c) if c.is_whitespace() => {
                has_ws = true;
                reader.advance();
            }
            _ => {
                return if has_ws {
                    Ok(Some(AST::Whitespace))
                } else {
                    Ok(None)
                }
            }
        }
    }
}

fn escaped_tok(reader: &mut CharReader) -> ASTResult {
    if let Some('\\') = reader.current {
        println!("Found backslash");
        let prev: usize = reader.pos;
        reader.advance();
        if let Some(c) = reader.current {
            println!("Found escaped {}", c);
            reader.advance();
            return Ok(Some(AST::String(c.to_string())));
        } else {
            reader.backtrack(prev);
            return Ok(None);
        }
    }
    Ok(None)
}

fn lit_string_tok(reader: &mut CharReader) -> ASTResult {
    if !is_match!(reader.current, Some('\'')) {
        return Ok(None)
    }

    let mut contents = String::new();
    loop {
        reader.advance();
        if let Some(c) = reader.current {
            if c == '\'' {
                reader.advance();
                return Ok(Some(AST::String(contents)));
            }
            contents.push(c);
        } else {
            return Err("Unterminated string".to_string());
        }
    }
}

fn quotemark_tok(reader: &mut CharReader) -> ASTResult {
    if let Some('"') = reader.current {
        reader.advance();
        println!("Current {:?}", reader.current);
        let tokenizers: Vec<_> = vec![escaped_tok as fn(&mut CharReader) -> ASTResult, var_tok];
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
            Ok(Some(AST::Quoted(tokens)))
        } else {
            Err("Unterminated string".to_string())
        }
    } else {
        Ok(None)
    }
}

fn var_tok(reader: &mut CharReader) -> ASTResult {
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
                    Ok(Some(AST::String("$".to_string())))
                } else {
                    Ok(Some(AST::Var(ident)))
                }
            },
        }
    }
}

fn tokenize_loop<F: Fn(&mut CharReader) -> Option<char>>(
    reader: &mut CharReader,
    tokenizers: Vec<fn(&mut CharReader) -> ASTResult>,
    loop_cond: F)
-> Result<Vec<AST>, String> {

    let mut tokens = Vec::<AST>::new();
    let mut word = String::new();

    let mut f = true;
    while let Some(c) = loop_cond(reader) {
        //println!("c: '{}'", c);
        let mut token = None;
        for tokenizer in tokenizers.iter() {
            token = try!(tokenizer(reader));
            if token.is_some() {
                break;
            }
        }
        if f {
            println!("Token: {:?}", token);
            f = false;
        }
        if token.is_none() {
            word.push(c);
            reader.advance();
            continue;
        } else if !word.is_empty() {
            tokens.push(AST::String(word.clone()));
            word.clear();
        }
        tokens.push(token.unwrap());
    }
    if !word.is_empty() {
        tokens.push(AST::String(word.clone()));
    }

    Ok(tokens)
}

pub fn tokenize(s: &str) -> Result<Vec<AST>, String> {
    let mut reader = CharReader::new(s.to_string());
    let tokenizers: Vec<_> = vec![ws_tok as fn(&mut CharReader) -> ASTResult, escaped_tok, lit_string_tok, quotemark_tok, var_tok];
    tokenize_loop(&mut reader, tokenizers, |reader| reader.current)
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;
    use super::*;

    fn to_string(s: &str) -> AST {
println!("to_string");
        AST::String(s.to_string())
    }

    #[test]
    fn parse_single_word() {
println!("parse_single_word");
        let word = tokenize("cmd").unwrap();
        assert_eq!(word, vec![to_string("cmd")]);
    }

    #[test]
    fn parse_multiple_words() {
println!("parse_multiple_words");
        let word = tokenize("hello world").unwrap();
        assert_eq!(word, vec![to_string("hello"), AST::Whitespace, to_string("world")]);
    }

    #[test]
    fn parse_leading_ws() {
println!("parse_leading_ws");
        let word = tokenize("    cmd").unwrap();
        assert_eq!(word, vec![AST::Whitespace, to_string("cmd")]);
    }

    #[test]
    fn parse_trailing_ws() {
println!("parse_trailing_ws");
        let word = tokenize("cmd    ").unwrap();
        assert_eq!(word, vec![to_string("cmd"), AST::Whitespace]);
    }

    #[test]
    fn parse_empty_cmd() {
println!("parse_empty_cmd");
        let word = tokenize("").unwrap();
        assert_eq!(word, Vec::new());
    }

    #[test]
    fn parse_quotemark() {
println!("parse_quotemark");
        let t = tokenize(r#""hello world""#).unwrap();
        assert_eq!(t, vec![AST::Quoted(
                vec![to_string("hello world")]
                )]);
    }

    #[test]
    fn parse_string_with_escaped() {
println!("parse_string_with_escaped");
        let t = tokenize(r#""hello \"""#).unwrap();
        assert_eq!(t, vec![AST::Quoted(
                vec![to_string("hello "), to_string("\"")]
                )]);
    }

    #[test]
    fn parse_unterminated_string() {
println!("parse_unterminated_string");
        let t = tokenize(r#""hello"#);
        assert!(t.is_err());
    }

    #[test]
    fn parse_literal_string() {
println!("parse_literal_string");
        let t = tokenize("'hello world'").unwrap();
        assert_eq!(t, vec![to_string("hello world")]);
    }

    #[test]
    fn parse_literal_string_unclosed_fails() {
println!("parse_literal_string_unclosed_fails");
        let t = tokenize("'hello");
        assert!(t.is_err());
    }

    #[test]
    fn parse_var() {
println!("parse_var");
        let t = tokenize("$ABC").unwrap();
        assert_eq!(t, vec![AST::Var("ABC".to_string())]);
    }

    #[test]
    fn parse_escaped_var() {
println!("parse_escaped_var");
        let t = tokenize(r#"\$ABC"#).unwrap();
        assert_eq!(t, vec![to_string("$"), to_string("ABC")]);
    }

    #[test]
    fn parse_escaped_string() {
println!("parse_escaped_string");
        let t = tokenize(r#"\"hello"#).unwrap();
        assert_eq!(t, vec![to_string("\""), to_string("hello")]);
    }
}

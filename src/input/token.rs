use std::fmt::{Display, Formatter, Error};

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum RedirOp {
    LESS,
    DLESS,
    DLESSDASH,
    LESSAND,
    LESSGREAT,

    GREAT,
    DGREAT,
    GREATAND,
    CLOBBER,
}

impl RedirOp {
    pub fn is_out(&self) -> bool {
        match *self {
            RedirOp::GREAT | RedirOp::DGREAT |
                RedirOp::GREATAND | RedirOp::CLOBBER => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Token {
    // Semantically important
    LineBreak,
    // Not semantically important
    Whitespace,

    String(String),
    Quoted(Vec<Token>),
    Var(String),
    Eql,
    Redir(Option<i32>, RedirOp),

    Semi,
    Async,
    And,
    Pipe,
    Or,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        try!(write!(f, "{:?}", self));
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenError {
    Unterminated,
    Unexpected(String),
}

impl Display for TokenError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            &TokenError::Unterminated => try!(write!(f, "unterminated string")),
            &TokenError::Unexpected(ref s) => try!(write!(f, "unexpected input: {}", s)),
        }
        Ok(())
    }
}

pub type TokenResult = Result<Option<Token>, TokenError>;

pub fn to_value(a: Token) -> Option<String> {
    match a {
        Token::String(s) => Some(s),
        Token::Quoted(contents) => Some(contents.into_iter().fold(String::new(), |mut acc: String, t| {
            let v = to_value(t);
            if v.is_none() {
                return acc;
            }
            acc.push_str(&v.unwrap());
            acc
        })),
        _ => None,
    }
}


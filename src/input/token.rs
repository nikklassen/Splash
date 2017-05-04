use std::fmt::{Display, Formatter, Error};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    Word(String),
    IONumber(i32),

    LineBreak,

    // Operators
    LESS,
    DLESS,
    DLESSDASH,
    LESSAND,
    LESSGREAT,

    GREAT,
    DGREAT,
    GREATAND,
    CLOBBER,

    AND,
    OR,
    DSEMI,

    // Non-operator special symbols
    Semi,
    Async,
    Pipe,
}


impl Token {
    pub fn is_out(&self) -> bool {
        match *self {
            Token::GREAT | Token::DGREAT | Token::GREATAND | Token::CLOBBER => true,
            _ => false,
        }
    }
}

pub fn is_redir(t: Token) -> bool {
    match t {
        Token::GREAT | Token::DGREAT | Token::GREATAND | Token::CLOBBER
            | Token::LESS | Token::DLESS | Token::DLESSDASH | Token::LESSAND
            | Token::LESSGREAT => true,
        _ => false,
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        try!(write!(f, "{:?}", self));
        Ok(())
    }
}

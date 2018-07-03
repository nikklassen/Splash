use std::fmt::{Display, Error, Formatter};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ReservedWord {
    IF,
    THEN,
    ELSE,
    ELIF,
    FI,
    DO,
    DONE,
    CASE,
    ESAC,
    WHILE,
    UNTIL,
    FOR,
    LBRACE,
    RBRACE,
    BANG,
    IN,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    Word(String),
    Reserved(ReservedWord),
    IONumber(i32),

    LINEBREAK,

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

    LPAREN,
    RPAREN,

    // Non-operator special symbols
    SEMI,
    ASYNC,
    PIPE,
}

impl Token {
    pub fn is_out(&self) -> bool {
        match *self {
            Token::GREAT | Token::DGREAT | Token::GREATAND | Token::CLOBBER => true,
            _ => false,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let s;
        write!(
            f,
            "{}",
            match self {
                Token::Word(w) => w,
                Token::IONumber(i) => {
                    s = format!("fd '{}'", i);
                    s.as_str()
                }
                Token::Reserved(r) => {
                    s = format!("{}", r);
                    s.as_str()
                }
                Token::LINEBREAK => "\\n",
                Token::LESS => "<",
                Token::DLESS => "<<",
                Token::DLESSDASH => "<<-",
                Token::LESSAND => "<&",
                Token::LESSGREAT => "<>",
                Token::GREAT => ">",
                Token::DGREAT => ">>",
                Token::GREATAND => ">&",
                Token::CLOBBER => ">|",
                Token::AND => "&&",
                Token::OR => "||",
                Token::DSEMI => ";;",
                Token::SEMI => ";",
                Token::ASYNC => "&",
                Token::PIPE => "|",
                Token::LPAREN => "(",
                Token::RPAREN => ")",
            }
        )
    }
}

impl Display for ReservedWord {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(
            f,
            "{}",
            match self {
                ReservedWord::IF => "if",
                ReservedWord::THEN => "then",
                ReservedWord::ELSE => "else",
                ReservedWord::ELIF => "elif",
                ReservedWord::FI => "fi",
                ReservedWord::DO => "do",
                ReservedWord::DONE => "done",
                ReservedWord::CASE => "case",
                ReservedWord::ESAC => "esac",
                ReservedWord::WHILE => "while",
                ReservedWord::UNTIL => "until",
                ReservedWord::FOR => "for",
                ReservedWord::LBRACE => "{",
                ReservedWord::RBRACE => "}",
                ReservedWord::BANG => "!",
                ReservedWord::IN => "in",
            }
        )
    }
}

pub fn is_redir(t: Token) -> bool {
    match t {
        Token::GREAT
        | Token::DGREAT
        | Token::GREATAND
        | Token::CLOBBER
        | Token::LESS
        | Token::DLESS
        | Token::DLESSDASH
        | Token::LESSAND
        | Token::LESSGREAT => true,
        _ => false,
    }
}

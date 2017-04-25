use nix::fcntl::{self, OFlag};
use libc::{STDOUT_FILENO, STDIN_FILENO};

use super::token::{self, Token, RedirOp};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Redir {
    Copy(i32),
    File(Word, OFlag),
    Temp(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CmdPrefix {
    IORedirect {
        fd: i32,
        target: Redir,
    },
    Assignment {
        lhs: String,
        rhs: Word,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Cmd {
        prog: Option<Word>,
        args: Vec<Word>,
        io: Vec<CmdPrefix>,
        env: Vec<CmdPrefix>,
    },
    EqlStmt {
        lhs: String,
        rhs: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pipeline {
    pub bang: bool,
    pub seq: Vec<Op>,
    pub async: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CommandList {
    AndList(Box<CommandList>, Pipeline),
    OrList(Box<CommandList>, Pipeline),
    SimpleList(Pipeline),
}

pub type CompleteCommand = Vec<CommandList>;

pub fn build_io_redirect((redir, io_file): (Token, Word)) -> CmdPrefix {
    let io_number;
    let redir_op;
    if let Token::Redir(io_number_opt, op) = redir {
        io_number = io_number_opt.unwrap_or(
            if op.is_out() { STDOUT_FILENO } else { STDIN_FILENO });
        redir_op = op;
    } else {
        unreachable!();
    }
    let parse_fd = |fd| word_to_value(&fd).parse::<i32>().unwrap();
    let target = match redir_op {
        RedirOp::LESS => Redir::File(io_file, fcntl::O_RDONLY),
        RedirOp::LESSAND => Redir::Copy(parse_fd(io_file)),
        RedirOp::LESSGREAT => Redir::File(io_file, fcntl::O_RDWR | fcntl::O_CREAT),

        RedirOp::GREAT | RedirOp::CLOBBER => Redir::File(io_file, fcntl::O_WRONLY | fcntl::O_CREAT | fcntl::O_TRUNC),
        RedirOp::DGREAT => Redir::File(io_file, fcntl::O_WRONLY | fcntl::O_CREAT | fcntl::O_APPEND),
        RedirOp::GREATAND => Redir::Copy(parse_fd(io_file)),
        RedirOp::DLESS | RedirOp::DLESSDASH => Redir::Temp(String::new()),
    };
    CmdPrefix::IORedirect {
        fd: io_number,
        target: target,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Word(pub Vec<Token>);

pub fn word_to_value(w: &Word) -> String {
    let mut val = String::new();
    for part in w.0.iter() {
        val.push_str(&token::to_value(part).unwrap_or(String::new()));
    }
    val
}

use std::fmt;
impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", word_to_value(&self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn word_to_value_joins() {
        let word = Word(vec![
                        Token::String("a".to_string()),
                        Token::String("b c".to_string())]);
        let val = word_to_value(&word);
        assert_eq!(val, "ab c".to_string());
    }
}

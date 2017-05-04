use nix::fcntl::{self, OFlag};
use libc::{STDOUT_FILENO, STDIN_FILENO};

use super::token::Token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Redir {
    Copy(i32),
    File(String, OFlag),
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
        rhs: String,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Cmd {
        prog: Option<String>,
        args: Vec<String>,
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

pub fn build_io_redirect(((io_number_opt, redir_op), io_file): ((Option<Token>, Token), String)) -> CmdPrefix {
    fn parse_fd(fd: String) -> i32 {
        // TODO fail gracefully
        fd.parse::<i32>().unwrap()
    }
    let io_number = io_number_opt
        .map(|t| if let Token::IONumber(n) = t {
            n
        } else {
            unreachable!()
        }).unwrap_or(if redir_op.is_out() { STDOUT_FILENO } else { STDIN_FILENO });
    let target = match redir_op {
        Token::LESS => Redir::File(io_file, fcntl::O_RDONLY),
        Token::LESSAND => Redir::Copy(parse_fd(io_file)),
        Token::LESSGREAT => Redir::File(io_file, fcntl::O_RDWR | fcntl::O_CREAT),

        Token::GREAT | Token::CLOBBER => Redir::File(io_file, fcntl::O_WRONLY | fcntl::O_CREAT | fcntl::O_TRUNC),
        Token::DGREAT => Redir::File(io_file, fcntl::O_WRONLY | fcntl::O_CREAT | fcntl::O_APPEND),
        Token::GREATAND => Redir::Copy(parse_fd(io_file)),
        Token::DLESS | Token::DLESSDASH => Redir::Temp(String::new()),
        _ => unreachable!(),
    };
    CmdPrefix::IORedirect {
        fd: io_number,
        target: target,
    }
}

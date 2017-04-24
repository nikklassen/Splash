use nix::fcntl::{self, OFlag};
use libc::{STDOUT_FILENO, STDIN_FILENO};

use super::token::{self, Token, RedirOp};

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

pub fn build_io_redirect((redir, io_file): (Token, Token)) -> CmdPrefix {
    let file_name = token::to_value(io_file).unwrap();
    let io_number;
    let redir_op;
    if let Token::Redir(io_number_opt, op) = redir {
        io_number = io_number_opt.unwrap_or(
            if op.is_out() { STDOUT_FILENO } else { STDIN_FILENO });
        redir_op = op;
    } else {
        unreachable!();
    }
    let target = match redir_op {
        RedirOp::LESS => Redir::File(file_name, fcntl::O_RDONLY),
        RedirOp::LESSAND => Redir::Copy(file_name.parse::<i32>().unwrap()),
        RedirOp::LESSGREAT => Redir::File(file_name, fcntl::O_RDWR | fcntl::O_CREAT),

        RedirOp::GREAT | RedirOp::CLOBBER => Redir::File(file_name, fcntl::O_WRONLY | fcntl::O_CREAT | fcntl::O_TRUNC),
        RedirOp::DGREAT => Redir::File(file_name, fcntl::O_WRONLY | fcntl::O_CREAT | fcntl::O_APPEND),
        RedirOp::GREATAND => Redir::Copy(file_name.parse::<i32>().unwrap()),
        RedirOp::DLESS | RedirOp::DLESSDASH => Redir::Temp(String::new()),
    };
    CmdPrefix::IORedirect {
        fd: io_number,
        target: target,
    }
}


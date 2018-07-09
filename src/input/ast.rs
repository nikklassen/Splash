use libc::{STDIN_FILENO, STDOUT_FILENO};
use nix::fcntl::OFlag;

use super::token::Token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Redir {
    Copy(i32),
    File(String, OFlag),
    Temp(String),
    // Essentially the same as the Replace IOOp
    Pipe(i32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CmdPrefix {
    IORedirect { fd: i32, target: Redir },
    Assignment { lhs: String, rhs: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimpleCommand {
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
pub struct IfBranch {
    pub condition: Vec<Statement>,
    pub block: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseItem {
    pub patterns: Vec<String>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompoundCommand {
    If {
        branches: Vec<IfBranch>,
        else_block: Option<Box<Vec<Statement>>>,
    },
    SubShell(Vec<Statement>),
    BraceGroup(Vec<Statement>),
    For {
        var: String,
        list: Vec<String>,
        body: Vec<Statement>,
    },
    While {
        cond: Vec<Statement>,
        body: Vec<Statement>,
    },
    Until {
        cond: Vec<Statement>,
        body: Vec<Statement>,
    },
    Case {
        match_var: String,
        cases: Vec<CaseItem>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command {
    SimpleCommand(SimpleCommand),
    CompoundCommand(CompoundCommand, Vec<CmdPrefix>),
    // TODO
    // FunctionDef(),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pipeline {
    pub bang: bool,
    pub cmds: Vec<Command>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AndOrList {
    And(Box<AndOrList>, Pipeline),
    Or(Box<AndOrList>, Pipeline),
    Pipeline(Pipeline),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Async(AndOrList),
    Seq(AndOrList),
}

pub type Script = Vec<Statement>;

pub fn build_io_redirect(
    ((io_number_opt, redir_op), io_file): ((Option<Token>, Token), String),
) -> CmdPrefix {
    fn parse_fd(fd: String) -> i32 {
        // TODO fail gracefully
        fd.parse::<i32>().unwrap()
    }
    let io_number = io_number_opt
        .map(|t| {
            if let Token::IONumber(n) = t {
                n
            } else {
                unreachable!()
            }
        })
        .unwrap_or(if redir_op.is_out() {
            STDOUT_FILENO
        } else {
            STDIN_FILENO
        });
    let target = match redir_op {
        Token::LESS => Redir::File(io_file, OFlag::O_RDONLY),
        Token::LESSAND => Redir::Copy(parse_fd(io_file)),
        Token::LESSGREAT => Redir::File(io_file, OFlag::O_RDWR | OFlag::O_CREAT),

        Token::GREAT | Token::CLOBBER => {
            Redir::File(io_file, OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_TRUNC)
        }
        Token::DGREAT => Redir::File(io_file, OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_APPEND),
        Token::GREATAND => Redir::Copy(parse_fd(io_file)),
        Token::DLESS | Token::DLESSDASH => Redir::Temp(String::new()),
        _ => unreachable!(),
    };
    CmdPrefix::IORedirect {
        fd: io_number,
        target: target,
    }
}

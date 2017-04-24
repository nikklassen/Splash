use combine::*;
use combine::combinator::{self, Optional, Many1};
use combine::primitives::{Stream, Positioner};
use env::UserEnv;
use super::interpolate;
use libc::{STDOUT_FILENO, STDIN_FILENO};
use nix::fcntl::{self, OFlag};
use super::token::{Token, RedirOp};

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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TokenPositioner(usize);
impl Positioner for Token {
    type Position = TokenPositioner;

    fn start() -> TokenPositioner {
        TokenPositioner(0)
    }

    fn update(&self, p: &mut TokenPositioner) {
        p.0 += 1;
    }
}

use std::fmt;
impl fmt::Display for TokenPositioner {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Position {}", self.0)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct OpPositioner(usize);
impl Positioner for Op {
    type Position = OpPositioner;

    fn start() -> OpPositioner {
        OpPositioner(0)
    }

    fn update(&self, p: &mut OpPositioner) {
        p.0 += 1;
    }
}

fn word<I>(input: I) -> primitives::ParseResult<Token, I>
where I: Stream<Item=Token> {
    many1(satisfy(|t| match t {
        Token::String(_) | Token::Quoted(_) | Token::Var(_) => true,
        _ => false,
    })).map(|arg: Vec<Token>| {
            let value = arg
            .into_iter()
            .fold(String::new(), |mut acc, a| {
                if let Some(ref v) = to_value(a) {
                    acc.push_str(&v);
                }
                acc
            });
            Token::String(value)
        })
        .parse_stream(input)
}

fn to_value(a: Token) -> Option<String> {
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

fn build_io_redirect((redir, io_file): (Token, Token)) -> CmdPrefix {
    let file_name = to_value(io_file).unwrap();
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

fn io_redirect<I>(input: I) -> primitives::ParseResult<CmdPrefix, I>
where I: Stream<Item=Token> {
    satisfy(|t| is_match!(t, Token::Redir(..)))
         .skip(ws())
         .and(parser(word::<I>))
    .map(build_io_redirect)
    .parse_stream(input)
}

fn assignment<I>(input: I) -> primitives::ParseResult<CmdPrefix, I>
where I: Stream<Item=Token> {
    try(satisfy(|t| is_match!(t, Token::String(_)))
        .skip(token(Token::Eql)))
        .and(parser(word::<I>))
        .map(|(lhs, rhs)| {
            CmdPrefix::Assignment {
                lhs: to_value(lhs).unwrap(),
                rhs: to_value(rhs).unwrap()
            }
        })
    .parse_stream(input)
}

fn command<I>(input: I) -> primitives::ParseResult<Op, I>
where I: Stream<Item=Token> {
    let cmd_prefix = many1(choice![
         parser(io_redirect::<I>),
         parser(assignment::<I>)
    ].skip(ws()));
    let cmd_word = || parser(word::<I>).skip(ws());

    (cmd_prefix.and(many(cmd_word())))
     .or(many1(cmd_word()).map(|w| (vec![], w)))
    // TODO redirects and args can be interleaved
    .and(many(parser(io_redirect::<I>).skip(ws())))
    .map(|((cmd_prefix, cmd_args), redirect_suffix): ((Vec<CmdPrefix>, Vec<Token>), Vec<CmdPrefix>)| {
        let mut prog = None;
        let mut args: Vec<String> = Vec::new();
        let mut string_args: Vec<String> = cmd_args
            .into_iter()
            .map(|a| to_value(a).unwrap_or(String::new()))
            .collect();
        if string_args.len() > 0 {
            prog = Some(string_args.remove(0));
            args = string_args;
        }
        let (env, io) = cmd_prefix.into_iter().partition(|r| is_match!(r, &CmdPrefix::Assignment { .. }));
        Op::Cmd {
            prog: prog,
            args: args,
            env: env,
            io: io.into_iter().chain(redirect_suffix).collect(),
        }
    })
    .parse_stream(input)
}

fn ws<I>() -> Optional<combinator::Token<I>> where I: Stream<Item=Token> {
    optional(token(Token::Whitespace))
}

fn pipeline<I>(input: I) -> primitives::ParseResult<Pipeline, I>
where I: Stream<Item=Token> {
    let cmd_chain = parser(command::<I>).map(|op| vec![op]);
    let pipe = token(Token::Pipe).map(|_t| return |mut lhs: Vec<Op>, mut rhs: Vec<Op>| {
        // Remove the first argument so we can distinguish it as the command name
        lhs.push(rhs.remove(0));
        lhs
    });

    chainl1(
        cmd_chain.skip(ws()),
        pipe.skip(ws()))
        .map(|cmds| {
            Pipeline {
                seq: cmds,
                async: false,
                bang: false,
            }
        })
    .parse_stream(input)
}

fn and_or<I>(input: I) -> primitives::ParseResult<CommandList, I>
where I: Stream<Item=Token> {
    chainl1(
        parser(pipeline::<I>).map(CommandList::SimpleList),
        choice([token(Token::And), token(Token::Or)]).map(|t: Token| move |l, r| {
            if let CommandList::SimpleList(pipe) = r {
                if t == Token::And {
                    CommandList::AndList(Box::new(l), pipe)
                } else {
                    CommandList::OrList(Box::new(l), pipe)
                }
            } else {
                unreachable!()
            }
        }))
        .parse_stream(input)
}

fn newline_list<I>() -> Many1<Vec<Token>, combinator::Token<I>>
where I: Stream<Item=Token> {
    many1(token(Token::LineBreak))
}

fn linebreak<I>() -> Optional<Many1<Vec<Token>, combinator::Token<I>>>
where I: Stream<Item=Token> {
    optional(newline_list())
}

fn complete_command<I>(input: I) -> primitives::ParseResult<CompleteCommand, I>
where I: Stream<Item=Token> {
    let separator_op = || choice![token(Token::Semi), token(Token::Async)];
    let separator = (separator_op().skip(linebreak()))
        .or(newline_list().map(|_| Token::LineBreak))
        .skip(ws());

    fn process_sep(sep: Token, cmd: &mut CompleteCommand) -> () {
        if sep == Token::Async {
            let p: &mut Pipeline = match cmd.last_mut().unwrap() {
                &mut CommandList::SimpleList(ref mut p) => p,
                &mut CommandList::AndList(_, ref mut p) => p,
                &mut CommandList::OrList(_, ref mut p) => p,
            };
            p.async = true;
        }
    }

    let list = parser(and_or).map(|p| vec![p])
        .skip(ws())
        .and(many(try(
                    separator_op()
                    .skip(ws())
                    .and(parser(and_or).map(|p| vec![p])))))
        .map(|(cmd, cmds): (CompleteCommand, Vec<(Token, CompleteCommand)>)| {
            let mut c = cmd;
            let mut new_cmds = Vec::new();
            for (sep, cmd2) in cmds.into_iter() {
                process_sep(sep, &mut c);
                new_cmds.append(&mut c);
                c = cmd2;
            }
            new_cmds.append(&mut c);
            new_cmds
        });

    list.and(optional(separator))
    .map(|(mut l, sep)| {
        if let Some(s) = sep {
            process_sep(s, &mut l);
        }
        l
    })
    .skip(eof())
    .parse_stream(input)
}

pub fn parse(mut tokens: Vec<Token>, user_env: &UserEnv, heredocs: &mut Vec<String>) -> Result<CompleteCommand, String> {
    tokens = interpolate::expand(tokens, &user_env);
    let result = parser(complete_command)
        .parse(State::new(primitives::from_iter(tokens.iter().cloned())))
        .map(|r| r.0)
        .or_else(|e| Err(format!("{}", e)))?;

    fn populate_heredocs(cmds: &mut CommandList, heredocs: &mut Vec<String>) {
        match cmds {
            &mut CommandList::SimpleList(ref mut pipeline) => {
                for cmd in pipeline.seq.iter_mut() {
                    match cmd {
                        &mut Op::Cmd { ref mut io, .. } => {
                            for ios in io.iter_mut() {
                                if let &mut CmdPrefix::IORedirect { target: Redir::Temp(ref mut contents), .. } = ios {
                                    if let Some(hd) = heredocs.pop() {
                                        *contents = hd;
                                    } else {
                                        unreachable!("Not enough heredocs were specified");
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    let populated = result.into_iter().map(|mut r| {
        populate_heredocs(&mut r, heredocs);
        r
    }).collect::<Vec<_>>();
    Ok(populated)
}

#[cfg(test)]
mod tests {
    use env::UserEnv;
    use libc::{STDOUT_FILENO, STDIN_FILENO};
    use nix::fcntl;
    use input::tokenizer::tokenize;
    use super::*;

    fn to_command_list(cmd: Op) -> CompleteCommand {
        vec![CommandList::SimpleList(Pipeline {
            seq: vec![cmd],
            async: false,
            bang: false,
        })]
    }

    #[test]
    fn parse_cmd_no_args() {
        let user_env = UserEnv::new();
        let input = tokenize("cmd").unwrap();
        let cmd = parse(input, &user_env, &mut vec![]).unwrap();
        warn!("{:?}", cmd);
        assert_eq!(cmd, to_command_list(Op::Cmd {
            prog: Some("cmd".to_string()),
            args: Vec::new(),
            io: Vec::new(),
            env: Vec::new(),
        }));
    }

    #[test]
    fn parse_cmd_multiple_args() {
        let user_env = UserEnv::new();
        let input = tokenize("cmd arg1 arg2").unwrap();
        let cmd = parse(input, &user_env, &mut vec![]).unwrap();
        assert_eq!(cmd, to_command_list(Op::Cmd {
            prog: Some("cmd".to_string()),
            args: vec!["arg1".to_string(), "arg2".to_string()],
            io: Vec::new(),
            env: Vec::new(),
        }));
    }

    #[test]
    fn parse_cmd_with_string_arg() {
        let user_env = UserEnv::new();
        let input = tokenize(r#"cmd "arg1 $VAR arg2""#).unwrap();
        let cmd = parse(input, &user_env, &mut vec![]).unwrap();
        assert_eq!(cmd, to_command_list(Op::Cmd {
            prog: Some("cmd".to_string()),
            args: vec!["arg1  arg2".to_string()],
            io: Vec::new(),
            env: Vec::new(),
        }));
    }

    #[test]
    fn parse_cmd_with_connected_args() {
        let user_env = UserEnv::new();
        let input = tokenize(r#"cmd arg1"arg2 arg3"$TEST'arg4 arg5'"#).unwrap();
        let cmd = parse(input, &user_env, &mut vec![]).unwrap();
        assert_eq!(cmd, to_command_list(Op::Cmd {
            prog: Some("cmd".to_string()),
            args: vec!["arg1arg2 arg3arg4 arg5".to_string()],
            io: Vec::new(),
            env: Vec::new(),
        }));
    }

    #[test]
    fn parse_eql_stmt() {
        let user_env = UserEnv::new();
        let input = tokenize("FOO=bar").unwrap();
        let cmd = parse(input, &user_env, &mut vec![]).unwrap();
        assert_eq!(cmd, to_command_list(Op::Cmd {
            prog: None,
            args: Vec::new(),
            io: Vec::new(),
            env: vec![CmdPrefix::Assignment {
                lhs: "FOO".to_string(),
                rhs: "bar".to_string(),
            }],
        }));
    }

    #[test]
    fn parse_eql_trailing_arg() {
        let user_env = UserEnv::new();
        let input = tokenize("FOO=bar baz").unwrap();
        let cmd = parse(input, &user_env, &mut vec![]).unwrap();
        assert_eq!(cmd, to_command_list(Op::Cmd {
            prog: Some("baz".to_string()),
            args: Vec::new(),
            io: Vec::new(),
            env: vec![CmdPrefix::Assignment {
                lhs: "FOO".to_string(),
                rhs: "bar".to_string(),
            }],
        }));
    }

    #[test]
    fn parse_pipe() {
        let user_env = UserEnv::new();
        let input = tokenize("cmd1 | cmd2 arg").unwrap();
        let cmd = parse(input, &user_env, &mut vec![]).unwrap();
        assert_eq!(cmd, vec![CommandList::SimpleList(Pipeline {
            seq: vec![
                Op::Cmd {
                    prog: Some("cmd1".to_string()),
                    args: Vec::new(),
                    io: Vec::new(),
                    env: Vec::new(),
                },
                Op::Cmd {
                    prog: Some("cmd2".to_string()),
                    args: vec!["arg".to_string()],
                    io: Vec::new(),
                    env: Vec::new(),
                }],
            async: false,
            bang: false,
        })]);
    }

    #[test]
    fn parse_redir_out() {
        let user_env = UserEnv::new();
        let input = tokenize("cmd1 > file.txt >> log.txt 2>&1").unwrap();
        let cmd = parse(input, &user_env, &mut vec![]).unwrap();
        let write_flags = fcntl::O_WRONLY | fcntl::O_CREAT;
        let trunc_flags = write_flags | fcntl::O_TRUNC;
        let append_flags = write_flags | fcntl::O_APPEND;
        assert_eq!(cmd, to_command_list(Op::Cmd {
            prog: Some("cmd1".to_string()),
            args: Vec::new(),
            io: vec![
                CmdPrefix::IORedirect {
                    fd: STDOUT_FILENO,
                    target: Redir::File("file.txt".to_string(), trunc_flags)
                },
                CmdPrefix::IORedirect {
                    fd: STDOUT_FILENO,
                    target: Redir::File("log.txt".to_string(), append_flags)
                },
                CmdPrefix::IORedirect {
                    fd: 2,
                    target: Redir::Copy(1)
                },
            ],
            env: Vec::new(),
        }));
    }

    #[test]
    fn parse_redir_in() {
        let user_env = UserEnv::new();
        let input = tokenize("cmd1 < file.txt <&3").unwrap();
        let cmd = parse(input, &user_env, &mut vec![]).unwrap();
        let read_flags = fcntl::O_RDONLY;
        assert_eq!(cmd, to_command_list(Op::Cmd {
            prog: Some("cmd1".to_string()),
            args: Vec::new(),
            io: vec![
                CmdPrefix::IORedirect {
                    fd: STDIN_FILENO,
                    target: Redir::File("file.txt".to_string(), read_flags)
                },
                CmdPrefix::IORedirect {
                    fd: STDIN_FILENO,
                    target: Redir::Copy(3)
                },
            ],
            env: Vec::new(),
        }));
    }

    #[test]
    fn parse_redir_prefix() {
        let user_env = UserEnv::new();
        let input = tokenize("> file.txt cmd1").unwrap();
        let cmd = parse(input, &user_env, &mut vec![]).unwrap();
        let write_flags = fcntl::O_WRONLY | fcntl::O_CREAT | fcntl::O_TRUNC;
        assert_eq!(cmd, to_command_list(Op::Cmd {
            prog: Some("cmd1".to_string()),
            args: Vec::new(),
            io: vec![
                CmdPrefix::IORedirect {
                    fd: STDOUT_FILENO,
                    target: Redir::File("file.txt".to_string(), write_flags)
                },
            ],
            env: Vec::new(),
        }));
    }

    #[test]
    fn populate_heredocs() {
        let user_env = UserEnv::new();
        let input = tokenize("cmd <<EOF").unwrap();
        let contents: String = "contents".into();
        let cmd = parse(input, &user_env, &mut vec![contents.clone()]).unwrap();
        assert_eq!(cmd, to_command_list(Op::Cmd {
            prog: Some("cmd".into()),
            args: Vec::new(),
            io: vec![
                CmdPrefix::IORedirect {
                    fd: STDIN_FILENO,
                    target: Redir::Temp(contents)
                },
            ],
            env: Vec::new(),
        }));
    }

    #[test]
    fn async_command() {
        let user_env = UserEnv::new();
        let input = tokenize("cmd &").unwrap();
        let cmd = parse(input, &user_env, &mut vec![]).unwrap();

        assert_eq!(cmd, vec![CommandList::SimpleList(Pipeline {
            seq: vec![Op::Cmd {
                prog: Some("cmd".into()),
                args: Vec::new(),
                io: Vec::new(),
                env: Vec::new(),
            }],
            async: true,
            bang: false,
        })]);
    }

    #[test]
    fn semi_separated() {
        let user_env = UserEnv::new();
        let input = tokenize("cmd1; cmd2").unwrap();
        let cmd = parse(input, &user_env, &mut vec![]).unwrap();

        assert_eq!(cmd, vec![CommandList::SimpleList(Pipeline {
            seq: vec![Op::Cmd {
                prog: Some("cmd1".into()),
                args: Vec::new(),
                io: Vec::new(),
                env: Vec::new(),
            }],
            async: false,
            bang: false,
        }), CommandList::SimpleList(Pipeline {
            seq: vec![Op::Cmd {
                prog: Some("cmd2".into()),
                args: Vec::new(),
                io: Vec::new(),
                env: Vec::new(),
            }],
            async: false,
            bang: false,
        })]);
    }
}

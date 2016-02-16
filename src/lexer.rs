use env::UserEnv;
use interpolate;
use combine::*;
use combine::primitives::{Stream, Positioner};
use combine::combinator::{Optional, Token};
use tokenizer::{self, AST};

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Redir {
    In(String),
    Out(String),
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Op {
    Cmd {
        prog: String,
        args: Vec<String>,
        io: Option<Redir>,
    },
    EqlStmt {
        lhs: String,
        rhs: String,
    },
    Pipe {
        cmds: Vec<Op>,
    },
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ASTPositioner(usize);
impl Positioner for AST {
    type Position = ASTPositioner;

    fn start() -> ASTPositioner {
        ASTPositioner(0)
    }

    fn update(&self, p: &mut ASTPositioner) {
        p.0 += 1;
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

fn is_sep(t: &AST) -> bool {
    [AST::Whitespace, AST::Pipe, AST::GT, AST::LT].contains(t)
}

fn arg<I>(input: State<I>) -> primitives::ParseResult<AST, I>
where I: Stream<Item=AST> {
    many1(satisfy(|t| !is_sep(&t)))
        .map(|arg: Vec<AST>| {
            let value = arg
            .into_iter()
            .fold(String::new(), |mut acc, a| {
                if let Some(ref v) = to_value(a) {
                    acc.push_str(&v);
                }
                acc
            });
            AST::String(value)
        })
        .parse_state(input)
}

fn to_value(a: AST) -> Option<String> {
    match a {
        AST::String(s) => Some(s),
        AST::Quoted(contents) => Some(contents.into_iter().fold(String::new(), |mut acc: String, t| {
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

fn command<I>(input: State<I>) -> primitives::ParseResult<Op, I>
where I: Stream<Item=AST> {
    many1(parser(arg::<I>).skip(ws()))
        .and(optional(
            choice([token(AST::GT), token(AST::LT)])
            .skip(ws())
            .and(parser(arg::<I>))))
        .map(|(args, redir): (Vec<AST>, Option<(AST, AST)>)| {
            let io = redir.map(|(dir, io_target)| {
                let val = to_value(io_target).unwrap();
                if dir == AST::GT {
                    Redir::Out(val)
                } else {
                    Redir::In(val)
                }
            });

            let mut string_args: Vec<String> = args
                .into_iter()
                .map(|a| to_value(a).unwrap_or(String::new()))
                .collect();
            let prog = string_args.remove(0);
            Op::Cmd {
                prog: prog,
                args: string_args,
                io: io,
            }
        })
        .parse_state(input)
}

fn assignment<I>(input: State<I>) -> primitives::ParseResult<Op, I>
where I: Stream<Item=AST> {
    try(satisfy(|t| is_match!(t, AST::String(_)))
        .skip(token(AST::Eql)))
        .and(parser(arg::<I>))
        .map(|(lhs, rhs)| {
            Op::EqlStmt {
                lhs: to_value(lhs).unwrap(),
                rhs: to_value(rhs).unwrap()
            }
        })
    .parse_state(input)
}

fn ws<I>() -> Optional<Token<I>> where I: Stream<Item=AST> {
    optional(token(AST::Whitespace))
}

fn piped<I>(input: State<I>) -> primitives::ParseResult<Op, I>
where I: Stream<Item=AST> {
    let cmd_chain = parser(command::<I>).map(|op| vec![op]);
    let pipe = token(AST::Pipe).map(|_t| return |mut lhs: Vec<Op>, mut rhs: Vec<Op>| {
        // Remove the first argument so we can distinguish it as the command name
        lhs.push(rhs.remove(0));
        lhs
    });

    chainl1(
        cmd_chain.skip(ws()),
        pipe.skip(ws()))
        .map(|mut cmds| {
            if cmds.len() == 1 {
                return cmds.remove(0);
            }

            Op::Pipe {
                cmds: cmds
            }
        })
    .parse_state(input)
}

fn stmt<I>(input: State<I>) -> primitives::ParseResult<Op, I>
where I: Stream<Item=AST> {
    ws().with(choice(
            vec![assignment::<I> as fn(State<I>) -> primitives::ParseResult<Op, I>, piped::<I>]))
        .skip(ws())
        .skip(not_followed_by(any()))
        .parse_state(input)
}


pub fn parse(line: &str, user_env: &UserEnv) -> Result<Option<Op>, String> {
    let mut tokens: Vec<AST> = try!(tokenizer::tokenize(&line));
    if tokens.is_empty() {
        return Ok(None);
    }
    tokens = interpolate::expand(tokens, &user_env);
    parser(stmt)
        .parse(primitives::from_iter(tokens.iter().cloned()))
        .map(|r| Some(r.0))
        .or_else(|e| Err(format!("{:?}", e.errors)))
}

#[cfg(test)]
mod tests {
    use env::UserEnv;
    use super::*;

    #[test]
    fn parse_empty() {
        let user_env = UserEnv::new();
        let cmd = parse("", &user_env);
        assert_eq!(cmd, Ok(None));
    }

    #[test]
    fn parse_cmd_no_args() {
        let user_env = UserEnv::new();
        let cmd = parse("cmd", &user_env).unwrap().unwrap();
        assert_eq!(cmd, Op::Cmd {
            prog: "cmd".to_string(),
            args: Vec::new(),
            io: None,
        });
    }

    #[test]
    fn parse_cmd_multiple_args() {
        let user_env = UserEnv::new();
        let cmd = parse("cmd arg1 arg2", &user_env).unwrap().unwrap();
        assert_eq!(cmd, Op::Cmd {
            prog: "cmd".to_string(),
            args: vec!["arg1".to_string(), "arg2".to_string()],
            io: None,
        });
    }

    #[test]
    fn parse_cmd_with_string_arg() {
        let user_env = UserEnv::new();
        let cmd = parse(r#"cmd "arg1 $VAR arg2""#, &user_env).unwrap().unwrap();
        assert_eq!(cmd, Op::Cmd {
            prog: "cmd".to_string(),
            args: vec!["arg1  arg2".to_string()],
            io: None,
        });
    }

    #[test]
    fn parse_cmd_with_connected_args() {
        let user_env = UserEnv::new();
        let cmd = parse(r#"cmd arg1"arg2 arg3"$TEST'arg4 arg5'"#, &user_env).unwrap().unwrap();
        assert_eq!(cmd, Op::Cmd {
            prog: "cmd".to_string(),
            args: vec!["arg1arg2 arg3arg4 arg5".to_string()],
            io: None,
        });
    }

    #[test]
    fn parse_eql_stmt() {
        let user_env = UserEnv::new();
        let cmd = parse("FOO=bar", &user_env).unwrap().unwrap();
        assert_eq!(cmd, Op::EqlStmt {
            lhs: "FOO".to_string(),
            rhs: "bar".to_string(),
        });
    }

    #[test]
    fn parse_eql_trailing_arg_fails() {
        let user_env = UserEnv::new();
        let cmd = parse("FOO=bar baz", &user_env);
        assert!(cmd.is_err());
    }

    #[test]
    fn parse_pipe() {
        let user_env = UserEnv::new();
        let cmd = parse("cmd1 | cmd2 arg", &user_env).unwrap().unwrap();
        assert_eq!(cmd, Op::Pipe {
            cmds: vec![
                Op::Cmd {
                    prog: "cmd1".to_string(),
                    args: Vec::new(),
                    io: None,
                },
                Op::Cmd {
                    prog: "cmd2".to_string(),
                    args: vec!["arg".to_string()],
                    io: None,
                }],
        });
    }

    #[test]
    fn parse_redir_out() {
        let user_env = UserEnv::new();
        let cmd = parse("cmd1 > file.txt", &user_env).unwrap().unwrap();
        assert_eq!(cmd, Op::Cmd {
            prog: "cmd1".to_string(),
            args: Vec::new(),
            io: Some(Redir::Out("file.txt".to_string())),
        })
    }

    #[test]
    fn parse_redir_in() {
        let user_env = UserEnv::new();
        let cmd = parse("cmd1 < file.txt", &user_env).unwrap().unwrap();
        assert_eq!(cmd, Op::Cmd {
            prog: "cmd1".to_string(),
            args: Vec::new(),
            io: Some(Redir::In("file.txt".to_string())),
        })
    }
}

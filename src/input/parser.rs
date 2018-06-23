use super::ast::*;
use super::token::{self, Token};
use combine::easy;
use combine::*;
use util;

parser! {
    fn word[I]()(I) -> String
    where [
        I: Stream<Item = Token>,
    ] {
        satisfy(|t| is_match!(t, Token::Word(_)))
            .map(|t| {
                if let Token::Word(w) = t {
                    w
                } else {
                    unreachable!()
                }
            })
    }
}

parser! {
    fn io_redirect[I]()(I) -> CmdPrefix
    where [
        I: Stream<Item = Token>,
    ] {
        try(optional(satisfy(|t| is_match!(t, Token::IONumber(_)))).and(satisfy(token::is_redir)))
            .and(word())
            .map(build_io_redirect)
    }
}

parser! {
    fn assignment[I]()(I) -> CmdPrefix
    where [
I: Stream<Item = Token>,
] {
    try(satisfy(|t| {
        if let Token::Word(w) = t {
            w.contains('=')
        } else {
            false
        }
    })).map(|t| {
        if let Token::Word(w) = t {
            let mut split = w.split("=");
            CmdPrefix::Assignment {
                lhs: split.next().unwrap().to_owned(),
                rhs: split.next().unwrap().to_owned(),
            }
        } else {
            unreachable!()
        }
    })
}
}

parser! {
    fn command[I]()(I) -> Op
    where [
        I: Stream<Item = Token>,
    ] {
        let cmd_prefix = many1(choice![io_redirect(), assignment()]);
        let cmd_word = || word();

        (cmd_prefix.and(many(cmd_word())))
        .or(many1(cmd_word()).map(|w| (vec![], w)))
        // TODO redirects and args can be interleaved
        .and(many(io_redirect()))
        .map(|((cmd_prefix, mut cmd_args), redirect_suffix): ((Vec<CmdPrefix>, Vec<String>), Vec<CmdPrefix>)| {
            let mut prog = None;
            let mut args: Vec<String> = Vec::new();
            if cmd_args.len() > 0 {
                prog = Some(cmd_args.remove(0));
                args = cmd_args;
            }
            let (env, io) = cmd_prefix.into_iter().partition(|r| is_match!(r, &CmdPrefix::Assignment { .. }));
            Op::Cmd {
                prog: prog,
                args: args,
                env: env,
                io: io.into_iter().chain(redirect_suffix).collect(),
            }
        })
    }
}

parser! {
    fn pipeline[I]()(I) -> Pipeline
    where [
        I: Stream<Item = Token>,
    ] {
        let cmd_chain = command().map(|op| vec![op]);
        let pipe = token(Token::Pipe).map(|_t| {
            return |mut lhs: Vec<Op>, mut rhs: Vec<Op>| {
                // Remove the first argument so we can distinguish it as the command name
                lhs.push(rhs.remove(0));
                lhs
            };
        });

        chainl1(cmd_chain, pipe)
            .map(|cmds| Pipeline {
                seq: cmds,
                async: false,
                bang: false,
            })
    }
}

parser! {
    fn and_or[I]()(I) -> CommandList
    where [
        I: Stream<Item = Token>,
    ] {
        chainl1(
            pipeline().map(CommandList::SimpleList),
            choice([token(Token::AND), token(Token::OR)])
                .skip(linebreak())
                .map(|t: Token| {
                    move |l, r| {
                        if let CommandList::SimpleList(pipe) = r {
                            if t == Token::AND {
                                CommandList::AndList(Box::new(l), pipe)
                            } else {
                                CommandList::OrList(Box::new(l), pipe)
                            }
                        } else {
                            unreachable!()
                        }
                    }
                }),
        )
    }
}

parser! {
    fn newline_list[I]()(I) -> Vec<Token>
    where [
        I: Stream<Item = Token>,
    ] {
        many1(token(Token::LineBreak))
    }
}

parser! {
    fn linebreak[I]()(I) -> Option<Vec<Token>>
    where [
        I: Stream<Item = Token>,
    ] {
        optional(newline_list())
    }
}

parser! {
    fn complete_command[I]()(I) -> CompleteCommand
    where [
I: Stream<Item = Token>,
] {
    let separator_op = || choice![token(Token::Semi), token(Token::Async)];
    let separator = (separator_op().skip(linebreak())).or(newline_list().map(|_| Token::LineBreak));

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

    let list = and_or()
        .map(|p| vec![p])
        .and(many(try(
            separator_op().and(and_or().map(|p| vec![p]))
        )))
        .map(
            |(cmd, cmds): (CompleteCommand, Vec<(Token, CompleteCommand)>)| {
                let mut c = cmd;
                let mut new_cmds = Vec::new();
                for (sep, cmd2) in cmds.into_iter() {
                    process_sep(sep, &mut c);
                    new_cmds.append(&mut c);
                    c = cmd2;
                }
                new_cmds.append(&mut c);
                new_cmds
            },
        );

    list.and(optional(separator))
        .map(|(mut l, sep)| {
            if let Some(s) = sep {
                process_sep(s, &mut l);
            }
            l
        })
        .skip(eof())
}
}

pub fn parse(tokens: Vec<Token>, heredocs: &mut Vec<String>) -> Result<CompleteCommand, String> {
    let result = complete_command()
        .easy_parse(easy::Stream(&tokens[..]))
        .map(|r| r.0)
        .or_else(util::show_err)?;

    fn populate_heredocs(cmds: &mut CommandList, heredocs: &mut Vec<String>) {
        match cmds {
            &mut CommandList::SimpleList(ref mut pipeline) => {
                for cmd in pipeline.seq.iter_mut() {
                    match cmd {
                        &mut Op::Cmd { ref mut io, .. } => {
                            for ios in io.iter_mut() {
                                if let &mut CmdPrefix::IORedirect {
                                    target: Redir::Temp(ref mut contents),
                                    ..
                                } = ios
                                {
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

    let populated = result
        .into_iter()
        .map(|mut r| {
            populate_heredocs(&mut r, heredocs);
            r
        })
        .collect::<Vec<_>>();
    Ok(populated)
}

#[cfg(test)]
mod tests {
    use super::*;
    use input::tokenizer::tokenize;
    use libc::{STDIN_FILENO, STDOUT_FILENO};
    use nix::fcntl::OFlag;

    fn to_command_list(cmd: Op) -> CompleteCommand {
        vec![CommandList::SimpleList(Pipeline {
            seq: vec![cmd],
            async: false,
            bang: false,
        })]
    }

    fn to_word(s: &str) -> String {
        s.to_string()
    }

    #[test]
    fn parse_cmd_no_args() {
        let input = tokenize("cmd", false).0;
        let cmd = parse(input, &mut vec![]).unwrap();
        assert_eq!(
            cmd,
            to_command_list(Op::Cmd {
                prog: Some(to_word("cmd")),
                args: Vec::new(),
                io: Vec::new(),
                env: Vec::new(),
            })
        );
    }

    #[test]
    fn parse_cmd_multiple_args() {
        let input = tokenize("cmd argA argB", false).0;
        let cmd = parse(input, &mut vec![]).unwrap();
        assert_eq!(
            cmd,
            to_command_list(Op::Cmd {
                prog: Some(to_word("cmd")),
                args: vec![to_word("argA"), to_word("argB")],
                io: Vec::new(),
                env: Vec::new(),
            })
        );
    }

    #[test]
    fn parse_cmd_with_string_arg() {
        let input = tokenize(r#"cmd "argA $VAR argB""#, false).0;
        let cmd = parse(input, &mut vec![]).unwrap();
        assert_eq!(
            cmd,
            to_command_list(Op::Cmd {
                prog: Some(to_word("cmd")),
                args: vec![r#""argA $VAR argB""#.to_string()],
                io: Vec::new(),
                env: Vec::new(),
            })
        );
    }

    #[test]
    fn parse_cmd_with_connected_args() {
        let input = tokenize(r#"cmd argA"argB argC"$TEST'argD argE'"#, false).0;
        let cmd = parse(input, &mut vec![]).unwrap();
        assert_eq!(
            cmd,
            to_command_list(Op::Cmd {
                prog: Some(to_word("cmd")),
                args: vec![r#"argA"argB argC"$TEST'argD argE'"#.to_string()],
                io: Vec::new(),
                env: Vec::new(),
            })
        );
    }

    #[test]
    fn parse_eql_stmt() {
        let input = tokenize("FOO=bar", false).0;
        let cmd = parse(input, &mut vec![]).unwrap();
        assert_eq!(
            cmd,
            to_command_list(Op::Cmd {
                prog: None,
                args: Vec::new(),
                io: Vec::new(),
                env: vec![CmdPrefix::Assignment {
                    lhs: "FOO".to_string(),
                    rhs: to_word("bar"),
                }],
            })
        );
    }

    #[test]
    fn parse_eql_trailing_arg() {
        let input = tokenize("FOO=bar baz", false).0;
        let cmd = parse(input, &mut vec![]).unwrap();
        assert_eq!(
            cmd,
            to_command_list(Op::Cmd {
                prog: Some(to_word("baz")),
                args: Vec::new(),
                io: Vec::new(),
                env: vec![CmdPrefix::Assignment {
                    lhs: "FOO".to_string(),
                    rhs: to_word("bar"),
                }],
            })
        );
    }

    #[test]
    fn parse_pipe() {
        let input = tokenize("cmdA | cmdB arg", false).0;
        let cmd = parse(input, &mut vec![]).unwrap();
        assert_eq!(
            cmd,
            vec![CommandList::SimpleList(Pipeline {
                seq: vec![
                    Op::Cmd {
                        prog: Some(to_word("cmdA")),
                        args: Vec::new(),
                        io: Vec::new(),
                        env: Vec::new(),
                    },
                    Op::Cmd {
                        prog: Some(to_word("cmdB")),
                        args: vec![to_word("arg")],
                        io: Vec::new(),
                        env: Vec::new(),
                    },
                ],
                async: false,
                bang: false,
            })]
        );
    }

    #[test]
    fn parse_redir_out() {
        let input = tokenize("cmd > file.txt >> log.txt 2>&1", false).0;
        let cmd = parse(input, &mut vec![]).unwrap();
        let write_flags = OFlag::O_WRONLY | OFlag::O_CREAT;
        let trunc_flags = write_flags | OFlag::O_TRUNC;
        let append_flags = write_flags | OFlag::O_APPEND;
        assert_eq!(
            cmd,
            to_command_list(Op::Cmd {
                prog: Some(to_word("cmd")),
                args: Vec::new(),
                io: vec![
                    CmdPrefix::IORedirect {
                        fd: STDOUT_FILENO,
                        target: Redir::File(to_word("file.txt"), trunc_flags),
                    },
                    CmdPrefix::IORedirect {
                        fd: STDOUT_FILENO,
                        target: Redir::File(to_word("log.txt"), append_flags),
                    },
                    CmdPrefix::IORedirect {
                        fd: 2,
                        target: Redir::Copy(1),
                    },
                ],
                env: Vec::new(),
            })
        );
    }

    #[test]
    fn parse_redir_in() {
        let input = tokenize("cmd < file.txt <&3", false).0;
        let cmd = parse(input, &mut vec![]).unwrap();
        let read_flags = OFlag::O_RDONLY;
        assert_eq!(
            cmd,
            to_command_list(Op::Cmd {
                prog: Some(to_word("cmd")),
                args: Vec::new(),
                io: vec![
                    CmdPrefix::IORedirect {
                        fd: STDIN_FILENO,
                        target: Redir::File(to_word("file.txt"), read_flags),
                    },
                    CmdPrefix::IORedirect {
                        fd: STDIN_FILENO,
                        target: Redir::Copy(3),
                    },
                ],
                env: Vec::new(),
            })
        );
    }

    #[test]
    fn parse_redir_prefix() {
        let input = tokenize("> file.txt cmd", false).0;
        let cmd = parse(input, &mut vec![]).unwrap();
        let write_flags = OFlag::O_WRONLY | OFlag::O_CREAT | OFlag::O_TRUNC;
        assert_eq!(
            cmd,
            to_command_list(Op::Cmd {
                prog: Some(to_word("cmd")),
                args: Vec::new(),
                io: vec![CmdPrefix::IORedirect {
                    fd: STDOUT_FILENO,
                    target: Redir::File(to_word("file.txt"), write_flags),
                }],
                env: Vec::new(),
            })
        );
    }

    #[test]
    fn populate_heredocs() {
        let input = tokenize("cmd <<EOF", false).0;
        let contents: String = "contents".into();
        let cmd = parse(input, &mut vec![contents.clone()]).unwrap();
        assert_eq!(
            cmd,
            to_command_list(Op::Cmd {
                prog: Some(to_word("cmd")),
                args: Vec::new(),
                io: vec![CmdPrefix::IORedirect {
                    fd: STDIN_FILENO,
                    target: Redir::Temp(contents),
                }],
                env: Vec::new(),
            })
        );
    }

    #[test]
    fn async_command() {
        let input = tokenize("cmd &", false).0;
        let cmd = parse(input, &mut vec![]).unwrap();

        assert_eq!(
            cmd,
            vec![CommandList::SimpleList(Pipeline {
                seq: vec![Op::Cmd {
                    prog: Some(to_word("cmd")),
                    args: Vec::new(),
                    io: Vec::new(),
                    env: Vec::new(),
                }],
                async: true,
                bang: false,
            })]
        );
    }

    #[test]
    fn semi_separated() {
        let input = tokenize("cmdA; cmdB", false).0;
        let cmd = parse(input, &mut vec![]).unwrap();

        assert_eq!(
            cmd,
            vec![
                CommandList::SimpleList(Pipeline {
                    seq: vec![Op::Cmd {
                        prog: Some(to_word("cmdA")),
                        args: Vec::new(),
                        io: Vec::new(),
                        env: Vec::new(),
                    }],
                    async: false,
                    bang: false,
                }),
                CommandList::SimpleList(Pipeline {
                    seq: vec![Op::Cmd {
                        prog: Some(to_word("cmdB")),
                        args: Vec::new(),
                        io: Vec::new(),
                        env: Vec::new(),
                    }],
                    async: false,
                    bang: false,
                }),
            ]
        );
    }
}

use super::ast::*;
use super::token::{self, Token};
use combine::easy;
use combine::*;

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
    fn simple_command[I]()(I) -> SimpleCommand
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
            SimpleCommand::Cmd {
                prog: prog,
                args: args,
                env: env,
                io: io.into_iter().chain(redirect_suffix).collect(),
            }
        })
    }
}

parser! {
    fn if_statement[I]()(I) -> CompoundCommand
    where [
        I: Stream<Item = Token>,
    ] {
        let if_branch = || compound_list().skip(token(Token::Then)).and(compound_list()).map(|(condition, block)| IfBranch { condition, block });

        token(Token::If)
            .with(if_branch())
            .and(
                many(token(Token::Elif).with(if_branch()))
            )
            .and(
                optional(token(Token::Else).with(compound_list()))
            )
            .skip(token(Token::Fi))
            .map(|((if_part, mut elif_parts), else_block): ((IfBranch, Vec<IfBranch>), Option<Vec<Statement>>)| {
                let mut branches = vec![if_part];
                branches.append(&mut elif_parts);
                CompoundCommand::If {
                    branches,
                    else_block: else_block.map(Box::new),
                }
            })
    }
}

/*
compound_command : brace_group
                 | subshell
                 | for_clause
                 | case_clause
                 | if_clause
                 | while_clause
                 | until_clause
                 ;
*/
parser! {
    fn compound_command[I]()(I) -> CompoundCommand
    where [
        I: Stream<Item = Token>,
    ] {
        if_statement()
    }
}

parser! {
    fn command[I]()(I) -> Command
    where [
        I: Stream<Item = Token>,
    ] {
        simple_command().map(|cmd| Command::SimpleCommand(cmd))
            .or(compound_command().and(many(io_redirect())).map(|(cmd, redirs)| Command::CompoundCommand(cmd, redirs)))
            .expected("command")
    }
}

parser! {
    fn pipeline[I]()(I) -> Pipeline
    where [
        I: Stream<Item = Token>,
    ] {
        let cmd_chain = command().map(|op| vec![op]);
        let pipe = token(Token::Pipe).map(|_t| {
            return |mut lhs: Vec<Command>, mut rhs: Vec<Command>| {
                // Remove the first argument so we can distinguish it as the command name
                lhs.push(rhs.remove(0));
                lhs
            };
        });

        chainl1(cmd_chain, pipe)
            .map(|cmds| Pipeline {
                cmds,
                bang: false,
            })
    }
}

/*
and_or :                         pipeline
       | and_or AND_IF linebreak pipeline
       | and_or OR_IF  linebreak pipeline
       ;
*/
parser! {
    fn and_or[I]()(I) -> AndOrList
    where [
        I: Stream<Item = Token>,
    ] {
        chainr1(
            pipeline().map(AndOrList::Pipeline),
            choice([token(Token::AND), token(Token::OR)])
                .skip(linebreak())
                .map(|t: Token| {
                    move |l, r| {
                        let pipe = match r {
                            AndOrList::Pipeline(pipe) => pipe,
                            _ => unreachable!(),
                        };
                        if t == Token::AND {
                            AndOrList::And(Box::new(l), pipe)
                        } else {
                            AndOrList::Or(Box::new(l), pipe)
                        }
                    }
                }),
        )
    }
}

parser! {
    fn newline_list[I]()(I) -> ()
    where [
        I: Stream<Item = Token>,
    ] {
        skip_many1(token(Token::LineBreak))
    }
}

parser! {
    fn linebreak[I]()(I) -> ()
    where [
        I: Stream<Item = Token>,
    ] {
        skip_many(token(Token::LineBreak))
    }
}

fn and_or_to_statement(and_or: AndOrList) -> Vec<Statement> {
    vec![Statement::Seq(and_or)]
}

fn join_and_or_statements(
    (mut and_or, (sep_opt, mut tail_opt)): (
        Vec<Statement>,
        (Option<Token>, Option<Vec<Statement>>),
    ),
) -> Vec<Statement> {
    if let Some(sep) = sep_opt {
        if sep == Token::Async {
            let inner = match and_or.pop().unwrap() {
                Statement::Seq(inner) => inner,
                _ => unreachable!(),
            };
            and_or.push(Statement::Async(inner));
        }
    }
    if let Some(ref mut tail) = tail_opt {
        and_or.append(tail);
    }
    and_or
}

/*
compound_list    : linebreak term
                 | linebreak term separator
                 ;
term             : term separator and_or
                 |                and_or
                 ;
*/
fn compound_list<'a, I: 'a>(
) -> Box<Parser<Input = I, Output = Vec<Statement>, PartialState = ()> + 'a>
where
    I: Stream<Item = Token>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    // This may be a bug in combine, for now just doing something that works
    compound_list_inner().boxed()
}

parser! {
    fn compound_list_inner[I]()(I) -> Vec<Statement>
    where [
        I: Stream<Item = Token>,
    ] {
        linebreak()
            .with(and_or().map(and_or_to_statement))
            .and(
                separator()
                    .and(optional(compound_list()))
                    .map(|(sep, compound_list_opt)| (Some(sep), compound_list_opt))
                    .or(value((None, None))))
            .map(join_and_or_statements)
    }
}

parser! {
    fn separator_op[I]()(I) -> Token
    where [
        I: Stream<Item = Token>,
    ] {
        token(Token::Semi).or(token(Token::Async))
    }
}

parser! {
    fn separator[I]()(I) -> Token
    where [
        I: Stream<Item = Token>,
    ] {
        separator_op().or(token(Token::LineBreak)).skip(linebreak())
    }
}

/*
complete_command : list separator_op
                 | list
                 ;
list             : list separator_op and_or
                 |                   and_or
                 ;
*/
parser! {
    fn complete_command[I]()(I) -> Vec<Statement>
    where [
        I: Stream<Item = Token>,
    ] {
        and_or().map(and_or_to_statement)
            .and(
                separator_op()
                    .and(optional(complete_command()))
                    .map(|(sep, compound_list_opt)| (Some(sep), compound_list_opt))
                    .or(value((None, None))))
            .map(join_and_or_statements)
    }
}

/*
complete_commands: complete_commands newline_list complete_command
                 |                                complete_command
*/
parser! {
    fn complete_commands[I]()(I) -> Vec<Statement>
    where [
        I: Stream<Item = Token>,
    ] {
        complete_command()
            .and(
                newline_list()
                    .with(optional(complete_commands()).map(|cc| cc.unwrap_or(Vec::new())))
                    .or(value(vec![])))
            .map(|(mut l, mut r)| {
                l.append(&mut r);
                l
            })
    }
}

/*
program         : linebreak complete_commands linebreak
                | linebreak
                ;
*/
parser! {
    fn program[I]()(I) -> Option<Vec<Statement>>
    where [
        I: Stream<Item = Token>,
    ] {
        linebreak().with(optional(complete_commands()))
    }
}

pub fn parse(tokens: Vec<Token>, heredocs: &mut Vec<String>) -> Result<Vec<Statement>, String> {
    let input = easy::Stream(&tokens[..]);
    let parse_result = program().easy_parse(input).map_err(|e| format!("{:?}", e))?;

    // TODO handle partial input
    let result = parse_result.0;
    let script = if let Some(script) = result {
        script
    } else {
        return Ok(vec![]);
    };

    fn populate_heredocs(pipeline: &mut Pipeline, heredocs: &mut Vec<String>) {
        for cmd in pipeline.cmds.iter_mut() {
            match cmd {
                &mut Command::SimpleCommand(SimpleCommand::Cmd { ref mut io, .. }) => {
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

    let populated = script
        .into_iter()
        .map(|mut cmd_list_elem| {
            {
                let and_or = match cmd_list_elem {
                    Statement::Async(ref mut and_or) => and_or,
                    Statement::Seq(ref mut and_or) => and_or,
                };
                let mut pipeline = match and_or {
                    &mut AndOrList::And(_, ref mut pipeline) => pipeline,
                    &mut AndOrList::Or(_, ref mut pipeline) => pipeline,
                    &mut AndOrList::Pipeline(ref mut pipeline) => pipeline,
                };
                populate_heredocs(&mut pipeline, heredocs);
            }
            cmd_list_elem
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

    fn to_script(cmd: SimpleCommand) -> Script {
        vec![Statement::Seq(AndOrList::Pipeline(Pipeline {
            cmds: vec![Command::SimpleCommand(cmd)],
            bang: false,
        }))]
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
            to_script(SimpleCommand::Cmd {
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
            to_script(SimpleCommand::Cmd {
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
            to_script(SimpleCommand::Cmd {
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
            to_script(SimpleCommand::Cmd {
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
            to_script(SimpleCommand::Cmd {
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
            to_script(SimpleCommand::Cmd {
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
            vec![Statement::Seq(AndOrList::Pipeline(Pipeline {
                cmds: vec![
                    Command::SimpleCommand(SimpleCommand::Cmd {
                        prog: Some(to_word("cmdA")),
                        args: Vec::new(),
                        io: Vec::new(),
                        env: Vec::new(),
                    }),
                    Command::SimpleCommand(SimpleCommand::Cmd {
                        prog: Some(to_word("cmdB")),
                        args: vec![to_word("arg")],
                        io: Vec::new(),
                        env: Vec::new(),
                    }),
                ],
                bang: false,
            }))]
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
            to_script(SimpleCommand::Cmd {
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
            to_script(SimpleCommand::Cmd {
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
            to_script(SimpleCommand::Cmd {
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
            to_script(SimpleCommand::Cmd {
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
            vec![Statement::Async(AndOrList::Pipeline(Pipeline {
                cmds: vec![Command::SimpleCommand(SimpleCommand::Cmd {
                    prog: Some(to_word("cmd")),
                    args: Vec::new(),
                    io: Vec::new(),
                    env: Vec::new(),
                })],
                bang: false,
            }))]
        );
    }

    #[test]
    fn semi_separated() {
        let input = tokenize("cmdA; cmdB", false).0;
        let cmd = parse(input, &mut vec![]).unwrap();

        assert_eq!(
            cmd,
            vec![
                Statement::Seq(AndOrList::Pipeline(Pipeline {
                    cmds: vec![Command::SimpleCommand(SimpleCommand::Cmd {
                        prog: Some(to_word("cmdA")),
                        args: Vec::new(),
                        io: Vec::new(),
                        env: Vec::new(),
                    })],
                    bang: false,
                })),
                Statement::Seq(AndOrList::Pipeline(Pipeline {
                    cmds: vec![Command::SimpleCommand(SimpleCommand::Cmd {
                        prog: Some(to_word("cmdB")),
                        args: Vec::new(),
                        io: Vec::new(),
                        env: Vec::new(),
                    })],
                    bang: false,
                })),
            ]
        );
    }

    #[test]
    fn bare_if() {
        let input = tokenize("if true; then :; fi", false).0;
        let cmd = parse(input, &mut vec![]).unwrap();

        assert_eq!(
            cmd,
            vec![Statement::Seq(AndOrList::Pipeline(Pipeline {
                cmds: vec![Command::CompoundCommand(
                    CompoundCommand::If {
                        branches: vec![IfBranch {
                            condition: vec![Statement::Seq(AndOrList::Pipeline(Pipeline {
                                cmds: vec![Command::SimpleCommand(SimpleCommand::Cmd {
                                    prog: Some(to_word("true")),
                                    args: Vec::new(),
                                    io: Vec::new(),
                                    env: Vec::new(),
                                })],
                                bang: false,
                            }))],
                            block: vec![Statement::Seq(AndOrList::Pipeline(Pipeline {
                                cmds: vec![Command::SimpleCommand(SimpleCommand::Cmd {
                                    prog: Some(to_word(":")),
                                    args: Vec::new(),
                                    io: Vec::new(),
                                    env: Vec::new(),
                                })],
                                bang: false,
                            }))],
                        }],
                        else_block: None,
                    },
                    Vec::new(),
                )],
                bang: false,
            }))]
        );
    }

    #[test]
    fn multiline_if() {
        let input = tokenize(
            r#"
            if true
            then
                echo true
            fi
            "#,
            false,
        ).0;
        let cmd = parse(input, &mut vec![]);

        println!("{:?}", cmd);
        assert!(cmd.is_ok());
    }
}

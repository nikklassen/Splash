use std::fs::File;
use std::io::{BufRead, BufReader};

use libc::{STDIN_FILENO, STDOUT_FILENO};
use nix::unistd::{self, Pid};

use input::ast::*;
use input::parser::{self, ProgramParseResult};
use input::token::*;
use input::{prompt, tokenizer};
use interpolate;
use job;
use options;
use process::{self, CommandResult, Process};
use state::ShellState;
use util;

#[derive(Debug)]
pub enum InputReader {
    File(BufReader<File>),
    Command(Vec<String>),
    Stdin,
}

fn getline(input_method: &mut InputReader, cont: bool) -> Option<String> {
    match input_method {
        &mut InputReader::File(ref mut reader) => {
            let mut s = String::new();

            let res = reader.read_line(&mut s);
            if res.is_err() || res.unwrap() == 0 {
                None
            } else {
                // read_line doesn't remove trailing '\n', we also don't want any trailing '\r's
                Some(s.trim_right().to_string())
            }
        }
        &mut InputReader::Command(ref mut lines) => {
            if lines.is_empty() {
                None
            } else {
                Some(lines.remove(0))
            }
        }
        &mut InputReader::Stdin => prompt::getline(cont),
    }
}

pub fn eval(mut input_reader: InputReader, mut state: ShellState) {
    let mut last_status = 0;
    let mut line = String::new();

    loop {
        let cont = !line.is_empty();
        if let Some(next_line) = getline(&mut input_reader, cont) {
            line.push_str(&next_line);
        } else {
            if !cont && options::is_interactive(&state.opts) {
                println!("exit");
            }
            break;
        }

        let (tokens, unterminated) = tokenizer::tokenize(&line, false);
        if unterminated {
            line.push('\n');
            continue;
        }

        let mut input: Vec<String> = Vec::new();
        let mut here_docs: Vec<(Token, String)> = Vec::new();
        let mut i = 0;
        while i < tokens.len() {
            match tokens[i] {
                Token::DLESS | Token::DLESSDASH => {
                    if let Token::Word(ref s) = tokens[i + 1] {
                        here_docs.push((tokens[i].clone(), s.clone()));
                    } else {
                        print_err!("here docs must be strings");
                        continue;
                    }
                    i += 2;
                }
                _ => {
                    i += 1;
                }
            }
        }
        for (kind, here_doc) in here_docs {
            let mut content = String::new();
            loop {
                if let Some(mut s) = getline(&mut input_reader, true) {
                    if kind == Token::DLESSDASH {
                        s = s
                            .chars()
                            .skip_while(|c| c.is_whitespace())
                            .collect::<String>();
                    }
                    if s == here_doc {
                        input.push(content);
                        break;
                    }
                    content.push_str(&s);
                    content.push_str("\n");
                } else {
                    // Replicate other shells' behaviour, just ignore this heredoc
                    print_err!("warning: here-document delimited by end-of-file (wanted `EOF')");
                    input.push(content);
                    break;
                }
            }
        }

        let commands;
        match parser::parse(tokens, &mut input) {
            ProgramParseResult::Success(result) => {
                line = String::new();
                commands = result;
            }
            ProgramParseResult::Partial => {
                line.push_str("\n");
                continue;
            }
            ProgramParseResult::Error(e) => {
                print_err!("{}", e);
                line = String::new();
                continue;
            }
        }

        if commands.is_empty() {
            continue;
        }

        for mut command in commands {
            let res = run_statement(&mut state, &mut command);
            match res {
                Err(e) => {
                    print_err!("{}", e);
                }
                Ok(n) => {
                    last_status = n;
                }
            };
        }
    }

    ::std::process::exit(last_status);
}

fn run_statement(state: &mut ShellState, statement: &mut Statement) -> Result<i32, String> {
    match statement {
        Statement::Async(ref mut and_or) => run_and_or(state, and_or, true),
        Statement::Seq(ref mut and_or) => run_and_or(state, and_or, false),
    }
}

fn run_and_or(state: &mut ShellState, list: &mut AndOrList, async: bool) -> Result<i32, String> {
    let pipeline = match list {
        AndOrList::And(ref mut prev, p) => {
            let status = run_and_or(state, prev, false)?;
            if status != 0 {
                return Ok(status);
            }
            p
        }
        AndOrList::Or(ref mut prev, p) => {
            let status = run_and_or(state, prev, false)?;
            if status == 0 {
                return Ok(status);
            }
            p
        }
        AndOrList::Pipeline(p) => p,
    };
    run_pipeline(state, pipeline, async)
}

fn run_pipeline(
    state: &mut ShellState,
    pipeline: &mut Pipeline,
    async: bool,
) -> Result<i32, String> {
    let num_procs = pipeline.cmds.len();
    let mut prev_pipe_out = Redir::Copy(STDIN_FILENO);

    let multi = num_procs > 1;
    for i in 0..(num_procs - 1) {
        let p = &mut pipeline.cmds[i];
        let (pipe_out, pipe_in) = unistd::pipe().or_else(util::show_err)?;

        add_piped_io_to_command(p, prev_pipe_out, Redir::Pipe(pipe_in), multi);

        prev_pipe_out = Redir::Pipe(pipe_out);
    }

    let mut pgid = Pid::from_raw(0);
    let mut cmds = pipeline.cmds.iter_mut();
    // TODO all threads need to be spawned then the last waited for
    // the all others subsequently killed if not done
    for _ in 0..(num_procs - 1) {
        let mut cmd = cmds.next().unwrap();
        let CommandResult(proc, _builtin_result) = run_command(state, cmd, pgid, false)?;
        pgid = proc.pgid;
    }

    let last_cmd = cmds.next().unwrap();
    add_piped_io_to_command(last_cmd, prev_pipe_out, Redir::Copy(STDOUT_FILENO), multi);

    let CommandResult(last_proc, builtin_result) = run_command(state, last_cmd, pgid, async)?;

    let ret;
    {
        let job = job::add_job(&last_proc)?;
        ret = if builtin_result.is_some() {
            builtin_result.unwrap()
        } else if !options::is_interactive(&state.opts) {
            job::wait_for_job(&job)?
        } else if async {
            job::background_job(&job)?
        } else {
            job::foreground_job(&job)?
        };
        job::update_job_status(job.id);
    };
    job::update_job_list();

    Ok(ret)
}

fn add_piped_io_to_command(cmd: &mut Command, pipe_in: Redir, pipe_out: Redir, multi: bool) {
    let io = match cmd {
        Command::SimpleCommand(SimpleCommand::Cmd { ref mut io, .. }) => io,
        Command::CompoundCommand(_, ref mut io) => io,
        // EqlStmt
        _ => {
            return;
        }
    };

    let has_input = io
        .iter()
        .find(|io_item| is_match!(io_item, CmdPrefix::IORedirect { fd: STDIN_FILENO, .. }))
        .is_some();

    if multi && has_input {
        print_err!("splash: Ignoring piped input; programs may not behave as expected.");
    }
    // insert at the front so these file descriptors will be overwritten by anything later
    io.insert(
        0,
        CmdPrefix::IORedirect {
            fd: STDOUT_FILENO,
            target: pipe_out,
        },
    );
    io.insert(
        0,
        CmdPrefix::IORedirect {
            fd: STDIN_FILENO,
            target: pipe_in,
        },
    );
}

fn run_command(
    state: &mut ShellState,
    cmd: &mut Command,
    pgid: Pid,
    async: bool,
) -> Result<CommandResult, String> {
    match cmd {
        Command::SimpleCommand(simple_cmd) => run_simple_command(state, simple_cmd, pgid, async),
        Command::CompoundCommand(compound_cmd, redirs) => {
            run_compound_command(state, compound_cmd, redirs, async)
        }
    }
}

fn run_simple_command(
    state: &mut ShellState,
    cmd: &mut SimpleCommand,
    pgid: Pid,
    async: bool,
) -> Result<CommandResult, String> {
    process::exec_cmd(state, cmd, pgid, async)
}

fn run_statements(state: &mut ShellState, statements: &mut Vec<Statement>) -> Result<i32, String> {
    let mut last_result = 0;
    for statement in statements {
        last_result = run_statement(state, statement)?;
    }
    Ok(last_result)
}

fn run_compound_command(
    state: &mut ShellState,
    cmd: &mut CompoundCommand,
    // TODO
    _redirs: &Vec<CmdPrefix>,
    _async: bool,
) -> Result<CommandResult, String> {
    let wrap_result = |cmd_name: &str, result: i32| {
        Ok(CommandResult(
            Process::new(
                Some(cmd_name.to_string()),
                Vec::new(),
                Vec::new(),
                Vec::new(),
            ),
            Some(result),
        ))
    };
    match cmd {
        CompoundCommand::If {
            branches,
            else_block,
        } => {
            let mut last_result = 0;
            let mut has_branched = false;
            for IfBranch { condition, block } in branches {
                last_result = run_statements(state, condition)?;
                if last_result == 0 {
                    last_result = run_statements(state, block)?;
                    has_branched = true;
                    break;
                }
            }
            if !has_branched {
                if let Some(block) = else_block {
                    last_result = run_statements(state, block)?;
                }
            }
            wrap_result("if", last_result)
        }
        CompoundCommand::BraceGroup(block) => {
            let last_result = run_statements(state, block)?;
            wrap_result("{ .. }", last_result)
        }
        CompoundCommand::SubShell(block) => {
            let mut new_state = ShellState::from(state);
            let last_result = run_statements(&mut new_state, block)?;
            wrap_result("( .. )", last_result)
        }
        CompoundCommand::For {
            ref var,
            ref list,
            ref mut body,
        } => {
            let mut last_result = 0;
            for elem in list {
                let items = interpolate::expand_word(&elem, &mut state.env, false)?;
                for item in items {
                    state.env.set(var, item);
                    last_result = run_statements(state, body)?;
                }
            }
            wrap_result("for", last_result)
        }
    }
}

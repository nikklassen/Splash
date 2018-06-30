use std::ffi::CString;
use std::io::{Error, Seek, SeekFrom, Write};
use std::os::unix::io::IntoRawFd;
use std::path::Path;
use std::{iter, process};

use libc::STDIN_FILENO;
use nix;
use nix::errno::Errno;
use nix::fcntl::{self, OFlag};
use nix::sys::stat::Mode;
use nix::unistd::{self, execvp, getpid, setpgid, ForkResult, Pid};
use tempfile::tempfile;

use builtin::Builtin;
use env::UserEnv;
use file::Fd;
use input::ast::*;
use interpolate;
use options::{self, OptionTable};
use signals;
use state::ShellState;
use util;

// The main reason for essentially recreating Redir here is because
// we need to add the "Replace" condition.
#[derive(Debug)]
pub enum IOOp {
    Duplicate(i32),
    File(String, OFlag),
    FromString(String),
    Replace(i32),
}

#[derive(Debug)]
pub struct Process {
    pub pid: Pid,
    pub pgid: Pid,
    pub prog: Option<String>,
    pub args: Vec<String>,
    pub io: Vec<(i32, IOOp)>,
    pub async: bool,
    pub env: Vec<CmdPrefix>,
}

#[derive(Debug)]
pub struct CommandResult(pub Process, pub Option<i32>);

impl Process {
    pub fn new(
        prog: Option<String>,
        args: Vec<String>,
        env: Vec<CmdPrefix>,
        io: Vec<CmdPrefix>,
    ) -> Process {
        let new_io: Vec<(i32, IOOp)> = io
            .into_iter()
            .map(|prefix| {
                if let CmdPrefix::IORedirect { fd, target } = prefix {
                    let op = match target {
                        Redir::File(name, flags) => IOOp::File(name, flags),
                        Redir::Copy(fd) => IOOp::Duplicate(fd),
                        Redir::Temp(contents) => IOOp::FromString(contents),
                        Redir::Pipe(fd) => IOOp::Replace(fd),
                    };
                    (fd, op)
                } else {
                    unreachable!()
                }
            })
            .collect();

        Process {
            pid: Pid::from_raw(0),
            pgid: Pid::from_raw(0),
            prog: prog,
            args: args,
            io: new_io,
            async: false,
            env: env,
        }
    }
}

use std::fmt;
impl fmt::Display for Process {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref p) = self.prog {
            write!(f, "{}", p)?;
        }
        if self.prog.is_some() && !self.args.is_empty() {
            write!(f, " ")?;
        }
        if !self.args.is_empty() {
            write!(f, "{}", util::join_str(&self.args, " "))?;
        }
        Ok(())
    }
}

pub fn exec_cmd(
    state: &mut ShellState,
    cmd: SimpleCommand,
    pgid: Pid,
    async: bool,
) -> Result<CommandResult, String> {
    let mut proc = match cmd {
        SimpleCommand::Cmd {
            prog,
            args,
            io,
            env,
        } => Process::new(prog, args, env, io),
        _ => unreachable!(),
    };
    proc.async = async;

    if proc.prog.is_none() {
        update_env(&proc, &mut state.env);
        return Ok(CommandResult(proc, Some(0)));
    }

    // Interpolate parameters at the last possible moment
    interpolate::expand(&mut proc, &mut state.env)?;

    let ShellState {
        ref mut builtins,
        ref opts,
        ..
    } = state;
    let builtin_entry = proc
        .prog
        .as_ref()
        .and_then(|prog| builtins.get_mut(prog.as_str()));
    if let Some(cmd) = builtin_entry {
        let builtin_result = exec_builtin(&mut proc, cmd, pgid, &opts)?;
        if proc.pid == unistd::getpid() {
            Ok(CommandResult(proc, Some(builtin_result)))
        } else {
            Ok(CommandResult(proc, None))
        }
    } else {
        fork_proc(&mut proc, pgid, &opts).or_else(util::show_err)?;
        Ok(CommandResult(proc, None))
    }
}

pub fn exit(errno: i32) {
    signals::cleanup_signals();
    process::exit(errno);
}

fn update_env(p: &Process, user_env: &mut UserEnv) {
    for assignment in p.env.iter() {
        if let &CmdPrefix::Assignment { ref lhs, ref rhs } = assignment {
            user_env.vars.insert(lhs.clone(), rhs.clone());
        }
    }
}

fn add_redirects_to_io(io: &Vec<(i32, IOOp)>) -> Result<(), String> {
    // Order of i/o operations matters, that's why we wait until now to do anything
    // with them. For example "2> output >&2" should not output anything to stderr,
    // which was happening before when >&2 was dupping stderr before it had been redirected.
    for &(ref io_number, ref io_redirect) in io {
        let mut fd = match io_redirect {
            &IOOp::File(ref name, ref flags) => {
                let mode = Mode::S_IRUSR | Mode::S_IWUSR | Mode::S_IRGRP | Mode::S_IROTH;
                let path = Path::new(&name);
                let file = fcntl::open(path, *flags, mode).or_else(util::show_err)?;
                Fd::new(file)
            }
            &IOOp::Duplicate(fd) => {
                if fd == *io_number {
                    continue;
                }
                Fd::dup(fd)?
            }
            &IOOp::FromString(ref contents) => {
                let mut tmpfile =
                    tempfile().or(Err("Could not create temporary file".to_string()))?;
                tmpfile
                    .write_all(contents.as_bytes())
                    .or_else(util::show_err)?;
                tmpfile.seek(SeekFrom::Start(0)).or_else(util::show_err)?;
                Fd::new(tmpfile.into_raw_fd())
            }
            &IOOp::Replace(fd) => {
                if fd == *io_number {
                    continue;
                }
                Fd::new(fd)
            }
        };

        let raw_fd = fd.raw_fd;
        unistd::dup2(raw_fd, *io_number).map_err(|_| format!("{}: bad file descriptor", raw_fd))?;
        fd.close();
    }
    Ok(())
}

fn fork_process<F>(
    process: &mut Process,
    mut pgid: Pid,
    cmd: F,
    opts: &OptionTable,
) -> Result<(), nix::Error>
where
    F: FnOnce() -> Result<i32, Error>,
{
    let f = unistd::fork()?;
    if let ForkResult::Parent { child } = f {
        process.pid = child;
        if options::is_interactive(opts) {
            if pgid == Pid::from_raw(0) {
                pgid = child;
            }
            setpgid(child, pgid).unwrap();
            process.pgid = pgid;
        }

        // Close the child process's stdin/stdout fds
        for &(_io_number, ref redir) in process.io.iter() {
            if let &IOOp::Replace(ref fd) = redir {
                Fd::new(*fd).close();
            }
        }
    } else {
        if let Err(e) = add_redirects_to_io(&process.io) {
            println!("Got error: {}", e);
            exit(1);
        }

        if options::is_interactive(opts) {
            let pid = getpid();
            if pgid == Pid::from_raw(0) {
                pgid = pid;
            }
            setpgid(pid, pgid).unwrap();
            if !process.async {
                unistd::tcsetpgrp(STDIN_FILENO, pgid).unwrap();
            }

            // Reset signals
            signals::cleanup_signals();
        }

        let result = cmd();

        let exit_code = match result {
            Ok(ecode) => ecode,
            Err(e) => {
                print_err!("{:?}", e);
                127
            }
        };

        exit(exit_code);
    }
    Ok(())
}

fn exec_builtin(
    process: &mut Process,
    cmd: &mut Box<Builtin>,
    pgid: Pid,
    opts: &OptionTable,
) -> Result<i32, String> {
    let has_redirects = process.io.len() != 2
        || !is_match!(process.io[0], (0, IOOp::Duplicate(0)))
        || !is_match!(process.io[1], (1, IOOp::Duplicate(1)));

    if !has_redirects {
        process.pid = getpid();
        cmd.run(&process.args[..]).or_else(util::show_err)
    } else {
        let args = process.args.clone();
        fork_process(process, pgid, || cmd.run(&args[..]), opts).or_else(util::show_err)?;

        // This result doesn't actually matter since the forked process will be waited for
        Ok(0)
    }
}

fn fork_proc(process: &mut Process, pgid: Pid, opts: &OptionTable) -> Result<(), nix::Error> {
    let prog = process.prog.clone().unwrap();
    let args = process.args.clone();
    fork_process(
        process,
        pgid,
        move || {
            let args = &iter::once(&prog)
                .chain(args.iter())
                .map(|s| CString::new(s.as_bytes()).unwrap())
                .collect::<Vec<_>>()[..];

            let err = execvp(&CString::new(prog.as_bytes()).unwrap(), args).unwrap_err();

            match err {
                nix::Error::Sys(Errno::ENOENT) => print_err!("command not found: {}", prog),
                nix::Error::Sys(Errno::EACCES) => print_err!("permission denied: {}", prog),
                nix::Error::Sys(Errno::ENOTDIR) => print_err!("not a directory: {}", prog),
                _ => {}
            };
            Ok(127)
        },
        opts,
    )
}

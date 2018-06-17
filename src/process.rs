use std::ffi::CString;
use std::io::{Write, Seek, SeekFrom, Error};
use std::ops::IndexMut;
use std::os::unix::io::IntoRawFd;
use std::path::Path;
use std::{iter, process};

use libc::{STDOUT_FILENO, STDIN_FILENO};
use nix::sys::stat;
use nix::unistd::{self, ForkResult, execvp, getpid, setpgid, isatty};
use nix::fcntl::{self, OFlag};
use tempfile::tempfile;

use builtin::{Builtin, BuiltinMap};
use env::UserEnv;
use job;
use file::Fd;
use input::ast::*;
use interpolate;
use signals;
use util;
use options::{self, SOpt};

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
    pub pid: i32,
    pub pgid: i32,
    pub prog: Option<String>,
    pub args: Vec<String>,
    pub io: Vec<(i32, IOOp)>,
    pub async: bool,
    pub env: Vec<CmdPrefix>,
}

impl Process {
    pub fn new(prog: Option<String>, args: Vec<String>, env: Vec<CmdPrefix>, io: Vec<CmdPrefix>) -> Process {
        let new_io: Vec<(i32, IOOp)> = io.into_iter().map(|prefix| {
            if let CmdPrefix::IORedirect { fd, target } = prefix {
                let op = match target {
                    Redir::File(name, flags) => IOOp::File(name, flags),
                    Redir::Copy(fd) => IOOp::Duplicate(fd),
                    Redir::Temp(contents) => IOOp::FromString(contents),
                };
                (fd, op)
            } else {
                unreachable!()
            }
        }).collect();

        Process {
            pid: 0,
            pgid: 0,
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
        let prog = self.prog.clone().unwrap_or(String::new());
        if !self.args.is_empty() {
            write!(f, "{} {}",
                   prog,
                   util::join_str(&self.args, " "))
        } else {
            write!(f, "{}", prog)
        }
    }
}

pub fn run_processes(builtins: &mut BuiltinMap, command: CommandList, user_env: &mut UserEnv) -> Result<i32, String> {
    let pipeline = match command {
        CommandList::AndList(prev, p) => {
            let status = run_processes(builtins, *prev, user_env)?;
            if status != 0 {
                return Ok(status);
            }
            p
        },
        CommandList::OrList(prev, p) => {
            let status = run_processes(builtins, *prev, user_env)?;
            if status == 0 {
                return Ok(status);
            }
            p
        },
        CommandList::SimpleList(p) => p,
    };

    let mut procs = try!(pipeline_to_processes(pipeline).or_else(|e| Err(format!("{}", e))));
    let mut pgid = 0;
    let mut builtin_result: Result<i32, String> = Ok(0);

    // TODO all threads need to be spawned then the last waited for
    // the all others subsequently killed if not done
    let has_pipeline = procs.len() > 1;
    for mut p in procs.iter_mut() {
        if p.prog.is_none() {
            update_env(p, user_env);
            continue;
        }

        // Interpolate parameters at the last possible moment
        interpolate::expand(&mut p, user_env)?;

        let builtin_entry = builtins.get_mut(p.prog.as_ref().unwrap());
        if let Some(cmd) = builtin_entry {
            builtin_result = exec_builtin(p, cmd, pgid, has_pipeline);
        } else {
            fork_proc(p, pgid);
            pgid = p.pgid;
        }
    }

    let last_proc;
    match procs.iter().rev().find(|p| p.prog.is_some()) {
        Some(p) => last_proc = p,
        _ => return Ok(0),
    }
    let ret;
    {
        let job = job::add_job(&last_proc)?;
        ret = if job.pid == getpid() {
            builtin_result
        } else if !is_interactive() {
            job::wait_for_job(&job)
        } else if last_proc.async {
            job::background_job(&job)
        } else {
            job::foreground_job(&job)
        };
        job::update_job_status(job.id);
    };
    job::update_job_list();

    ret
}

pub fn exit(errno: i32) {
    signals::cleanup_signals();
    process::exit(errno);
}

macro_rules! str_vec {
    ( $( $name: expr ),* ) => {
        (vec![ $(String::from($name)),* ])
    }
}

fn update_env(p: &Process, mut user_env: &mut UserEnv) {
    for assignment in p.env.iter() {
        if let &CmdPrefix::Assignment { ref lhs, ref rhs } = assignment {
            let entry = user_env.vars.entry(lhs.clone())
                .or_insert(String::new());
            *entry = rhs.clone();
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
                let mode = stat::S_IRUSR | stat::S_IWUSR | stat::S_IRGRP | stat::S_IROTH;
                let path = Path::new(&name);
                let file = try!(fcntl::open(path, *flags, mode).or_else(util::show_err));
                let new_fd = Fd::new(file);
                new_fd
            },
            &IOOp::Duplicate(fd) => {
                if fd == *io_number {
                    continue;
                }
                try!(Fd::dup(fd))
            },
            &IOOp::FromString(ref contents) => {
                let mut tmpfile = try!(tempfile()
                                       .or(Err("Could not create temporary file".to_string())));
                try!(tmpfile.write_all(contents.as_bytes()).or_else(util::show_err));
                try!(tmpfile.seek(SeekFrom::Start(0)).or_else(util::show_err));
                Fd::new(tmpfile.into_raw_fd())
            },
            &IOOp::Replace(fd) => {
                if fd == *io_number {
                    continue;
                }
                Fd::new(fd)
            }
        };

        let raw_fd = fd.raw_fd;
        try!(unistd::dup2(raw_fd, *io_number)
             .map_err(|_| format!("{}: bad file descriptor", raw_fd)));
        fd.close();
    }
    Ok(())
}

fn pipeline_to_processes(pipeline: Pipeline) -> Result<Vec<Process>, String> {
    let cmds: Vec<Op> = pipeline.seq;

    let mut procs: Vec<Process> = cmds.into_iter()
        .map(|cmd| {
            match cmd {
                Op::Cmd { prog, args, io, env } => Process::new(prog, args, env, io),
                _ => unreachable!(),
            }
        })
        .collect();

    let num_procs = procs.len();
    let mut prev_pipe_out = IOOp::Duplicate(STDIN_FILENO);

    let has_input = |io: &Vec<(i32, IOOp)>| io.iter().find(|io_item| io_item.0 == STDIN_FILENO).is_some();

    for i in 0..(num_procs - 1) {
        let ref mut p = procs.index_mut(i);
        let (pipe_out, pipe_in) = unistd::pipe().unwrap();

        if has_input(&p.io) {
            print_err!(
                "splash: Ignoring piped input for {}; programs may not behave as expected.",
                p.prog.clone().unwrap_or("assignment".to_string()));
        }
        // insert at the front so these file descriptors will be overwritten by anything later
        p.io.insert(0, (STDOUT_FILENO, IOOp::Replace(pipe_in)));
        p.io.insert(0, (STDIN_FILENO, prev_pipe_out));

        prev_pipe_out = IOOp::Replace(pipe_out);
    }

    // End the borrow of procs before we return it
    {
        let mut last_proc = procs.last_mut().unwrap();
        last_proc.async = pipeline.async;

        if num_procs > 1 && has_input(&last_proc.io) {
            print_err!(
                "splash: Ignoring piped input for {}; programs may not behave as expected.",
                last_proc.prog.clone().unwrap_or("assignment".to_string()));
        }
        let stdout_dup = IOOp::Duplicate(STDOUT_FILENO);
        last_proc.io.insert(0, (STDOUT_FILENO, stdout_dup));
        last_proc.io.insert(0, (STDIN_FILENO, prev_pipe_out));
    }
    Ok(procs)
}


fn fork_process<F>(process: &mut Process, mut pgid: i32, cmd: F)
where F: FnOnce() -> Result<i32, Error> {
    let f = unistd::fork().unwrap();
    if let ForkResult::Parent { child } = f {
        process.pid = child;
        if is_interactive() {
            if pgid == 0 {
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

        if is_interactive() {
            let pid = getpid();
            if pgid == 0 {
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
}

fn exec_builtin(process: &mut Process, cmd: &mut Box<Builtin>, pgid: i32, has_pipeline: bool) -> Result<i32, String> {
    // BUG need to fork or revert i/o
    if !has_pipeline {
        if let Err(e) = add_redirects_to_io(&process.io) {
            return Err(e);
        }
        process.pid = getpid();
        cmd.run(&process.args[..]).or_else(util::show_err)
    } else {
        let args = process.args.clone();
        fork_process(process, pgid, || {
            cmd.run(&args[..])
        });

        // This result doesn't actually matter since the forked process will be waited for
        Ok(0)
    }
}

fn fork_proc(process: &mut Process, pgid: i32) {
    let prog = process.prog.clone().unwrap();
    let args = process.args.clone();
    fork_process(process, pgid, move || {
        let args = &iter::once(prog.clone())
            .chain(args)
            .map(|s| CString::new(s.as_bytes()).unwrap())
            .collect::<Vec<_>>()[..];

        let err = execvp(&CString::new(prog.as_bytes()).unwrap(), args).unwrap_err();

        // Take bitwise NOT so we can differentiate an internal error
        // from the process legitimately exiting with that status
        Ok(!(err.errno() as i8) as i32)
    });
}

fn is_interactive() -> bool {
    options::get_opt(SOpt::Interactive) && isatty(STDIN_FILENO).unwrap_or(false)
}


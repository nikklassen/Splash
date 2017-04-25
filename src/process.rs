use std::collections::HashMap;
use std::ffi::CString;
use std::io::{self, Write, Seek, SeekFrom, Error};
use std::ops::IndexMut;
use std::os::unix::io::IntoRawFd;
use std::path::Path;
use std::{iter, process};

use libc::{STDOUT_FILENO, STDIN_FILENO};
use nix::sys::stat;
use nix::unistd::{self, ForkResult, execvp, getpid, setpgid, isatty};
use nix::fcntl;
use tempfile::tempfile;

use bindings::nix::tcsetpgrp;
use env::UserEnv;
use job;
use file::Fd;
use input::ast::*;
use interpolate;
use signals;
use util;


#[derive(Debug)]
pub struct Process {
    pub pid: i32,
    pub pgid: i32,
    pub prog: Option<Word>,
    pub args: Vec<Word>,
    pub io: Vec<(i32, Fd)>,
    pub async: bool,
    pub env: Vec<CmdPrefix>,
}

impl Process {
    pub fn new(prog: Option<Word>, args: Vec<Word>, env: Vec<CmdPrefix>) -> Process {
        Process {
            pid: 0,
            pgid: 0,
            prog: prog,
            args: args,
            io: Vec::new(),
            async: false,
            env: env,
        }
    }

    /// Converts the `Word` value in the prog to a String
    pub fn expand_prog(&self) -> Option<String> {
        self.prog.as_ref().map(word_to_value)
    }

    /// Converts the `Word` value in each arg to a String
    pub fn expand_args(&self) -> Vec<String> {
        self.args.iter().map(word_to_value).collect()
    }
}

use std::fmt;
impl fmt::Display for Process {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let prog = self.expand_prog().unwrap_or(String::new());
        if !self.args.is_empty() {
            write!(f, "{} {}",
                   prog,
                   util::join_str(&self.expand_args(), " "))
        } else {
            write!(f, "{}", prog)
        }
    }
}

pub trait Builtin {
    fn run(&mut self, args: &[String]) -> io::Result<i32>;
}

pub type BuiltinMap = HashMap<String, Box<Builtin>>;

pub fn run_processes(builtins: &mut BuiltinMap, command: CommandList, user_env: &mut UserEnv) -> Result<i32, String> {
    // TODO multiple pipelines
    let pipeline = match command {
        CommandList::AndList(_, p) => p,
        CommandList::OrList(_, p) => p,
        CommandList::SimpleList(p) => p,
    };

    let mut procs = try!(pipeline_to_processes(pipeline).or_else(|e| Err(format!("{}", e))));
    let mut pgid = 0;

    // TODO all threads need to be spawned then the last waited for
    // the all others subsequently killed if not done
    for mut p in procs.iter_mut() {
        if p.prog.is_none() {
            update_env(p, user_env);
            continue;
        }

        // Interpolate parameters at the last possible moment
        interpolate::expand(&mut p, user_env);

        let builtin_entry = builtins.get_mut(&p.expand_prog().unwrap());
        if let Some(cmd) = builtin_entry {
            exec_builtin(p, cmd, pgid);
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
            Ok(0)
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
            *entry = word_to_value(rhs);
        }
    }
}

fn add_redirects_to_io(io: &mut Vec<(i32, Fd)>, redirects: &Vec<CmdPrefix>) -> Result<(), String> {
    for r in redirects {
        if let &CmdPrefix::IORedirect { fd: ref io_number, target: ref io_redirect } = r {
            let fd = match io_redirect {
                &Redir::File(ref name, ref flags) => {
                    let mode = stat::S_IRUSR | stat::S_IWUSR | stat::S_IRGRP | stat::S_IROTH;
                    let file_name = word_to_value(name);
                    let path = Path::new(&file_name);
                    let file = try!(fcntl::open(path, *flags, mode).or_else(util::show_err));
                    Fd::new(file)
                },
                &Redir::Copy(n) => try!(Fd::dup(n)),
                &Redir::Temp(ref contents) => {
                    let mut tmpfile = try!(tempfile()
                        .or(Err("Could not create temporary file".to_string())));
                    try!(tmpfile.write_all(contents.as_bytes()).or_else(util::show_err));
                    try!(tmpfile.seek(SeekFrom::Start(0)).or_else(util::show_err));
                    Fd::new(tmpfile.into_raw_fd())
                },
            };
            io.push((*io_number, fd));
        }
    }
    Ok(())
}

fn pipeline_to_processes(pipeline: Pipeline) -> Result<Vec<Process>, String> {
    let cmds: Vec<Op> = pipeline.seq;

    let mut procs: Vec<Process> = try!(util::sequence(cmds
        .into_iter()
        .map(|cmd| {
            match cmd {
                Op::Cmd { prog, args, io, env } => {
                    let mut p = Process::new(prog, args, env);
                    add_redirects_to_io(&mut p.io, &io)
                        .and(Ok(p))
                }
                _ => unreachable!()
            }
        })
        .collect()));

    let num_procs = procs.len();
    let mut prev_pipe_out = try!(Fd::dup(STDIN_FILENO));

    let has_input = |io: &Vec<(i32, Fd)>| io.iter().find(|io_item| io_item.0 == STDIN_FILENO).is_some();

    for i in 0..(num_procs - 1) {
        let ref mut p = procs.index_mut(i);
        let (pipe_out, pipe_in) = unistd::pipe().unwrap();

        if has_input(&p.io) {
            util::write_err(&format!(
                "splash: Ignoring piped input for {}; programs may not behave as expected.",
                p.expand_prog().unwrap_or("assignment".to_string())));
        }
        // insert at the front so these file descriptors will be overwritten by anything later
        p.io.insert(0, (STDOUT_FILENO, Fd::new(pipe_in)));
        p.io.insert(0, (STDIN_FILENO, prev_pipe_out));

        prev_pipe_out = Fd::new(pipe_out);
    }

    // End the borrow of procs before we return it
    {
        let mut last_proc = procs.last_mut().unwrap();
        last_proc.async = pipeline.async;

        if num_procs > 1 && has_input(&last_proc.io) {
            util::write_err(&format!(
                "splash: Ignoring piped input for {}; programs may not behave as expected.",
                last_proc.expand_prog().unwrap_or("assignment".to_string())));
        }
        let stdout_dup = try!(Fd::dup(STDOUT_FILENO));
        last_proc.io.insert(0, (STDOUT_FILENO, stdout_dup));
        last_proc.io.insert(0, (STDIN_FILENO, prev_pipe_out));
    }
    Ok(procs)
}

/// To be called after forking this function will reassign the file descriptors correctly for the
/// child process
fn remap_fds(process: &mut Process) -> Result<(), String> {
    for &mut (io_number, ref mut fd) in process.io.iter_mut() {
        let raw_fd = fd.raw_fd;
        if io_number == raw_fd { continue };

        try!(unistd::dup2(raw_fd, io_number)
             .map_err(|_| format!("{}: bad file descriptor", raw_fd)));
        fd.close();
    }
    Ok(())
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

        for &mut (_io_number, ref mut fd) in process.io.iter_mut() {
            fd.close();
        }
    } else {
        if let Err(e) = remap_fds(process) {
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
                tcsetpgrp(STDIN_FILENO, pgid).unwrap();
            }

            // Reset signals
            signals::cleanup_signals();
        }

        let result = cmd();

        let exit_code = match result {
            Ok(ecode) => ecode,
            Err(e) => {
                error!("{:?}", e);
                127
            }
        };

        exit(exit_code);
    }
}

fn exec_builtin(process: &mut Process, cmd: &mut Box<Builtin>, pgid: i32) {
    let args: Vec<String> = process.expand_args();
    let has_output = process.io.iter().find(|io_item| io_item.0 == STDOUT_FILENO).is_some();
    if !has_output {
        if let Err(e) = remap_fds(process) {
            println!("Got error: {}", e);
            return;
        }
        process.pid = getpid();
        let _ = cmd.run(&args[..]);
    } else {
        fork_process(process, pgid, || {
            cmd.run(&args[..])
        });
    }
}

fn fork_proc(process: &mut Process, pgid: i32) {
    let prog = process.expand_prog().unwrap();
    let args = process.expand_args();
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
    isatty(STDIN_FILENO).unwrap_or(false)
}


use builtin::{BuiltinMap, Builtin};
use file::Fd;
use lexer::{Op, Redir};
use std::path::Path;
use libc::{STDOUT_FILENO, STDIN_FILENO};
use nix::errno::Errno;
use nix::sys::wait::{self, WaitStatus};
use nix::sys::stat;
use nix::unistd::{self, Fork, execvp};
use nix::{fcntl, NixPath};
use std::ffi::CString;
use std::fmt::Debug;
use std::io::{self, Write};
use std::ops::IndexMut;
use std::{iter, process};
use std::result;
use util;

#[derive(Debug)]
pub struct Process {
    pub pid: i32,
    pub prog: String,
    pub args: Vec<String>,
    pub io: Vec<(i32, Fd)>,
}

impl Process {
    pub fn new(prog: String, args: Vec<String>) -> Process {
        Process {
            pid: 0,
            prog: prog,
            args: args,
            io: Vec::new(),
        }
    }
}

fn sequence<T, E>(v: Vec<Result<T, E>>) -> result::Result<Vec<T>, E>
where T: Debug, E: Debug + Clone {
    for res in v.iter() {
        if let &Err(ref e) = res {
            return Err(e.clone());
        }
    }
    Ok(v.into_iter().map(|i| i.unwrap()).collect())
}

pub fn run_processes(builtins: &mut BuiltinMap, command: Op) -> Result<i32, String> {
    let mut procs = try!(op_to_processes(command).or_else(|e| Err(format!("{}", e))));
    let mut result = Ok(0);

    // TODO all threads need to be spawned then the last waited for
    // the all others subsequently killed if not done
    for p in procs.iter_mut() {

        let builtin_entry = builtins.get_mut(&p.prog);
        if builtin_entry.is_none() {
            fork_proc(p);
            result = wait_for_pid(&p);
        } else {
            let cmd = builtin_entry.unwrap();
            fork_builtin(p, cmd);
            result = wait_for_pid(&p);
        }
    }

    result
}

fn wait_for_pid(p: &Process) -> Result<i32, String> {
    match wait::waitpid(p.pid, None) {
        Ok(WaitStatus::Exited(_pid, exit_code)) => {
            if exit_code < 0 {
                match Errno::from_i32(!exit_code as i32) {
                    Errno::ENOENT => {
                        util::write_err(&format!("splash: {}: command not found", p.prog))
                    }
                    Errno::EACCES => {
                        util::write_err(&format!("splash: permission denied: {}", p.prog))
                    }
                    Errno::ENOTDIR => {
                        util::write_err(&format!("splash: not a directory: {}", p.prog))
                    }
                    e => util::write_err(&format!("splash: {}: {}", e.desc(), p.prog)),
                }
                Ok(127)
            } else {
                Ok(exit_code as i32)
            }
        }
        e => show_err(e),
    }
}

macro_rules! str_vec {
    ( $( $name: expr ),* ) => {
        (vec![ $(String::from($name)),* ])
    }
}

fn add_redirects_to_io(io: &mut Vec<(i32, Fd)>, redirects: &Vec<(i32, Redir)>) -> Result<(), String> {
    for &(ref io_number, ref io_redirect) in redirects {
        let fd = match io_redirect {
            &Redir::File(ref name, ref flags) => {
                let mode = stat::S_IRUSR | stat::S_IWUSR | stat::S_IRGRP | stat::S_IROTH;
                let path = Path::new(name);
                let file = try!(fcntl::open(path, *flags, mode).or_else(show_err));
                Fd::new(file)
            },
            &Redir::Copy(n) => try!(Fd::dup(n)),
        };
        io.push((*io_number, fd));
    }
    Ok(())
}

fn has_fd(target_fd: i32, io: &Vec<(i32, Redir)>) -> bool {
    io.iter().any(|&(fd, _)| fd == target_fd)
}

fn op_to_processes(op: Op) -> Result<Vec<Process>, String> {
    match op {
        Op::Cmd { prog, args, io } => {
            let mut p = Process::new(prog, args);
            if has_fd(STDOUT_FILENO, &io) {
                let stdout_dup = try!(Fd::dup(STDOUT_FILENO));
                p.io.push((STDOUT_FILENO, stdout_dup));
            }
            if has_fd(STDIN_FILENO, &io) {
                let stdin_dup = try!(Fd::dup(STDIN_FILENO));
                p.io.push((STDIN_FILENO, stdin_dup));
            }
            try!(add_redirects_to_io(&mut p.io, &io));
            Ok(vec![p])
        }
        Op::Pipe { cmds } => {
            let mut procs: Vec<Process> = try!(sequence(cmds
                .into_iter()
                .map(|cmd| {
                    match cmd {
                        Op::Cmd { prog, args, io } => {
                            let mut p = Process::new(prog, args);
                            add_redirects_to_io(&mut p.io, &io)
                                .and(Ok(p))
                        }
                        _ => unreachable!(),
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
                        p.prog));
                }
                // insert at the front so these file descriptors will be overwritten by anything later
                p.io.insert(0, (STDOUT_FILENO, Fd::new(pipe_in)));
                p.io.insert(0, (STDIN_FILENO, prev_pipe_out));

                prev_pipe_out = Fd::new(pipe_out);
            }

            // End the borrow of procs before we return it
            {
                let mut last_proc = procs.last_mut().unwrap();
                if num_procs > 1 && has_input(&last_proc.io) {
                    util::write_err(&format!(
                        "splash: Ignoring piped input for {}; programs may not behave as expected.",
                        last_proc.prog));
                }
                let stdout_dup = try!(Fd::dup(STDOUT_FILENO));
                last_proc.io.insert(0, (STDOUT_FILENO, stdout_dup));
                last_proc.io.insert(0, (STDIN_FILENO, prev_pipe_out));
            }
            Ok(procs)
        }
        _ => unreachable!(),
    }
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

fn fork_builtin(process: &mut Process, cmd: &mut Box<Builtin>) {
    let f = unistd::fork().unwrap();
    if let Fork::Parent(pid) = f {
        process.pid = pid;
        for &mut (_io_number, ref mut fd) in process.io.iter_mut() {
            fd.close();
        }
    } else {
        if let Err(e) = remap_fds(process) {
            println!("Got error: {}", e);
            process::exit(1);
        }

        let result = cmd.run(&process.args[..]);
        let exit_code = match result {
            Ok(ecode) => ecode,
            Err(e) => {
                let stderr = io::stderr();
                writeln!(stderr.lock(), "{:?}", e).unwrap();
                127
            }
        };
        process::exit(exit_code);
    }
}

fn fork_proc(process: &mut Process) {
    let f = unistd::fork().unwrap();
    if let Fork::Parent(pid) = f {
        process.pid = pid;
        for &mut (_io_number, ref mut fd) in process.io.iter_mut() {
            fd.close();
        }
    } else {
        if let Err(e) = remap_fds(process) {
            println!("Got error: {}", e);
            process::exit(1);
        }

        let args = &iter::once(process.prog.clone())
            .chain(process.args.iter().cloned())
            .map(|s| CString::new(s.as_bytes()).unwrap())
            .collect::<Vec<_>>()[..];

        let err = execvp(&CString::new(process.prog.as_bytes()).unwrap(), args).unwrap_err();

        // Take bitwise NOT so we can differentiate an internal error
        // from the process legitimately exiting with that status
        process::exit(!(err.errno() as i8) as i32);
    }
}

fn show_err<S, T: Debug>(e: T) -> Result<S, String> {
    Err(format!("{:?}", e))
}

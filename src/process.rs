use builtin::{BuiltinMap, Builtin};
use file::Fd;
use lexer::{Op, FileMode};
use libc::{STDOUT_FILENO, STDIN_FILENO};
use nix::errno::Errno;
use nix::sys::wait::{self, WaitStatus};
use nix::unistd::{self, Fork, execvp};
use nix::fcntl;
use std::collections::HashMap;
use std::ffi::CString;
use std::fmt::Debug;
use std::fs::File;
use std::io::{self, Write};
use std::ops::IndexMut;
use std::{iter, process};
use util;

#[derive(Debug)]
pub struct Process {
    pub pid: i32,
    pub prog: String,
    pub args: Vec<String>,
    pub io: HashMap<i32, Fd>
}

impl Process {
    pub fn new(prog: String, args: Vec<String>) -> Process {
        Process {
            pid: 0,
            prog: prog,
            args: args,
            io: hash_map!{
                STDIN_FILENO => Fd::new(STDIN_FILENO),
                STDOUT_FILENO => Fd::new(STDOUT_FILENO),
            },
        }
    }
}

pub fn run_processes(builtins: &mut BuiltinMap, command: Op) -> Result<i32, String> {
    let mut procs = op_to_processes(command);
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
                        util::write_err(format!("splash: {}: command not found", p.prog))
                    }
                    Errno::EACCES => {
                        util::write_err(format!("splash: permission denied: {}", p.prog))
                    }
                    Errno::ENOTDIR => {
                        util::write_err(format!("splash: not a directory: {}", p.prog))
                    }
                    e => util::write_err(format!("splash: {}: {}", e.desc(), p.prog)),
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

fn op_to_processes(op: Op) -> Vec<Process> {
    match op {
        Op::Cmd { prog, args, io } => {
            let mut p = Process::new(prog, args);
            if !io.contains_key(&STDOUT_FILENO) {
                p.io.insert(STDOUT_FILENO, Fd::dup(STDOUT_FILENO));
            }
            if !io.contains_key(&STDIN_FILENO) {
                p.io.insert(STDIN_FILENO, Fd::dup(STDIN_FILENO));
            }
            for (io_number, (io_mode, io_file)) in io {
                let file = if io_mode == FileMode::Read {
                    File::open(io_file).unwrap()
                } else {
                    File::create(io_file).unwrap()
                };
                p.io.insert(io_number, Fd::from(file));
            }
            vec![p]
        }
        Op::Pipe { cmds } => {
            let mut procs: Vec<_> = cmds
                .into_iter()
                .map(|cmd| {
                    match cmd {
                        Op::Cmd { prog, args, io } => {
                            let mut p = Process::new(prog, args);
                            for (io_number, (io_mode, io_file)) in io {
                                let file = if io_mode == FileMode::Read {
                                    File::open(io_file).unwrap()
                                } else {
                                    File::create(io_file).unwrap()
                                };
                                p.io.insert(io_number, Fd::from(file));
                            }
                            p
                        }
                        _ => unreachable!(),
                    }
                })
                .collect();

            let num_procs = procs.len();
            let mut prev_pipe_out = Fd::dup(STDIN_FILENO);

            for i in 0..(num_procs - 1) {
                let ref mut p = procs.index_mut(i);
                let (pipe_out, pipe_in) = unistd::pipe().unwrap();
                // Replace the fake default stdin/stdout with the pipeline chain
                if let Some(&Fd::Raw(_)) = p.io.get(&STDOUT_FILENO) {
                    p.io.insert(STDOUT_FILENO, Fd::new(pipe_in));
                } else {
                    // Close the output since this process will be writing to a file
                    unistd::close(pipe_in).unwrap();
                }
                if let Some(&Fd::Raw(_)) = p.io.get(&STDIN_FILENO) {
                    p.io.insert(STDIN_FILENO, prev_pipe_out);
                } else if i != 0 {
                    // Ignore the previous output since this process is reading from a file,
                    // prev_pipe_out will be dropped and closed as necessary
                    util::write_err(format!(
                        "splash: Ignoring piped input for {}; programs may not behave as expected.",
                        p.prog));
                }

                prev_pipe_out = Fd::new(pipe_out);
            }

            // End the borrow of procs before we return it
            {
                let mut last_proc = procs.last_mut().unwrap();
                if let Some(&Fd::Raw(_)) = last_proc.io.get(&STDOUT_FILENO) {
                    last_proc.io.insert(STDOUT_FILENO, Fd::dup(STDOUT_FILENO));
                }
                // Like above, if prev_pipe_out is not used here it will be dropped and cleaned up
                if let Some(&Fd::Raw(_)) = last_proc.io.get(&STDIN_FILENO) {
                    last_proc.io.insert(STDIN_FILENO, prev_pipe_out);
                } else if num_procs > 1 {
                    util::write_err(format!(
                        "splash: Ignoring piped input for {}; programs may not behave as expected.",
                        last_proc.prog));
                }
            }
            procs
        }
        _ => panic!("{:?} is not executable", op),
    }
}

// Get the largest file descriptor we want to have open
// this will be passed to fcntl so we don't end up with conflicting file descriptors when we
// actually open them
fn reopen_no_conflict(io_table: &mut HashMap<i32, Fd>) {
    let mut first_unwanted = {
        io_table.keys().max().unwrap_or(&0) + 1
    };
    let redirects: Vec<_> =
        io_table.iter()
            .map(|(&io_number, fd)| (io_number, fd.get_fd()))
            .collect();
    for (io_number, fd) in redirects {
        if io_table.contains_key(&fd) {
            let new_fd = fcntl::fcntl(fd, fcntl::FcntlArg::F_DUPFD(first_unwanted)).unwrap();
            first_unwanted += 1;
            io_table.insert(io_number, Fd::new(new_fd));
        }
    }
}

/// To be called after forking this function will reassign the file descriptors correctly for the
/// child process
fn remap_fds(process: &mut Process) {
    reopen_no_conflict(&mut process.io);
    for (&io_number, fd) in process.io.iter_mut() {
        unistd::dup2(fd.get_fd(), io_number).unwrap();
        fd.close();
    }
}

fn fork_builtin(process: &mut Process, cmd: &mut Box<Builtin>) {
    let f = unistd::fork().unwrap();
    if let Fork::Parent(pid) = f {
        process.pid = pid;
        for (_io_number, fd) in process.io.iter_mut() {
            fd.close();
        }
    } else {
        remap_fds(process);

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
        for (_io_number, fd) in process.io.iter_mut() {
            fd.close();
        }
    } else {
        remap_fds(process);

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

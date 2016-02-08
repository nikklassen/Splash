use builtin::{BuiltinMap, Builtin};
use lexer::Op;
use libc::{STDOUT_FILENO, STDIN_FILENO};
use nix::errno::Errno;
use std::io::{self, Write};
use nix::sys::wait::{self, WaitStatus};
use nix::unistd::{self, Fork, execvp};
use std::ffi::CString;
use std::fmt::Debug;
use std::{iter, process};
use std::process::exit;
use util;

#[derive(Debug)]
pub struct Process {
    pub pid: i32,
    pub prog: String,
    pub args: Vec<String>,
    pub stdin: Pipe,
    pub stdout: Pipe,
}

impl Process {
    pub fn new(prog: String, args: Vec<String>) -> Process {
        Process {
            pid: 0,
            prog: prog,
            args: args,
            stdin: Pipe::new(STDIN_FILENO),
            stdout: Pipe::new(STDOUT_FILENO),
        }
    }
}

#[derive(Debug)]
pub struct Pipe {
    fd: i32,
    is_closed: bool,
}

impl Drop for Pipe {
    fn drop(&mut self) {
        if self.fd >= 3 {
            self.close();
        }
    }
}

impl From<i32> for Pipe {
    fn from(fd: i32) -> Pipe {
        Pipe::new(unistd::dup(fd).unwrap())
    }
}

impl Pipe {
    fn new(fd: i32) -> Pipe {
        Pipe {
            fd: fd,
            is_closed: false,
        }
    }

    fn as_stdout(&mut self) {
        self.switch_fd(STDOUT_FILENO);
    }

    fn as_stdin(&mut self) {
        self.switch_fd(STDIN_FILENO);
    }

    fn close(&mut self) {
        if self.is_closed {
            return;
        }

        unistd::close(self.fd).unwrap();
        self.is_closed = true;
    }

    fn switch_fd(&mut self, new_fd: i32) {
        if self.fd == new_fd {
            return;
        }

        unistd::dup2(self.fd, new_fd).unwrap();
        unistd::close(self.fd).unwrap();
        self.fd = new_fd;
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
        Op::Cmd { prog, args } => {
            let mut p = Process::new(prog, args);
            p.stdout = Pipe::from(STDOUT_FILENO);
            p.stdin = Pipe::from(STDIN_FILENO);
            vec![p]
        }
        Op::Pipe { cmds } => {
            let mut procs: Vec<_> = cmds.into_iter()
                                        .map(|cmd| {
                                            match cmd {
                                                Op::Cmd { prog, args } => Process::new(prog, args),
                                                _ => unreachable!(),
                                            }
                                        })
                                        .collect();

            let num_procs = procs.len();
            let mut prev_pipe_out = Pipe::from(STDIN_FILENO);

            for p in &mut procs[..num_procs - 1] {
                let (pipe_out, pipe_in) = unistd::pipe().unwrap();
                p.stdout = Pipe::new(pipe_in);
                p.stdin = prev_pipe_out;

                prev_pipe_out = Pipe::new(pipe_out);
            }

            // End the borrow of procs before we return it
            {
                let mut last_proc = procs.last_mut().unwrap();
                last_proc.stdout = Pipe::from(STDOUT_FILENO);
                last_proc.stdin = prev_pipe_out;
            }
            procs
        }
        _ => panic!("{:?} is not executable", op),
    }
}

fn fork_builtin(process: &mut Process, cmd: &mut Box<Builtin>) {
    let f = unistd::fork().unwrap();
    if let Fork::Parent(pid) = f {
        process.pid = pid;
        process.stdout.close();
        process.stdin.close();
    } else {
        process.stdout.as_stdout();
        process.stdin.as_stdin();

        let result = cmd.run(&process.args[..]);
        let exit_code = match result {
            Ok(ecode) => ecode,
            Err(e) => {
                let stderr = io::stderr();
                writeln!(stderr.lock(), "{:?}", e).unwrap();
                127
            }
        };
        exit(exit_code);
    }
}

fn fork_proc(process: &mut Process) {
    let f = unistd::fork().unwrap();
    if let Fork::Parent(pid) = f {
        process.pid = pid;
        process.stdout.close();
        process.stdin.close();
    } else {
        process.stdout.as_stdout();
        process.stdin.as_stdin();

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

use builtin::BuiltinMap;
use lexer::Op;
use libc::{c_char, STDOUT_FILENO, STDIN_FILENO};
use nix::errno::Errno;
use nix::sys::wait::{self, WaitStatus};
use nix::unistd::{self, Fork};
use nix::{self, Error};
use std::ffi::CString;
use std::fmt::Debug;
use std::{env, iter, process};
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
        if self.fd < 3 {
            return;
        }
        self.close();
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
    let mut procs = op_to_process(command);
    let mut result = Ok(0);

    /* TODO all threads need to be spawned then the last waited for
     * the all others subsequently killed if not done
     */
    for p in procs.iter_mut() {

        let builtin_entry = builtins.get_mut(&p.prog);
        if builtin_entry.is_none() {
            fork_proc(p);
            match wait::waitpid(p.pid, None) {
                Ok(WaitStatus::Exited(_pid, exit_code)) => {
                    if exit_code < 0 {
                        match Errno::from_i32(!exit_code as i32) {
                            Errno::ENOENT => util::write_err(format!("splash: {}: command not found", p.prog)),
                            Errno::EACCES => util::write_err(format!("splash: permission denied: {}", p.prog)),
                            Errno::ENOTDIR => util::write_err(format!("splash: not a directory: {}", p.prog)),
                            e => util::write_err(format!("splash: {}: {}", e.desc(), p.prog)),
                        }
                        result = Ok(127);
                    } else if exit_code != 0 {
                        result = Ok(exit_code as i32);
                    }
                },
                e => {
                    result = show_err(e);
                },
            };
        } else {

            p.stdout.as_stdout();
            p.stdin.as_stdin();

            let cmd = builtin_entry.unwrap();
            result = cmd.run(&p.args[..])
                .or_else(show_err);

            p.stdout.close();
            p.stdin.close();
        }
    }

    result
}

macro_rules! str_vec {
    ( $( $name: expr ),* ) => {
        (vec![ $(String::from($name)),* ])
    }
}

fn op_to_process(op: Op) -> Vec<Process> {
    match op {
        Op::Cmd { prog, args } => vec!(Process::new(prog, args)),
        Op::Pipe { cmds } => {
            let mut procs: Vec<_> = cmds
                .into_iter()
                .flat_map(op_to_process)
                .collect();

            let num_procs = procs.len();
            if num_procs == 1 {
                return procs;
            }

            let mut prev_pipe_read = Pipe::from(STDIN_FILENO);

            for p in &mut procs[..num_procs-1] {
                let (pipe_read, pipe_write) = unistd::pipe().unwrap();
                p.stdout = Pipe::new(pipe_write);
                p.stdin = prev_pipe_read;

                prev_pipe_read = Pipe::new(pipe_read);
            }

            // End the borrow of procs before we return it
            {
                let mut last_proc = procs.last_mut().unwrap();
                last_proc.stdout = Pipe::from(STDOUT_FILENO);
                last_proc.stdin = prev_pipe_read;
            }
            procs
        },
        _ => panic!("{:?} is not executable", op),
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

// TODO: Temp until nix-rust is updated
mod ffi {
    use libc::{c_char, c_int};

    extern {
        pub fn execvp(filename: *const c_char, argv: *const *const c_char) -> c_int;

        pub static mut environ: *const *const c_char;
    }
}

#[inline]
pub fn execvp(filename: &CString, args: &[CString]) -> nix::Result<()> {
    use std::ptr;

    let env_vars: Vec<_> = env::vars()
        .map(|(k, v)|
             CString::new([k, v].join("=").as_bytes())
             .unwrap()
             .as_ptr())
        .collect();

    let mut args_p: Vec<*const c_char> = args.iter().map(|s| s.as_ptr()).collect();
    args_p.push(ptr::null());

    unsafe {
        ffi::environ = env_vars.as_ptr();
        ffi::execvp(filename.as_ptr(), args_p.as_ptr())
    };

    Err(Error::Sys(Errno::last()))
}

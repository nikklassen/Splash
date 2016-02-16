use libc::{STDOUT_FILENO, STDIN_FILENO};
use nix::unistd;
use std::os::unix::io::{RawFd, AsRawFd};
use std::fs::File;

#[derive(Debug)]
pub enum Fd {
    Pipe(Pipe),
    File(File),
}

/// Reopen `fd` as stdin
pub fn as_stdin(fd: &mut Fd) {
    if fd.get_fd() == STDIN_FILENO {
        return;
    }

    unistd::dup2(fd.get_fd(), STDIN_FILENO).unwrap();
    fd.close();
}

/// Reopen `fd` as stdout
pub fn as_stdout(fd: &mut Fd) {
    if fd.get_fd() == STDOUT_FILENO {
        return;
    }

    unistd::dup2(fd.get_fd(), STDOUT_FILENO).unwrap();
    fd.close();
}

#[derive(Debug)]
pub struct Pipe {
    fd: i32,
    is_closed: bool,
}

impl Pipe {
    pub fn new(fd: i32) -> Pipe {
        Pipe {
            fd: fd,
            is_closed: false,
        }
    }

    pub fn close(&mut self) {
        if self.is_closed {
            return;
        }

        unistd::close(self.fd).unwrap();
        self.is_closed = true;
    }
}

impl Drop for Pipe {
    fn drop(&mut self) {
        if self.fd > 2 && !self.is_closed {
            self.close();
        }
    }
}

impl From<i32> for Pipe {
    /// Actually duplicates the file descriptor
    fn from(fd: i32) -> Pipe {
        Pipe::new(unistd::dup(fd).unwrap())
    }
}

impl Fd {
    /// Wraps an existing `fd` in a pipe object
    pub fn new_pipe(fd: i32) -> Fd {
        Fd::Pipe(Pipe::new(fd))
    }

    pub fn get_fd(&self) -> RawFd {
        match self {
            &Fd::Pipe(ref p) => p.fd,
            &Fd::File(ref f) => f.as_raw_fd(),
        }
    }

    pub fn close(&mut self) {
        match self {
            &mut Fd::Pipe(ref mut p) => p.close(),
            // no-op since files automatically closed
            &mut Fd::File(..) => {},
        }
    }
}

impl From<File> for Fd {
    fn from(f: File) -> Fd {
        Fd::File(f)
    }
}

impl From<i32> for Fd {
    fn from(fd: i32) -> Fd {
        Fd::Pipe(Pipe::from(fd))
    }
}

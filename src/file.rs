use nix::unistd;
use std::os::unix::io::{RawFd, AsRawFd};
use std::fs::File;

#[derive(Debug)]
pub enum Fd {
    Raw(Raw),
    File(File),
}

#[derive(Debug)]
pub struct Raw {
    fd: i32,
    is_closed: bool,
}

impl Raw {
    pub fn new(fd: i32) -> Raw {
        Raw {
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

impl Drop for Raw {
    fn drop(&mut self) {
        if self.fd > 2 && !self.is_closed {
            self.close();
        }
    }
}

impl Fd {
    /// Wraps an existing `fd` in a raw file descriptor object
    pub fn new(fd: i32) -> Fd {
        Fd::Raw(Raw::new(fd))
    }

    /// Duplicates the file descriptor
    pub fn dup(fd: i32) -> Fd {
        Fd::Raw(Raw::new(unistd::dup(fd).unwrap()))
    }

    pub fn get_fd(&self) -> RawFd {
        match self {
            &Fd::Raw(ref p) => p.fd,
            &Fd::File(ref f) => f.as_raw_fd(),
        }
    }

    pub fn close(&mut self) {
        match self {
            &mut Fd::Raw(ref mut p) => p.close(),
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

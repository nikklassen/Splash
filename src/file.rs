use nix::unistd;
use std::os::unix::io::RawFd;

#[derive(Debug)]
pub struct Fd {
    pub raw_fd: RawFd,
    is_closed: bool,
}

impl Fd {
    /// Wraps an existing `fd` in our file descriptor object
    pub fn new(fd: RawFd) -> Fd {
        Fd {
            raw_fd: fd,
            is_closed: false,
        }
    }

    /// Duplicates the file descriptor
    pub fn dup(fd: RawFd) -> Result<Fd, String> {
        let new_fd = try!(unistd::dup(fd)
            .or(Err(format!("{}: bad file descriptor", fd))));
        Ok(Fd::new(new_fd))
    }

    pub fn close(&mut self) {
        if self.is_closed {
            return;
        }

        unistd::close(self.raw_fd).unwrap();
        self.is_closed = true;
    }
}

impl Drop for Fd {
    fn drop(&mut self) {
        // Don't automatically drop any of the default file descriptors
        if self.raw_fd > 2 && !self.is_closed {
            self.close();
        }
    }
}

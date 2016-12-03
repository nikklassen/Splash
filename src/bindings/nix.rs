//! This file includes code from upstream nix-rust (https://github.com/nix-rust/nix/blob/master/src/unistd.rs)
//! TODO remove these functions when the next version of nix-rust is released

use nix::{Errno, Result};
use libc::{self, pid_t, c_int};

#[inline]
pub fn getpgrp() -> Result<pid_t> {
    let res = unsafe { libc::getpgrp() };
    Errno::result(res)
}

#[inline]
pub fn tcgetpgrp(fd: c_int) -> Result<pid_t> {
    let res = unsafe { libc::tcgetpgrp(fd) };
    Errno::result(res)
}

#[inline]
pub fn tcsetpgrp(fd: c_int, pgrp: pid_t) -> Result<()> {
    let res = unsafe { libc::tcsetpgrp(fd, pgrp) };
    Errno::result(res).map(drop)
}

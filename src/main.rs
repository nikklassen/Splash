extern crate combine;
extern crate getopts;
extern crate libc;
extern crate nix;
extern crate tempfile;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate log;

#[macro_use]
pub mod util;

#[cfg(test)]
#[macro_use]
mod test_fixture;

pub mod builtin;
pub mod env;
pub mod file;
pub mod job;
pub mod logger;
pub mod process;
pub mod signals;
pub mod input;
mod interpolate;

#[allow(dead_code, non_camel_case_types)]
mod bindings;

use getopts::Options;

use signals::initialize_signals;
use process::BuiltinMap;

fn main() {
    use std::env;

    let args: Vec<String> = env::args().collect();

    let mut opts = Options::new();
    opts.optflag("V", "version", "show version information");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };
    if matches.opt_present("V") {
        print_version();
        return;
    }

    let log_level = if cfg!(debug_assertions) {
        logger::LogLevel::Debug
    } else {
        logger::LogLevel::Info
    };
    // TODO should be basename of args[0]
    logger::init("splash", log_level).unwrap();

    let builtins = initialize_term();
    input::prompt::input_loop(builtins);
}

fn initialize_term() -> BuiltinMap {
    use libc::STDIN_FILENO;
    use nix::unistd;
    use nix::sys::signal;
    use bindings::nix::{tcgetpgrp, tcsetpgrp, getpgrp};

    // See if we are running interactively
    let shell_terminal = STDIN_FILENO;
    let interactive = unistd::isatty(shell_terminal).unwrap();
    let mut shell_pgid;

    initialize_signals();

    let builtins = builtin::init_builtins();

    if interactive {
        // Loop until we are in the foreground.
        loop {
            match getpgrp() {
                Ok(id) => { shell_pgid = id; },
                Err(_) => { continue; }
            }
            let term_grp;
            match tcgetpgrp(shell_terminal) {
                Ok(id) => { term_grp = id; },
                Err(_) => { continue; }
            }

            if term_grp != shell_pgid {
                signal::kill(shell_pgid, signal::SIGTTIN).unwrap();
            } else {
                break;
            }
        }

        // Put ourselves in our own process group
        shell_pgid = unistd::getpid();
        if let Err(_) = unistd::setpgid(shell_pgid, shell_pgid) {
            panic!("Couldn't put the shell in its own process group");
        }

        // Grab control of the terminal
        tcsetpgrp(shell_terminal, shell_pgid).unwrap();
    }

    builtins
}

fn print_version() {
    print!("Splash 0.0.1
Copyright (c) 2016 Nik Klassen and Dan Reynolds
License GPLv3+: GNU GPL version 3 or later
<http://www.gnu.org/licenses/gpl.html>.");
}

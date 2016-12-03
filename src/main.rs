extern crate combine;
extern crate getopts;
extern crate nix;
extern crate libc;
extern crate tempfile;

#[macro_use]
extern crate lazy_static;

#[macro_use]
pub mod util;

#[cfg(test)]
#[macro_use]
mod test_fixture;

pub mod builtin;
pub mod env;
pub mod interpolate;
pub mod lexer;
pub mod process;
pub mod prompt;
pub mod tokenizer;
pub mod file;
pub mod signals;

#[allow(dead_code, non_camel_case_types)]
mod bindings;

use getopts::Options;
use signals::initialize_signals;

fn main() {
    use std::env;

    let args: Vec<String> = env::args().collect();

    let mut opts = Options::new();
    opts.optflag("v", "version", "show version information");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };
    if matches.opt_present("v") {
        print_version();
        return;
    }

    initialize_term();
    prompt::input_loop();
}

fn initialize_term() {
    use libc::STDIN_FILENO;
    use nix::unistd;
    use nix::sys::signal;
    use bindings::nix::{tcgetpgrp, tcsetpgrp, getpgrp};

    // See if we are running interactively
    let shell_terminal = STDIN_FILENO;
    let interactive = unistd::isatty(shell_terminal).unwrap();
    let mut shell_pgid;

    if !interactive {
        return;
    }

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

    initialize_signals();

    /* Put ourselves in our own process group.  */
    shell_pgid = unistd::getpid();
    if let Err(_) = unistd::setpgid(shell_pgid, shell_pgid) {
        panic!("Couldn't put the shell in its own process group");
    }

    // Grab control of the terminal
    tcsetpgrp(shell_terminal, shell_pgid).unwrap();
}

fn print_version() {
    print!("Splash 0.0.1
Copyright (c) 2016 Nik Klassen and Dan Reynolds
License GPLv3+: GNU GPL version 3 or later
<http://www.gnu.org/licenses/gpl.html>.");
}

extern crate combine;
extern crate getopts;
extern crate nix;
extern crate libc;

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
mod readline;

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

    initialize_signals();
    prompt::input_loop();
}

fn print_version() {
    print!("Splash 0.0.1
Copyright (c) 2016 Nik Klassen and Dan Reynolds
License GPLv3+: GNU GPL version 3 or later
<http://www.gnu.org/licenses/gpl.html>.");
}

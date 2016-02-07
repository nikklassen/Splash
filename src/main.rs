extern crate readline;
extern crate combine;
extern crate getopts;
extern crate nix;
extern crate libc;

#[cfg(test)]
#[macro_use]
extern crate lazy_static;

#[macro_use]
mod util;

#[cfg(test)]
#[macro_use]
mod test_fixture;

mod builtin;
mod env;
mod interpolate;
mod lexer;
mod process;
mod prompt;
mod tokenizer;

use getopts::Options;

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

    prompt::input_loop();
}

fn print_version() {
    print!("Splash 0.0.1
Copyright (c) 2016 Nik Klassen and Dan Reynolds
License GPLv3+: GNU GPL version 3 or later
<http://www.gnu.org/licenses/gpl.html>.");
}

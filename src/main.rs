#![allow(unused_features)]
#![feature(std_misc)]
#![feature(static_mutex)]
#![feature(box_syntax)]
#![feature(libc)]

extern crate readline;
extern crate parser_combinators;
extern crate getopts;
extern crate nix;
extern crate libc;

#[cfg(test)]
#[macro_use]
mod test_fixture;

mod prompt;
mod parser;
mod builtin;
mod signals;

use signals::initialize_signals;

fn main() {
    initialize_signals();
    prompt::input_loop();
}

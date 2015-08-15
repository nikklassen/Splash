#![allow(unused_features)]
#![feature(static_mutex)]
#![feature(box_syntax)]

extern crate readline;
extern crate parser_combinators;
extern crate getopts;
extern crate nix;
extern crate libc;

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

fn main() {
    prompt::input_loop();
}

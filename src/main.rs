#![feature(std_misc)]

extern crate readline;
extern crate parser_combinators;
extern crate getopts;
extern crate regex;

#[macro_use]
mod tests;

mod prompt;
mod parser;
mod builtin;

fn main() {
    prompt::input_loop();
}

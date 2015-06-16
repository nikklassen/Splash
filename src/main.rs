#![feature(std_misc)]
extern crate readline;
extern crate parser_combinators;
extern crate getopts;
extern crate regex;

mod prompt;
mod parser;
mod builtin;

mod tests;

fn main() {
    prompt::input_loop();
}

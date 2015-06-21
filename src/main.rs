#![feature(std_misc)]
#![feature(box_syntax)]

extern crate readline;
extern crate parser_combinators;
extern crate getopts;

#[macro_use]
mod test_fixture;

mod prompt;
mod parser;
mod builtin;

fn main() {
    prompt::input_loop();
}

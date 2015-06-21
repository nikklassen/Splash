#![allow(unused_features)]
#![feature(static_mutex)]
#![feature(box_syntax)]

extern crate readline;
extern crate parser_combinators;
extern crate getopts;

#[cfg(test)]
#[macro_use]
mod test_fixture;

mod prompt;
mod parser;
mod builtin;

fn main() {
    prompt::input_loop();
}

#![allow(unused_features)]
#![feature(static_mutex)]
#![feature(box_syntax)]

extern crate readline;
extern crate parser_combinators;
extern crate getopts;

#[cfg(test)]
#[macro_use]
mod test_fixture;

macro_rules! is_match {
    ($e: expr, $p: pat) => ((
        if let $p = $e { true } else { false }
    ))
}

mod builtin;
mod env;
mod interpolate;
mod lexer;
mod prompt;
mod tokenizer;

fn main() {
    prompt::input_loop();
}

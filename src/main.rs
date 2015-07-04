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

mod prompt;
mod lexer;
mod builtin;
mod tokenizer;
mod interpolate;

fn main() {
    prompt::input_loop();
}

extern crate readline;
extern crate parser_combinators;
extern crate getopts;

mod prompt;
mod parser;
mod builtin;

fn main() {
    prompt::input_loop();
}

extern crate readline;
extern crate getopts;

mod prompt;
mod builtin;

fn main() {
    prompt::input_loop();
}

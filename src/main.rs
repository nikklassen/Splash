extern crate readline;
mod prompt;
mod builtin;

fn main() {
    prompt::input_loop();
}

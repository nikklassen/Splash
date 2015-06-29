#![allow(unused_features)]
#![feature(static_mutex)]
#![feature(box_syntax)]

extern crate readline;
extern crate parser_combinators;
extern crate getopts;
extern crate nix;

#[cfg(test)]
#[macro_use]
mod test_fixture;

mod prompt;
mod parser;
mod builtin;
mod signals;

use signals::handle_signal;
use nix::sys::signal;

fn main() {
    let sig_action = signal::SigAction::new(
        handle_signal,
        signal::SockFlag::empty(),
        signal::SigSet::empty()
    );
    unsafe { signal::sigaction(signal::SIGINT, &sig_action); }
    prompt::input_loop();
}

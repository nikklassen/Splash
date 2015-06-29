use nix::sys::signal;
use std::fs;

pub extern fn handle_signal(code: i32) {
    match code {
        signal::SIGINT => {
            println!("Handled Interrupt.");
        },
        _ => {println!("#{}", code)}
    }
}

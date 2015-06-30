use nix::sys::signal;
use libc::c_int;

static SIGNALS: [c_int; 2] = [
    signal::SIGINT,
    signal::SIGTSTP
];

pub fn initialize_signals() {
    let sig_action = signal::SigAction::new(
        handle_signal,
        signal::SockFlag::empty(),
        signal::SigSet::empty()
    );
    for signal in SIGNALS.iter() {
        unsafe {
            if let Err(res) = signal::sigaction(*signal, &sig_action) {
                println!("Error initializing signal handling: {:?}", res);
            }
        }
    }
}

pub extern fn handle_signal(_: i32) {}

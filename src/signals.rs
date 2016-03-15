//! Functions for signal handling within splash

use nix::sys::signal;
use std::sync::atomic::{AtomicIsize, Ordering, ATOMIC_ISIZE_INIT};

pub static LAST_SIGNAL: AtomicIsize = ATOMIC_ISIZE_INIT;

static SIGNALS: [i32; 2] = [
    signal::SIGINT,
    signal::SIGTSTP
];

/// Initialize all signal handlers
pub fn initialize_signals() {
    let sig_action = signal::SigAction::new(
        signal::SigHandler::Handler(handle_signal),
        signal::SaFlag::empty(),
        signal::SigSet::all());
    for signal in SIGNALS.iter() {
        unsafe {
            if let Err(res) = signal::sigaction(*signal, &sig_action) {
                println!("Error initializing signal handling: {:?}", res);
            }
        }
    }
}

/// Remove any signal handlers created by splash, setting them to SIG_DFL
pub fn cleanup_signals() {
    let sig_action = signal::SigAction::new(
        signal::SigHandler::SigDfl,
        signal::SaFlag::empty(),
        signal::SigSet::all());
    for signal in SIGNALS.iter() {
        unsafe {
            if let Err(res) = signal::sigaction(*signal, &sig_action) {
                println!("Error de-initializing signal handling: {:?}", res);
            }
        }
    }
}

pub extern "C" fn handle_signal(sig: i32) {
    LAST_SIGNAL.store(sig as isize, Ordering::Relaxed);
}

/// Get the value of the most recent signal.  This function resets the last
/// signal once it has been queried, so signals are lost once queried for.  A
/// value of 0 indicates no signal has been caught since the last query
pub fn get_last_signal() -> i32 {
    return LAST_SIGNAL.swap(0, Ordering::Relaxed) as i32;
}

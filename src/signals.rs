//! Functions for signal handling within splash
use std::sync::atomic::{AtomicIsize, Ordering, ATOMIC_ISIZE_INIT};

use nix::sys::signal;

use job;

pub static LAST_SIGNAL: AtomicIsize = ATOMIC_ISIZE_INIT;

const SIGNALS: [signal::Signal; 3] = [
    signal::SIGINT,
    signal::SIGQUIT,
    signal::SIGTSTP,
];

static IGNORE_SIGNALS: [signal::Signal; 2] = [
    signal::SIGTTIN,
    signal::SIGTTOU,
];

/// Initialize all signal handlers
pub fn initialize_signals() {
    let mut sig_action = signal::SigAction::new(
        signal::SigHandler::Handler(handle_signal),
        signal::SaFlags::empty(),
        signal::SigSet::all());
    for signal in SIGNALS.iter() {
        unsafe {
            if let Err(res) = signal::sigaction(*signal, &sig_action) {
                println!("Error initializing signal handling: {:?}", res);
            }
        }
    }

    sig_action = signal::SigAction::new(
        signal::SigHandler::SigIgn,
        signal::SaFlags::empty(),
        signal::SigSet::all());
    for signal in IGNORE_SIGNALS.iter() {
        unsafe {
            if let Err(res) = signal::sigaction(*signal, &sig_action) {
                println!("Error initializing signal handling: {:?}", res);
            }
        }
    }

    sig_action = signal::SigAction::new(
        signal::SigHandler::Handler(handle_sigchld),
        signal::SA_NOCLDSTOP | signal::SA_RESTART,
        signal::SigSet::all());
    unsafe {
        if let Err(res) = signal::sigaction(signal::SIGCHLD, &sig_action) {
            println!("Error initializing signal handling: {:?}", res);
        }
    }
}

/// Remove any signal handlers created by splash, setting them to SIG_DFL
pub fn cleanup_signals() {
    let sig_action = signal::SigAction::new(
        signal::SigHandler::SigDfl,
        signal::SaFlags::empty(),
        signal::SigSet::all());
    for signal in SIGNALS.iter().chain(IGNORE_SIGNALS.iter()) {
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

pub extern "C" fn handle_sigchld(_sig: i32) {
    let mut sigchldset = signal::SigSet::empty();
    sigchldset.add(signal::SIGCHLD);
    let _ = sigchldset.thread_block();

    job::update_jobs();

    let _ = sigchldset.thread_unblock();
}

/// Get the value of the most recent signal.  This function resets the last
/// signal once it has been queried, so signals are lost once queried for.  A
/// value of 0 indicates no signal has been caught since the last query
pub fn get_last_signal() -> i32 {
    return LAST_SIGNAL.swap(0, Ordering::Relaxed) as i32;
}

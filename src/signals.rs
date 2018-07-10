//! Functions for signal handling within splash
use std::sync::atomic::{AtomicIsize, Ordering, ATOMIC_ISIZE_INIT};

use nix::sys::signal::{self, SaFlags};

use job;

static LAST_SIGNAL: AtomicIsize = ATOMIC_ISIZE_INIT;

const SIGNALS: [signal::Signal; 3] = [signal::SIGINT, signal::SIGQUIT, signal::SIGTSTP];

static IGNORE_SIGNALS: [signal::Signal; 2] = [signal::SIGTTIN, signal::SIGTTOU];

/// Initialize all signal handlers
pub fn initialize_signals(interactive: bool) {
    let mut sig_action;
    if interactive {
        sig_action = signal::SigAction::new(
            signal::SigHandler::Handler(handle_signal),
            signal::SaFlags::empty(),
            signal::SigSet::all(),
        );
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
            signal::SigSet::all(),
        );
        for signal in IGNORE_SIGNALS.iter() {
            unsafe {
                if let Err(res) = signal::sigaction(*signal, &sig_action) {
                    println!("Error initializing signal handling: {:?}", res);
                }
            }
        }
    }

    sig_action = signal::SigAction::new(
        signal::SigHandler::Handler(handle_sigchld),
        SaFlags::SA_NOCLDSTOP | SaFlags::SA_RESTART,
        signal::SigSet::all(),
    );
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
        signal::SigSet::all(),
    );
    for signal in SIGNALS.iter().chain(IGNORE_SIGNALS.iter()) {
        unsafe {
            if let Err(res) = signal::sigaction(*signal, &sig_action) {
                println!("Error de-initializing signal handling: {:?}", res);
            }
        }
    }
}

pub struct SignalMutex(signal::Signal);

pub struct SignalGuard(signal::SigSet);

impl SignalGuard {
    pub fn new(signal: signal::Signal) -> Self {
        let mut sigset = signal::SigSet::empty();
        sigset.add(signal);
        let _ = sigset.thread_block();
        SignalGuard(sigset)
    }
}

impl Drop for SignalGuard {
    fn drop(&mut self) {
        let _ = self.0.thread_unblock();
    }
}

impl SignalMutex {
    pub fn new(signal: signal::Signal) -> Self {
        SignalMutex(signal)
    }

    pub fn lock(&self) -> SignalGuard {
        SignalGuard::new(self.0)
    }
}

pub extern "C" fn handle_signal(sig: i32) {
    LAST_SIGNAL.store(sig as isize, Ordering::Relaxed);
}

pub extern "C" fn handle_sigchld(_sig: i32) {
    let sig_mutex = SignalMutex::new(signal::SIGCHLD);
    let _ = sig_mutex.lock();

    job::update_jobs();
}

/// Get the value of the most recent signal.  This function resets the last
/// signal once it has been queried, so signals are lost once queried for.  A
/// value of 0 indicates no signal has been caught since the last query
pub fn get_last_signal() -> i32 {
    return LAST_SIGNAL.swap(0, Ordering::Relaxed) as i32;
}

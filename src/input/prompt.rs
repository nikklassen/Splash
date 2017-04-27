use std::env;
use std::ffi::{CString, CStr};
use std::io;
use std::os::raw::c_char;
use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, ATOMIC_BOOL_INIT, Ordering};

use libc::{self, STDIN_FILENO};
use nix::unistd::isatty;

use bindings::readline;
use signals;

static WAVE_EMOJI: &'static str = "\u{1F30A}";

pub fn getline(cont: bool) -> Option<String> {
    let mut line = String::new();
    loop {
        if let Some(s) = readline(if cont {
            "\\ ".to_string()
        } else {
            get_prompt_string()
        }) {
            if s.is_empty() {
                continue;
            }

            if !line.is_empty() {
                line.push_str("\n");
            }
            line.push_str(&s);
            break;
        } else {
            if !cont {
                info!("exit");
            }
            return None;
        }
    }
    Some(line)
}

pub fn readline<T>(prompt: T) -> Option<String>
where T: Into<String> {
    let res = isatty(STDIN_FILENO);
    // If stdin is closed
    if res.is_err() {
        return None;
    }
    if res.unwrap() {
        readline_raw(prompt)
    } else {
        let mut buf = String::new();
        match io::stdin().read_line(&mut buf) {
            // An error or ^D
            Err(_) | Ok(0) => {
                return None;
            }
            _ => {}
        }
        // Remove trailing newline
        buf.pop();
        Some(buf)
    }
}

static RUNNING: AtomicBool = ATOMIC_BOOL_INIT;
lazy_static! {
    static ref CURRENT_LINE: Mutex<Option<String>> = Mutex::new(Some(String::new()));
}

pub extern "C" fn readline_line_callback(line_raw: *const c_char) {
    *CURRENT_LINE.lock().unwrap() = if line_raw.is_null() {
        // ^D
        None
    } else {
        let line_string;
        unsafe {
            line_string = CStr::from_ptr(line_raw).to_string_lossy().into_owned();
        }
        if line_string == "exit" {
            None
        } else {
            if line_string.len() > 0 {
                unsafe {
                    readline::add_history(line_raw);
                }
            }
            Some(line_string)
        }
    };
    /* This function needs to be called to reset the terminal settings,
       and calling it from the line handler keeps one extra prompt from
       being displayed. */
    unsafe {
        readline::rl_callback_handler_remove();
    }
    RUNNING.store(false, Ordering::Relaxed)
}

fn readline_raw<T>(prompt: T) -> Option<String>
where T: Into<String> {
    use nix::sys::select;

    unsafe {
        let prompt = CString::new(prompt.into()).unwrap();
        readline::rl_callback_handler_install(prompt.as_ptr(), readline_line_callback);
        // Clear any pending writes, potentially due to a process interrupted by a signal
        libc::fflush(readline::rl_instream);
    }

    let mut fds = select::FdSet::new();
    RUNNING.store(true, Ordering::Relaxed);
    while RUNNING.load(Ordering::Relaxed) {
        fds.clear();

        unsafe {
            fds.insert(libc::fileno(readline::rl_instream));
        }

        let r = select::select(select::FD_SETSIZE, Some(&mut fds), None, None, None);
        if r.is_err() {
            if r != Err(::nix::Error::Sys(::nix::Errno::EINTR)) {
                unsafe {
                    readline::rl_callback_handler_remove();
                }

                // print a blank line to put the next prompt on the next line
                println!("");

                // Just return a blank line because this isn't the "exit" case that comes from ^D
                return Some("".to_string());
            }
            // ^C
            else if signals::get_last_signal() == signals::SIGINT as i32 {
                // print a blank line to put the next prompt on the next line
                println!("\r");

                // Just return a blank line because this isn't the "exit" case that comes from ^D
                return Some("".to_string());
            } else {
                unsafe {
                    readline::rl_forced_update_display();
                }
            }
        }

        unsafe {
            if fds.contains(libc::fileno(readline::rl_instream)) {
                readline::rl_callback_read_char();
            }
        }
    }
    (*CURRENT_LINE.lock().unwrap()).clone()
}

fn get_prompt_string() -> String {
    let pwd = env::var("PWD")
        .map(|v| {
            let home = env::var("HOME").unwrap_or(String::new());
            v.replace(&home, "~") + " "
        }).unwrap_or(String::new());
    format!("{}{}  ", pwd, WAVE_EMOJI)
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::path::PathBuf;
    use super::{get_prompt_string, WAVE_EMOJI};
    use test_fixture::*;

    struct PromptTests;

    impl TestFixture for PromptTests {
        fn tests(&self) -> TestList<Self> {
            vec![test!("prompt, unset pwd", prompt_unset_pwd),
            test!("prompt, includes pwd", prompt_includes_pwd),
            test!("prompt, includes home", prompt_includes_home),
            ]
        }
    }

    impl PromptTests {
        fn prompt_unset_pwd(&mut self) {
            let pwd = "my_dir";
            env::set_var("PWD", pwd);
            env::remove_var("PWD");
            let prompt = get_prompt_string();
            assert_eq!(prompt, WAVE_EMOJI.to_string() + "  ");
        }

        fn prompt_includes_pwd(&mut self) {
            let pwd = "my_dir";
            env::set_var("PWD", pwd);
            let prompt = get_prompt_string();
            assert!(prompt.starts_with(pwd));
        }

        fn prompt_includes_home(&mut self) {
            let home = "my_home";
            env::set_var("HOME", home);
            let dir = "my_dir";
            env::set_var("PWD", join(home, dir));
            let prompt = get_prompt_string();
            assert!(prompt.starts_with(
                    &join("~", dir)));
        }
    }

    fn join<'a>(p1: &str, p2: &str) -> String {
        let mut path: PathBuf = PathBuf::from(p1);
        path.push(p2);
        String::from(path.to_str().unwrap())
    }

    #[test]
    fn prompt_tests() {
        let fixture = PromptTests;
        test_fixture_runner(fixture);
    }
}

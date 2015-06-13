use std::env;
use std::io::prelude::*;
use std::io;
use std::path::{PathBuf, Component, Path};
use std::process;
use getopts::Options;
use std::collections::HashMap;

const SUCCESS: io::Result<i32> = Ok(0);

pub trait Builtin {
    fn run(&mut self, args: &[String]) -> io::Result<i32>;
}

pub type BuiltinMap = HashMap<String, Box<Builtin>>;

struct Cd {
    prev_dir: String,
}

impl Cd {
    fn new() -> Cd {
        let pwd = env::var("PWD").unwrap_or(String::new());
        Cd { prev_dir: pwd }
    }

    fn change_to<P: AsRef<Path>>(&mut self, p: &P) -> io::Result<()> {
        let pwd = env::var("PWD").unwrap_or(String::new());
        self.prev_dir = pwd;

        let new_pwd_buf = normalize_logical_path(&p);
        env::set_var("PWD", &new_pwd_buf);
        env::set_current_dir(&new_pwd_buf)
    }
}

fn normalize_logical_path<P: AsRef<Path>>(path: &P) -> PathBuf {
    let path = path.as_ref();
    let mut normalized_path = PathBuf::new();
    for c in path.components() {
        match c {
            Component::ParentDir => { normalized_path.pop(); },
            Component::CurDir => continue,
            _ => normalized_path.push(c.as_os_str()),
        };
    }
    normalized_path
}

impl Builtin for Cd {
    fn run(&mut self, args: &[String]) -> io::Result<i32> {
        if args.len() == 0 || args[0] == "~" {
            if let Ok(home) = env::var("HOME") {
                if home.len() != 0 {
                    return self.change_to(&PathBuf::from(&home))
                        .and(SUCCESS);
                }
            }
            return SUCCESS;
        }

        if args[0] == "-" {
            let prev_dir = self.prev_dir.clone();
            return self.change_to(&prev_dir).and(SUCCESS);
        }

        let cur_dir = env::current_dir();
        let mut pwd_buf = env::var("PWD")
            .map(|p| PathBuf::from(p))
            .or(cur_dir)
            .unwrap();
        pwd_buf.push(&args[0]);

        self.change_to(&pwd_buf).and(SUCCESS)
    }
}

struct Exit;
impl Builtin for Exit {
    fn run(&mut self, _args: &[String]) -> io::Result<i32> {
        process::exit(0)
    }
}

struct Pwd;
impl Builtin for Pwd {
    fn run(&mut self, _args: &[String]) -> io::Result<i32> {
        println!("{}", env::var("PWD").unwrap_or(String::new()));
        SUCCESS
    }
}

struct Echo;
impl Builtin for Echo {
    fn run(&mut self, args: &[String]) -> io::Result<i32> {
        let mut opts = Options::new();

        opts.optflag("n", "", "Suppress new lines");

        let matches = match opts.parse(args) {
            Ok(m) => m,
            Err(_) => { return Err(
                    io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "Unable to parse arguments.")) },
        };

        let remaining_args = matches.free.connect(" ");

        if matches.opt_present("n") {
            print!("{}", remaining_args);
            try!(io::stdout().flush());
        } else {
            println!("{}", remaining_args);
        }
        SUCCESS
    }
}

macro_rules! add_builtins {
    ($map:ident, [ $( ($n:expr, $cmd:expr) ),* ] ) => {{
        $($map.insert(
                String::from($n),
                Box::new($cmd) as Box<Builtin>
                );)*
    }}
}

pub fn init_builtins() -> BuiltinMap {
    let mut builtins = HashMap::new();
    add_builtins!(
        builtins,
        [
        ("cd", Cd::new()),
        ("echo", Echo),
        ("exit", Exit),
        ("pwd", Pwd)
        ]);
    builtins
}

#[cfg(test)]
mod tests {
    use std::env;
    use super::{Cd, Builtin};
    use std::sync::{StaticMutex, MUTEX_INIT};

    static LOCK: StaticMutex = MUTEX_INIT;

    #[test]
    fn cd_with_no_args() {
        let _g = LOCK.lock().unwrap();
        let home = String::from("home");
        env::set_var("HOME", &home);

        let mut cd = Cd::new();
        cd.run(&[]);

        assert_eq!(env::var("PWD"), Ok(home));
    }

    #[test]
    fn cd_with_tilde() {
        let _g = LOCK.lock().unwrap();
        let home = String::from("home");
        env::set_var("HOME", &home);

        let mut cd = Cd::new();
        cd.run(&["~".to_string()]);

        assert_eq!(env::var("PWD"), Ok(home));
    }

    #[test]
    fn cd_with_absolute_arg() {
        let _g = LOCK.lock().unwrap();
        let dir = String::from("/dir");

        let mut cd = Cd::new();
        cd.run(&[dir.clone()]);

        assert_eq!(env::var("PWD"), Ok(dir));
    }

    #[test]
    fn cd_with_relative_arg() {
        let _g = LOCK.lock().unwrap();
        let sub_dir = String::from("subdir");
        let new_dir = env::var("PWD").unwrap() + "/" + &sub_dir;

        let mut cd = Cd::new();
        cd.run(&[sub_dir.clone()]);

        assert_eq!(env::var("PWD"), Ok(new_dir));
    }

    #[test]
    fn cd_previous_directory() {
        let _g = LOCK.lock().unwrap();
        let pwd = env::var("PWD");
        let dir = String::from("/dirmod");
        let prev_dir = String::from("-");

        let mut cd = Cd::new();
        cd.run(&[dir]);
        cd.run(&[prev_dir]);

        assert_eq!(env::var("PWD"), pwd);
    }
}

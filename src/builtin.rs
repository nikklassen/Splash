use std::env;
use std::io::prelude::*;
use std::io;
use std::path::{PathBuf, Component, Path};
use std::process;
use getopts::Options;
use std::collections::HashMap;
use regex::Regex;

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
        try!(env::set_current_dir(&new_pwd_buf));
        env::set_var("PWD", &new_pwd_buf);
        Ok(())
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
        if args.len() == 0 {
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

        let re = Regex::new("^~($|/)(.*)").unwrap();
        if let Some(caps) = re.captures(&args[0]) {
            if caps.len() != 0 {
                if let Ok(home) = env::var("HOME") {
                    if home.len() != 0 {
                        pwd_buf.push(home);
                        pwd_buf.push(caps.at(2).unwrap());
                    }
                }
            }
        } else {
            pwd_buf.push(&args[0]);
        }
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
pub mod tests {
    use std::{env, fs};
    use std::path::PathBuf;
    use super::{Cd, Builtin};
    use tests;
    use tests::{TestFixture, TestList, test_fixture_runner};

    struct BuiltinTests {
        pwd: PathBuf
    }

    impl TestFixture for BuiltinTests {
        fn setup(&mut self) {
            let mut pwd = env::temp_dir();
            pwd.push("pwd");

            fs::create_dir(&pwd);
            self.pwd = pwd;
            env::set_current_dir(&self.pwd).unwrap();
        }

        fn teardown(&mut self) {
            fs::remove_dir(&self.pwd);
        }

        fn tests(&mut self) -> TestList<BuiltinTests> {
            vec![test!("cd, no args", cd_with_no_args),
            test!("cd, absolute arg", cd_with_absolute_arg),
            test!("cd, relative arg", cd_with_relative_arg),
            test!("cd, previous dir", cd_previous_directory),
            ]
        }
    }

    impl BuiltinTests {
        fn new() -> BuiltinTests {
            BuiltinTests {
                pwd: PathBuf::new()
            }
        }

        fn cd_with_no_args(&mut self) {
            let home = String::from("/");
            env::set_var("HOME", &home);

            let mut cd = Cd::new();
            cd.run(&[]).unwrap();

            assert_eq!(env::var("PWD"), Ok(home));
        }

        fn cd_with_absolute_arg(&mut self) {
            let dir = String::from("/");
            env::set_var("PWD", &self.pwd);

            let mut cd = Cd::new();
            cd.run(&[dir.clone()]).unwrap();

            assert_eq!(env::var("PWD"), Ok(dir));
        }

        fn cd_with_relative_arg(&mut self) {
            let mut pwd = self.pwd.clone();
            pwd.pop();
            env::set_var("PWD", &pwd);
            env::set_current_dir("..").unwrap();

            let mut cd = Cd::new();
            cd.run(&[String::from("pwd")]).unwrap();

            assert_eq!(env::var("PWD"), Ok(pathbuf_to_string(&self.pwd)));
        }

        fn cd_previous_directory(&mut self) {
            let mut cd = Cd::new();
            cd.run(&[String::from("..")]).unwrap();
            cd.run(&[String::from("-")]).unwrap();

            assert_eq!(env::var("PWD"), Ok(pathbuf_to_string(&self.pwd)));
        }
    }

    fn pathbuf_to_string(p: &PathBuf) -> String {
        String::from((*p).to_str().unwrap())
    }

    pub fn cd_tests() {
        let builtin_tests = BuiltinTests::new();
        test_fixture_runner(builtin_tests);
    }
}

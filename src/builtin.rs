use env::UserEnv;
use getopts::Options;
use std::collections::HashMap;
use std::env;
use std::io;
use std::io::prelude::*;
use std::path::{Component, Path, PathBuf};

use job;

const SUCCESS: io::Result<i32> = Ok(0);

pub trait Builtin {
    fn run(&mut self, args: &[String], env: &mut UserEnv) -> io::Result<i32>;
    fn dup(&self) -> Box<Builtin>;
}

pub struct SimpleBuiltin(fn(&[String], &mut UserEnv) -> io::Result<i32>);

impl Clone for SimpleBuiltin {
    fn clone(&self) -> Self {
        SimpleBuiltin(self.0)
    }
}

impl Builtin for SimpleBuiltin where {
    fn run(&mut self, args: &[String], env: &mut UserEnv) -> io::Result<i32> {
        self.0(args, env)
    }

    fn dup(&self) -> Box<Builtin> {
        Box::new(self.clone())
    }
}

pub type BuiltinMap = HashMap<&'static str, Box<Builtin>>;

#[derive(Clone)]
struct Cd {
    prev_dir: String,
}

impl Cd {
    fn new() -> Cd {
        let pwd = env::var("PWD").unwrap_or(String::new());
        Cd { prev_dir: pwd }
    }

    fn change_to<P: AsRef<Path>>(&mut self, p: &P, env: &mut UserEnv) -> io::Result<()> {
        let pwd = env.get("PWD");
        self.prev_dir = pwd;

        let new_pwd_buf = normalize_logical_path(&p);
        env::set_current_dir(&new_pwd_buf)?;
        let path_str = new_pwd_buf.to_str().ok_or(io::Error::new(
            io::ErrorKind::Other,
            "Invalid characters in path",
        ))?;
        env.set("PWD", path_str);
        Ok(())
    }
}

fn normalize_logical_path<P: AsRef<Path>>(path: &P) -> PathBuf {
    let path = path.as_ref();
    let mut normalized_path = PathBuf::new();
    for c in path.components() {
        match c {
            Component::ParentDir => {
                normalized_path.pop();
            }
            Component::CurDir => continue,
            _ => normalized_path.push(c.as_os_str()),
        };
    }
    normalized_path
}

impl Builtin for Cd {
    fn run(&mut self, args: &[String], env: &mut UserEnv) -> io::Result<i32> {
        if args.len() == 0 {
            let home = env.get("HOME");
            if home.len() != 0 {
                return self.change_to(&PathBuf::from(&home), env).and(SUCCESS);
            }
            return SUCCESS;
        }

        if args[0] == "-" {
            let prev_dir = self.prev_dir.clone();
            return self.change_to(&prev_dir, env).and(SUCCESS);
        }

        let pwd = env.get("PWD");
        let mut pwd_buf = if pwd == "" {
            env::current_dir()?
        } else {
            PathBuf::from(pwd)
        };
        pwd_buf.push(&args[0]);

        self.change_to(&pwd_buf, env).and(SUCCESS)
    }

    fn dup(&self) -> Box<Builtin> {
        Box::new(self.clone())
    }
}

fn pwd(_args: &[String], env: &mut UserEnv) -> io::Result<i32> {
    println!("{}", env.get("PWD"));
    SUCCESS
}

fn echo(args: &[String], _env: &mut UserEnv) -> io::Result<i32> {
    let mut opts = Options::new();

    opts.optflag("n", "", "Suppress new lines");

    let matches = match opts.parse(args) {
        Ok(m) => m,
        Err(_) => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Unable to parse arguments.",
            ))
        }
    };

    let remaining_args = matches.free.join(" ");

    if matches.opt_present("n") {
        print!("{}", remaining_args);
        try!(io::stdout().flush());
    } else {
        println!("{}", remaining_args);
    }
    SUCCESS
}

fn fg(_args: &[String], _env: &mut UserEnv) -> io::Result<i32> {
    let res = job::start_job(true)?;
    Ok(res)
}

fn bg(_args: &[String], _env: &mut UserEnv) -> io::Result<i32> {
    let res = job::start_job(false)?;
    Ok(res)
}

fn jobs(_args: &[String], _env: &mut UserEnv) -> io::Result<i32> {
    job::print_jobs();
    Ok(0)
}

macro_rules! add_builtin_fns {
    ($map:ident, [ $( ($n:expr, $cmd:expr) ),* ] ) => {{
        $($map.insert(
                $n,
                Box::new(SimpleBuiltin($cmd)) as Box<Builtin>
                );)*
    }}
}

fn builtin_true(_args: &[String], _env: &mut UserEnv) -> io::Result<i32> {
    SUCCESS
}

pub fn init_builtins() -> BuiltinMap {
    let mut builtins: BuiltinMap = HashMap::new();
    builtins.insert("cd", Box::new(Cd::new()));
    add_builtin_fns!(
        builtins,
        [
            ("echo", echo),
            ("pwd", pwd),
            ("fg", fg),
            ("bg", bg),
            ("jobs", jobs),
            ("true", builtin_true),
            ("false", |_args: &[String], _env: &mut UserEnv| Ok(1)),
            (":", builtin_true)
        ]
    );
    builtins
}

pub fn clone_builtins(builtins: &BuiltinMap) -> BuiltinMap {
    let mut builtins_clone: BuiltinMap = HashMap::new();
    for (cmd, func) in builtins.iter() {
        builtins_clone.insert(cmd, func.dup());
    }
    builtins_clone
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;
    use std::{env, fs};
    use test_fixture::*;

    struct BuiltinTests {
        pwd: PathBuf,
    }

    impl TestFixture for BuiltinTests {
        fn setup(&mut self) {
            let mut pwd = env::temp_dir();
            pwd.push("pwd");

            fs::create_dir(&pwd).unwrap();
            self.pwd = pwd;
            env::set_current_dir(&self.pwd).unwrap();
            env::set_var("PWD", &self.pwd);
        }

        fn teardown(&mut self) {
            fs::remove_dir(&self.pwd).unwrap();
        }

        fn tests(&self) -> TestList<Self> {
            vec![
                test!("cd, no args", cd_with_no_args),
                test!("cd, absolute arg", cd_with_absolute_arg),
                test!("cd, relative arg", cd_with_relative_arg),
                test!("cd, previous dir", cd_previous_directory),
            ]
        }
    }

    impl BuiltinTests {
        fn new() -> BuiltinTests {
            BuiltinTests {
                pwd: PathBuf::new(),
            }
        }

        fn cd_with_no_args(&mut self) {
            let home = String::from("/");
            let mut user_env = UserEnv::new();
            user_env.set("HOME", &home);

            let mut cd = Cd::new();
            cd.run(&[], &mut user_env).unwrap();

            assert_eq!(user_env.get("PWD"), home);
        }

        fn cd_with_absolute_arg(&mut self) {
            let dir = String::from("/");
            let mut user_env = UserEnv::new();
            user_env.set("PWD", &pathbuf_to_string(&self.pwd));

            let mut cd = Cd::new();
            cd.run(&[dir.clone()], &mut user_env).unwrap();

            assert_eq!(user_env.get("PWD"), dir);
        }

        fn cd_with_relative_arg(&mut self) {
            let mut pwd = self.pwd.clone();
            pwd.pop();
            let mut user_env = UserEnv::new();
            user_env.set("PWD", &pathbuf_to_string(&pwd));

            env::set_current_dir("..").unwrap();

            let mut cd = Cd::new();
            cd.run(&[String::from("pwd")], &mut user_env).unwrap();

            assert_eq!(env::var("PWD"), Ok(pathbuf_to_string(&self.pwd)));
        }

        fn cd_previous_directory(&mut self) {
            let mut user_env = UserEnv::new();
            let mut cd = Cd::new();
            cd.run(&[String::from("..")], &mut user_env).unwrap();
            cd.run(&[String::from("-")], &mut user_env).unwrap();

            assert_eq!(user_env.get("PWD"), pathbuf_to_string(&self.pwd));
        }
    }

    fn pathbuf_to_string(p: &PathBuf) -> String {
        String::from((*p).to_str().unwrap())
    }

    #[test]
    fn builtin_tests() {
        let fixture = BuiltinTests::new();
        test_fixture_runner(fixture);
    }
}

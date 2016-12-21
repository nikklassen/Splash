use std::collections::HashMap;
use process::{Builtin, BuiltinMap};
use job::{SharedJobTable, JobTable};
use getopts::Options;
use std::env;
use std::io::prelude::*;
use std::io;
use std::path::{PathBuf, Component, Path};

const SUCCESS: io::Result<i32> = Ok(0);

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
        pwd_buf.push(&args[0]);

        self.change_to(&pwd_buf).and(SUCCESS)
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

        let remaining_args = matches.free.join(" ");

        if matches.opt_present("n") {
            print!("{}", remaining_args);
            try!(io::stdout().flush());
        } else {
            println!("{}", remaining_args);
        }
        SUCCESS
    }
}

struct Fg {
    jobs: SharedJobTable,
}
impl Fg {
    pub fn new(jobs: SharedJobTable) -> Self {
        Fg {
            jobs: jobs,
        }
    }
}

fn start_job(jobs: &mut JobTable, job_id: Option<i32>, foreground: bool) -> io::Result<i32> {
    let mut job_entry = if let Some(id) = job_id {
        jobs.get_job_mut(id)
    } else {
        jobs.last_job_mut()
    };
    if let Some(ref mut job) = job_entry {
        info!("[{}]  {} continued\t{}", job.id,
              if foreground { "-" } else { "+" }, job.cmd);
        let res = if foreground {
            job.foreground()
        } else {
            job.background()
        };
        if let Err(e) = res {
            error!("splash: {}", e);
            Ok(2)
        } else {
            Ok(0)
        }
    } else {
        warn!("{}: no current job",
              if foreground { "fg" } else { "bg" });
        Ok(1)
    }
}

impl Builtin for Fg {
    fn run(&mut self, _args: &[String]) -> io::Result<i32> {
        start_job(&mut self.jobs.get_inner(), None, true)
    }
}

struct Bg {
    jobs: SharedJobTable,
}
impl Bg {
    pub fn new(jobs: SharedJobTable) -> Self {
        Bg {
            jobs: jobs,
        }
    }
}

impl Builtin for Bg {
    fn run(&mut self, _args: &[String]) -> io::Result<i32> {
        start_job(&mut self.jobs.get_inner(), None, false)
    }
}

struct Jobs {
    jobs: SharedJobTable,
}
impl Jobs {
    pub fn new(jobs: SharedJobTable) -> Self {
        Jobs {
            jobs: jobs,
        }
    }
}

impl Builtin for Jobs {
    fn run(&mut self, _args: &[String]) -> io::Result<i32> {
        self.jobs.get_inner().print_jobs();
        Ok(0)
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

pub fn init_builtins(jobs: SharedJobTable) -> BuiltinMap {
    let mut builtins = HashMap::new();
    add_builtins!(
        builtins,
        [
        ("cd", Cd::new()),
        ("echo", Echo),
        ("pwd", Pwd),
        ("fg", Fg::new(jobs.clone())),
        ("bg", Bg::new(jobs.clone())),
        ("jobs", Jobs::new(jobs.clone()))
        ]);
    builtins
}

#[cfg(test)]
mod tests {
    use std::{env, fs};
    use std::path::PathBuf;
    use super::Cd;
    use process::Builtin;
    use test_fixture::*;

    struct BuiltinTests {
        pwd: PathBuf
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

    #[test]
    fn builtin_tests() {
        let fixture = BuiltinTests::new();
        test_fixture_runner(fixture);
    }
}

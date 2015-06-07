use std::env;
use std::io;
use std::path::{PathBuf, Component};
use std::process;

static BUILTINS : [Builtin; 3]= [
    Builtin { name: "cd", func: cd },
    Builtin { name: "exit", func: exit },
    Builtin { name: "pwd", func: pwd },
];

const SUCCESS: io::Result<i32> = Ok(0);

pub fn is_builtin(cmd : &str) -> bool {
    get_builtin(cmd).is_some()
}

fn get_builtin(cmd : &str) -> Option<&Builtin> {
    BUILTINS.iter().find(|b| {
        b.name == cmd
    })
}

fn normalize_logical_path(path: &PathBuf) -> PathBuf {
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

pub fn cd(args : &[&str]) -> io::Result<i32> {
    fn change_to(p: &PathBuf) -> io::Result<()> {
        let new_pwd_buf = normalize_logical_path(p);
        env::set_var("PWD", &new_pwd_buf);
        env::set_current_dir(new_pwd_buf)
    }

    if args.len() == 0 {
        if let Ok(home) = env::var("HOME") {
            if home.len() != 0 {
                return change_to(&PathBuf::from(&home))
                    .and(SUCCESS);
            }
        }
        return SUCCESS;
    }

    let cur_dir = env::current_dir();
    let mut pwd_buf = env::var("PWD")
        .map(|p| PathBuf::from(p))
        .or(cur_dir)
        .unwrap();
    pwd_buf.push(args[0]);

    change_to(&pwd_buf).and(SUCCESS)
}

pub fn exit(_args : &[&str]) -> io::Result<i32> {
    process::exit(0)
}

pub fn pwd(_args : &[&str]) -> io::Result<i32> {
    println!("{}", env::var("PWD").unwrap_or(String::new()));
    SUCCESS
}

pub fn exec_builtin(args : &[&str]) -> io::Result<i32> {
    if let Some(b) = get_builtin(args[0]) {
        let f = b.func;
        f(&args[1..])
    } else {
        Err(
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("Builtin \"{}\" not found", args[0])))
    }
}

struct Builtin {
    name : &'static str,
    func : fn(&[&str]) -> io::Result<i32>,
}

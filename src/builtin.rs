use std::env;
use std::io;
use std::path::Path;
use std::process;

static BUILTINS : [Builtin; 2] = [
    Builtin { name: "cd", func: cd },
    Builtin { name: "exit", func: exit },
];

pub fn is_builtin(cmd : &str) -> bool {
    get_builtin(cmd).is_some()
}

fn get_builtin(cmd : &str) -> Option<&Builtin> {
    BUILTINS.iter().find(|b| {
        b.name == cmd
    })
}

pub fn cd(args : &[&str]) -> io::Result<i32> {
    let path = Path::new(args[0]);
    env::set_current_dir(path).and(Ok(0))
}

pub fn exit(_args : &[&str]) -> io::Result<i32> {
    process::exit(0)
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

use readline::readline;
use std::process::Command;
use std::io;
use builtin::{is_builtin, exec_builtin};

pub fn input_loop() {
    loop {
        let input = readline("\u{1F30A}   ");
        let line = match input {
            Ok(l) => l,
            Err(_) => continue,
        };
        // ^D
        if line.len() == 0 {
            return
        }

        let tokens : Vec<_> = line.split_whitespace().collect(); 
        if tokens.len() == 0 {
            continue;
        }

        if let Err(e) = execute(tokens) {
            println!("{}", e);
        }
    }
}

fn execute(args : Vec<&str>) -> io::Result<i32> {
    if is_builtin(args[0]) {
        return exec_builtin(&args)
    }

    let mut child = try!(Command::new(&args[0])
        .args(&args[1..])
        .spawn());

    child.wait()
        .and_then(|result| {
            Ok(result.code().unwrap_or(0))
        })
}

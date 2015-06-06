use std::io;
use std::io::Write;

pub fn readline(prompt : &str) -> io::Result<String> {
    let mut stdout = io::stdout();
    try!(stdout.write(prompt.as_bytes()));
    try!(stdout.flush());

    let mut s = String::new();
    try!(io::stdin().read_line(&mut s));
    Ok(s)
}

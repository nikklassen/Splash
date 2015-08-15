use std::io::{self, Write};

#[export_macro]
macro_rules! is_match {
    ($e: expr, $p: pat) => ((
        if let $p = $e { true } else { false }
    ))
}

#[inline]
pub fn write_err(s: String) {
    let stderr = io::stderr();
    let res = writeln!(stderr.lock(), "{}", s);
    res.unwrap();
}

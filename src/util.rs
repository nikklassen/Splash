use std::io::{self, Write};

#[export_macro]
macro_rules! is_match {
    ($e: expr, $p: pat) => ((
        if let $p = $e { true } else { false }
    ))
}

// from http://stackoverflow.com/a/27582993
#[export_macro]
macro_rules! hash_map(
    { $($key:expr => $value:expr,)+ } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key, $value);
            )+
            m
        }
     };
);

#[inline]
pub fn write_err(s: String) {
    let stderr = io::stderr();
    let res = writeln!(stderr.lock(), "{}", s);
    res.unwrap();
}

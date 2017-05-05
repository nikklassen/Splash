use std::fmt::Debug;

#[macro_export]
macro_rules! is_match {
    ($e: expr, $p: pat) => (
        if let $p = $e { true } else { false }
    )
}

// from http://stackoverflow.com/a/27582993
#[macro_export]
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

#[macro_export]
macro_rules! print_err {
    ($fmt:expr) => {{
        use std::io::{self, Write};
        let stderr = io::stderr();
        let exe = ::std::env::args().next().unwrap_or("splash".to_owned());
        let res = writeln!(stderr.lock(), concat!("{}: ", $fmt), exe);
        res.unwrap();
    }};
    ($fmt:expr, $($arg:tt)*) => {{
        use std::io::{self, Write};
        let stderr = io::stderr();
        let exe = ::std::env::args().next().unwrap_or("splash".to_owned());
        let res = writeln!(stderr.lock(), concat!("{}: ", $fmt), exe, $($arg)*);
        res.unwrap();
    }};
}

pub fn show_err<S, T: Debug>(e: T) -> Result<S, String> {
    Err(format!("{:?}", e))
}

pub fn join_str(vs: &Vec<String>, sep: &str) -> String {
    let mut s = String::new();
    for i in 0..vs.len()-1 {
        s.push_str(&vs[i]);
        s.push_str(sep);
    }
    vs.last().map(|v| {
        s.push_str(&v)
    });
    s
}

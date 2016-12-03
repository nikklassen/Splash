use std::io::{self, Write};
use std::fmt::{Display, Debug};
use std::result;

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

pub fn sequence<T, E>(v: Vec<Result<T, E>>) -> result::Result<Vec<T>, E>
where T: Debug, E: Debug + Clone {
    for res in v.iter() {
        if let &Err(ref e) = res {
            return Err(e.clone());
        }
    }
    Ok(v.into_iter().map(|i| i.unwrap()).collect())
}

#[inline]
pub fn write_err<D: Display>(s: &D) {
    let stderr = io::stderr();
    let res = writeln!(stderr.lock(), "{}", s);
    res.unwrap();
}

pub fn show_err<S, T: Debug>(e: T) -> Result<S, String> {
    Err(format!("{:?}", e))
}

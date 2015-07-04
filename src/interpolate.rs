use tokenizer::AST;
use std::env;

fn expand_arg(mut acc: String, arg: &AST) -> String {
    match arg {
        &AST::Var(ref v) => {
            let s = env::var(v)
                .unwrap_or("".to_string());
            acc.push_str(&s);
        },
        &AST::String(ref s) => {
            acc.push_str(&s);
        },
        &AST::Quoted(ref ts) => {
            let res = expand(&ts);
            acc.push_str(&res);
        },
        &AST::Whitespace => {},
    };
    acc
}

pub fn expand(items: &Vec<AST>) -> String {
    items.iter().fold(String::new(), expand_arg)
}

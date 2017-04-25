use env::UserEnv;
use std::env;
use input::token::{self, Token};
use input::ast::*;
use process::Process;
use util;

fn expand_token(t: &Token, user_env: &UserEnv) -> Token {
    match t {
        &Token::Var(ref v) => {
            let s = user_env.vars
            .get(v)
            .map(|value| value.clone())
            .unwrap_or_else(|| {
                println!("var {} not found", v);
                env::var(v)
                    .unwrap_or(String::new())
            });
            Token::String(s)
        },
        &Token::Quoted(ref ts) => Token::Quoted(
            ts.iter().map(|t| expand_token(t, user_env)).collect()),
        _ => t.clone(),
    }
}

fn expand_word(word: &Word, user_env: &UserEnv) -> Word {
    let expanded_tokens = word.0.iter()
        .map(|part| {
            let new_token = expand_token(&part, user_env);
            token::to_value(&new_token).unwrap_or(String::new())
        })
        .collect();

    let val = util::join_str(&expanded_tokens, "");
    Word(vec![Token::String(val)])
}

pub fn expand(p: &mut Process, user_env: &UserEnv) {
    p.prog = p.prog.as_ref().map(|prog| expand_word(prog, user_env));
    p.args = p.args.iter().map(|arg| expand_word(arg, user_env)).collect();
}

#[cfg(test)]
mod tests {
    use env::UserEnv;
    use std::env;
    use super::*;
    use input::token::Token;

    fn make_test_env() -> UserEnv {
        let mut user_env = UserEnv::new();
        user_env.vars.insert("TEST".to_string(), "value".to_string());
        user_env
    }

    #[test]
    fn expand_env_var() {
        let user_env = make_test_env();
        env::set_var("test_var", "value");
        let toks = vec![Token::Var("test_var".to_string())];
        let word = expand_word(&Word(toks), &user_env);
        assert_eq!(word, Word(vec![Token::String("value".to_string())]));
    }

    #[test]
    fn expand_user_var() {
        let user_env = make_test_env();
        let toks = vec![Token::Var("TEST".to_string())];
        let word = expand_word(&Word(toks), &user_env);
        assert_eq!(word, Word(vec![Token::String("value".to_string())]));
    }

    #[test]
    fn expand_quoted() {
        let user_env = make_test_env();
        let toks = vec![Token::Quoted(
            vec![Token::Var("TEST".to_string())])];
        let word = expand_word(&Word(toks), &user_env);
        assert_eq!(word, Word(vec![Token::String("value".to_string())]));
    }
}

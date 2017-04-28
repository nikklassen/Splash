use std::env;
use std::ffi::{CString, CStr};
use libc;

use env::UserEnv;
use input::ast::*;
use input::token::{self, Token};
use process::Process;
use util;

fn expand_token(t: &Token, user_env: &UserEnv) -> Token {
    match t {
        &Token::Var(ref v) => {
            let s = user_env.vars
            .get(v)
            .map(|value| value.clone())
            .unwrap_or_else(|| {
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

fn get_user_home(name: &str) -> Option<String> {
    let c_name;
    match CString::new(name) {
        Ok(s) => { c_name = s },
        Err(_) => return None,
    }
    let passwd = unsafe { libc::getpwnam(c_name.as_ptr()) };
    if passwd.is_null() {
        None
    } else {
        let home = unsafe {
            CStr::from_ptr((*passwd).pw_dir)
                .to_string_lossy()
                .into_owned()
        };
        Some(home)
    }
}

fn tilde_expansion(s: &str, is_assignment: bool) -> Result<String, String> {
    let get_name = |part: &str| -> String {
        part.chars().take_while(|&c| c != '/' && (!is_assignment || c != ':')).collect()
    };
    fn get_home(user: &str) -> Result<String, String> {
        if user.len() > 0 {
            get_user_home(&user)
                .ok_or(format!("Could not find user {}", &user))
        } else {
            Ok(env::var("HOME").unwrap_or(String::new()))
        }
    }

    if is_assignment {
        let mut expanded = String::new();
        let mut acc = 0;
        loop {
            let m = s[acc..].find(":");
            if let Some(i) = m {
                // End of s searched so far
                let mut end = acc + i + 1;

                // Include the : in the s pushed
                expanded.push_str(&s[acc..end]);
                if end < s.len() && &s[end..end+1] == "~" {
                    let name = get_name(&s[end+1..]);
                    let home = get_home(&name)?;
                    expanded.push_str(&home);

                    end += name.len() + 1;
                }
                acc = end;
            } else {
                expanded.push_str(&s[acc..]);
                break;
            }
        }
        Ok(expanded)
    } else if s.starts_with("~") {
        let user = get_name(&s[1..]);
        let home = get_home(&user)?;
        Ok(home + &s[user.len()+1..])
    } else {
        Ok(s.to_owned())
    }
}

fn expand_word(word: &Word, user_env: &UserEnv, is_assignment: bool) -> Result<Word, String> {
    let mut parts = word.0.clone();
    if !is_assignment {
        if let Some(&mut Token::String(ref mut s)) = parts.first_mut() {
            *s = tilde_expansion(s, false)?;
        }
    }
    let expanded_tokens = util::sequence(parts.iter_mut()
        .map(|part| {
            if is_assignment {
                if let &mut Token::String(ref mut s) = part {
                    *s = tilde_expansion(s, is_assignment)?
                }
            }
            let new_token = expand_token(&part, user_env);
            let expanded = token::to_value(&new_token).unwrap_or(String::new());
            Ok(expanded)
        })
        .collect::<Vec<Result<String, String>>>())?;

    let val = util::join_str(&expanded_tokens, "");
    Ok(Word(vec![Token::String(val)]))
}

pub fn expand(p: &mut Process, user_env: &UserEnv) -> Result<(), String> {
    if p.prog.is_some() {
        let expanded = expand_word(p.prog.as_ref().unwrap(), user_env, false)?;
        p.prog = Some(expanded);
    }
    p.args = util::sequence(p.args.iter().map(|arg| expand_word(arg, user_env, false)).collect())?;
    for ref mut prefix in p.env.iter_mut() {
        if let &mut CmdPrefix::Assignment { ref mut rhs, .. } = *prefix {
            *rhs = expand_word(rhs, user_env, true)?;
        }
    }
    Ok(())
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
        let toks = vec![Token::Var("HOME".to_string())];
        let word = expand_word(&Word(toks), &user_env, false).unwrap();

        let home = env::var("HOME").unwrap_or(String::new());
        assert_eq!(word, Word(vec![Token::String(home)]));
    }

    #[test]
    fn expand_user_var() {
        let user_env = make_test_env();
        let toks = vec![Token::Var("TEST".to_string())];
        let word = expand_word(&Word(toks), &user_env, false).unwrap();
        assert_eq!(word, Word(vec![Token::String("value".to_string())]));
    }

    #[test]
    fn expand_quoted() {
        let user_env = make_test_env();
        let toks = vec![Token::Quoted(
            vec![Token::Var("TEST".to_string())])];
        let word = expand_word(&Word(toks), &user_env, false).unwrap();
        assert_eq!(word, Word(vec![Token::String("value".to_string())]));
    }

    #[test]
    fn expand_tilde() {
        let user_env = make_test_env();
        let toks = vec![Token::String("~".to_string())];
        let word = expand_word(&Word(toks), &user_env, false).unwrap();
        let home = env::var("HOME").unwrap();
        assert_eq!(word, Word(vec![Token::String(home)]));
    }

    #[test]
    fn expand_tilde_with_slash() {
        let user_env = make_test_env();
        let toks = vec![Token::String("~/.config".to_string())];
        let word = expand_word(&Word(toks), &user_env, false).unwrap();
        let home = env::var("HOME").unwrap();
        assert_eq!(word, Word(vec![Token::String(home + "/.config")]));
    }

    #[test]
    fn expand_tilde_user() {
        let user_env = make_test_env();
        let toks = vec![Token::String("~root/.config".to_string())];
        let word = expand_word(&Word(toks), &user_env, false).unwrap();
        assert_eq!(word, Word(vec![Token::String("/root/.config".to_string())]));
    }

    #[test]
    fn expand_tilde_err_no_user() {
        let user_env = make_test_env();
        let toks = vec![Token::String("~unknown/.config".to_string())];
        let word = expand_word(&Word(toks), &user_env, false);
        assert!(word.is_err());
    }

    #[test]
    fn expand_tilde_after_semi() {
        let user_env = make_test_env();
        let toks = vec![Token::String("a:~".to_string())];
        let word = expand_word(&Word(toks), &user_env, true).unwrap();

        let home = env::var("HOME").unwrap();
        assert_eq!(word, Word(vec![Token::String("a:".to_string() + &home)]));
    }

    #[test]
    fn expand_tilde_after_semi_with_name() {
        let user_env = make_test_env();
        let toks = vec![Token::String("a:~root:b".to_string())];
        let word = expand_word(&Word(toks), &user_env, true).unwrap();

        assert_eq!(word, Word(vec![Token::String("a:/root:b".to_string())]));
    }
}

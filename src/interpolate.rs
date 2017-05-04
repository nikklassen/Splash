use std::iter;
use std::env;
use std::ffi::{CString, CStr};
use libc;

use env::UserEnv;
use input::ast::*;
use process::Process;
use util;

fn parameter_expansion(t: &String, user_env: &UserEnv) -> String {
    let mut s = String::new();
    let mut chars = t.chars();
    let mut is_lit = false;
    loop {
        let mut next = chars.next();
        match next {
            Some('$') => {
                if is_lit {
                    s.push('$');
                    continue;
                }
                // fall-through to variable evaluation
            },
            Some(c) => {
                if c == '\'' {
                    is_lit = !is_lit;
                }
                s.push(c);
                continue;
            },
            None => {
                break;
            },
        }

        next = chars.next();
        match next {
            Some('{') => {
                // TODO escaping of delimeter
                let param_name: String = chars.by_ref().take_while(|c| *c != '}').collect();
                chars.next();
                let param = user_env.vars
                    .get(&param_name)
                    .map(|value| value.clone())
                    .unwrap_or_else(|| {
                        env::var(&param_name)
                            .unwrap_or(String::new())
                    });
                s.push_str(&param);
            },
            Some('(') => {
                // TODO arithmetic
                let command: String = chars.by_ref().take_while(|c| *c != ')').collect();
                chars.next();
                s.push_str(&command);
            },
            Some(c) => {
                let param_name: String = iter::once(c).chain(chars.by_ref().take_while(|c| c.is_alphanumeric())).collect();
                let param = user_env.vars
                    .get(&param_name)
                    .map(|value| value.clone())
                    .unwrap_or_else(|| {
                        env::var(&param_name)
                            .unwrap_or(String::new())
                    });
                s.push_str(&param);
            },
            None => {
                break;
            },
        }
    }
    s
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

fn quote_removal(word: &String) -> String {
    word.chars()
        .filter(|c| !(*c == '\'' || *c == '\"'))
        .collect()
}

fn expand_word(word: &String, user_env: &UserEnv, is_assignment: bool) -> Result<String, String> {
    // TODO no path name expansion in double quotes
    let mut s = tilde_expansion(&word, is_assignment)?;
    s = parameter_expansion(&s, user_env);
    s = quote_removal(&s);
    Ok(s)
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

    fn make_test_env() -> UserEnv {
        let mut user_env = UserEnv::new();
        user_env.vars.insert("TEST".to_string(), "value".to_string());
        user_env
    }

    #[test]
    fn no_expansions() {
        let user_env = make_test_env();
        let toks = "hello".to_string();
        let word = expand_word(&toks, &user_env, false).unwrap();

        assert_eq!(word, toks);
    }

    #[test]
    fn expand_env_var() {
        let user_env = make_test_env();
        let toks = "$HOME".to_string();
        let word = expand_word(&toks, &user_env, false).unwrap();

        let home = env::var("HOME").unwrap_or(String::new());
        assert_eq!(word, home);
    }

    #[test]
    fn expand_user_var() {
        let user_env = make_test_env();
        let toks = "$TEST".to_string();
        let word = expand_word(&toks, &user_env, false).unwrap();
        assert_eq!(word, "value".to_string());
    }

    #[test]
    fn expand_quoted() {
        let user_env = make_test_env();
        let toks = r#""$TEST""#.to_string();
        let word = expand_word(&toks, &user_env, false).unwrap();
        assert_eq!(word, "value".to_string());
    }

    #[test]
    fn no_expand_var_in_single_quotes() {
        let user_env = make_test_env();
        let toks = "'$TEST'".to_string();
        let word = expand_word(&toks, &user_env, true).unwrap();

        assert_eq!(word, "$TEST".to_string());
    }

    #[test]
    fn expand_tilde() {
        let user_env = make_test_env();
        let toks = "~".to_string();
        let word = expand_word(&toks, &user_env, false).unwrap();
        let home = env::var("HOME").unwrap();
        assert_eq!(word, home);
    }

    #[test]
    fn expand_tilde_with_slash() {
        let user_env = make_test_env();
        let toks = "~/.config".to_string();
        let word = expand_word(&toks, &user_env, false).unwrap();
        let home = env::var("HOME").unwrap();
        assert_eq!(word, home + "/.config");
    }

    #[test]
    fn expand_tilde_user() {
        let user_env = make_test_env();
        let toks = "~root/.config".to_string();
        let word = expand_word(&toks, &user_env, false).unwrap();
        assert_eq!(word, "/root/.config".to_string());
    }

    #[test]
    fn expand_tilde_err_no_user() {
        let user_env = make_test_env();
        let toks = "~unknown/.config".to_string();
        let word = expand_word(&toks, &user_env, false);
        assert!(word.is_err());
    }

    #[test]
    fn expand_tilde_after_semi() {
        let user_env = make_test_env();
        let toks = "a:~".to_string();
        let word = expand_word(&toks, &user_env, true).unwrap();

        let home = env::var("HOME").unwrap();
        assert_eq!(word, "a:".to_string() + &home);
    }

    #[test]
    fn expand_tilde_after_semi_with_name() {
        let user_env = make_test_env();
        let toks = "a:~root:b".to_string();
        let word = expand_word(&toks, &user_env, true).unwrap();

        assert_eq!(word, "a:/root:b".to_string());
    }
}

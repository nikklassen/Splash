use std::env;
use std::ffi::{CStr, CString};
use std::iter;

use libc::{self, STDIN_FILENO, STDOUT_FILENO};
use nix::unistd::{self, ForkResult};

use env::UserEnv;
use eval::{self, InputReader};
use file::Fd;
use input::ast::*;
use input::token::Token;
use input::tokenizer::tokenize;
use process::Process;
use signals;
use state::ShellState;

fn run_command(input: &str) -> String {
    let (pipe_out, pipe_in) = unistd::pipe().unwrap();
    let mut pout = Fd::new(pipe_out);
    let mut pin = Fd::new(pipe_in);
    let f = unistd::fork().unwrap();
    if let ForkResult::Parent { .. } = f {
        pin.close();

        let mut output = String::new();
        let mut buffer = [0u8; 256];
        loop {
            match unistd::read(pout.raw_fd, &mut buffer) {
                Ok(0) => {
                    break;
                }
                Ok(n) => {
                    if let Ok(s) = String::from_utf8(buffer[..n].to_vec()) {
                        output.push_str(&s);
                    }
                }
                Err(e) => {
                    print_err!("{:?}", e);
                    break;
                }
            }
        }
        output
    } else {
        signals::cleanup_signals();

        pout.close();
        unistd::dup2(pin.raw_fd, STDOUT_FILENO).unwrap();
        unistd::close(STDIN_FILENO).unwrap();

        let command = InputReader::Command(input.lines().map(str::to_string).collect());
        let state = ShellState::new();
        eval::eval(command, state);

        unreachable!()
    }
}

fn split_fields(word: &String, user_env: &UserEnv) -> Vec<String> {
    let ifs = user_env.get("IFS");
    word.split(|c| ifs.contains(c))
        .filter(|s| s.len() > 0)
        .map(str::to_owned)
        .collect()
}

fn parameter_expansion(
    t: &String,
    user_env: &UserEnv,
    is_assignment: bool,
) -> Result<Vec<String>, String> {
    let mut s = String::new();
    let mut words = Vec::new();

    let mut chars = t.chars().peekable();
    let mut is_lit = false;
    let mut is_quoted = false;
    let mut escape = false;

    loop {
        match chars.next() {
            Some('$') => {
                if is_lit || escape {
                    escape = false;
                    s.push('$');
                    continue;
                }
                // fall-through to variable evaluation
            }
            Some(c) => {
                // TODO better quote handling
                if !escape {
                    if c == '\'' {
                        is_lit = !is_lit;
                    } else if c == '\"' {
                        is_quoted = !is_quoted;
                    }
                }
                escape = c == '\\';
                s.push(c);
                continue;
            }
            None => {
                break;
            }
        }

        let result;
        let next = chars.peek().map(|c| *c);
        if next.map(|c| c != '{' && c != '(').unwrap_or(false) {
            let c = next.unwrap();
            let param_name = if c.is_digit(10) {
                chars.next();
                c.to_string()
            } else {
                chars
                    .by_ref()
                    .take_while(|c| *c == '_' || c.is_alphanumeric())
                    .collect()
            };
            let param = user_env.get(&param_name);
            result = param;
        } else {
            let rest: String = iter::once('$').chain(chars.clone()).collect();
            let (tokens, _) = tokenize(&rest, true);
            let to_expand = if let Some(&Token::Word(ref s)) = tokens.first() {
                s.clone()
            } else {
                unreachable!()
            };

            let _ = chars.by_ref().skip(to_expand.len()).collect::<String>();
            result = if to_expand.starts_with(&"$((") {
                // TODO arithmetic
                String::new()
            } else if to_expand.starts_with(&"$(") {
                let input = &to_expand[2..to_expand.len() - 1];
                run_command(input)
            } else if to_expand.starts_with(&"${") {
                let chars = to_expand.chars();
                let param_name: String = chars
                    .clone()
                    .skip(2)
                    .take_while(|c| *c == '_' || c.is_alphanumeric())
                    .collect();

                let chars = chars.skip(param_name.len() + 2);
                // Only character remaining should be the trailing }
                if chars.count() > 1 {
                    // TODO more complicated variable manipulation
                    return Err("Bad substitution".to_string());
                }

                user_env.get(&param_name)
            } else {
                unreachable!()
            };
        }

        if is_quoted || is_assignment {
            s.push_str(&result);
        } else {
            let fields = split_fields(&result, user_env);
            if fields.len() == 1 {
                s.push_str(&fields[0]);
            } else if fields.len() > 1 {
                s.push_str(&fields[0]);
                words.push(s.clone());
                words.extend(fields[1..fields.len() - 1].iter().cloned());
                s = fields.last().unwrap().to_owned();
            }
        }
    }
    words.push(s.clone());
    Ok(words)
}

fn get_user_home(name: &str) -> Option<String> {
    let c_name;
    match CString::new(name) {
        Ok(s) => c_name = s,
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
        part.chars()
            .take_while(|&c| c != '/' && (!is_assignment || c != ':'))
            .collect()
    };
    fn get_home(user: &str) -> Result<String, String> {
        if user.len() > 0 {
            get_user_home(&user).ok_or(format!("Could not find user {}", &user))
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
                if end < s.len() && &s[end..end + 1] == "~" {
                    let name = get_name(&s[end + 1..]);
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
        Ok(home + &s[user.len() + 1..])
    } else {
        Ok(s.to_owned())
    }
}

fn quote_removal(word: &String) -> String {
    let mut escaped = false;
    word.chars()
        .filter(|c| {
            if escaped {
                true
            } else {
                escaped = *c == '\\';
                !(*c == '\'' || *c == '\"' || *c == '\\')
            }
        })
        .collect()
}

fn expand_word(
    word: &String,
    user_env: &UserEnv,
    is_assignment: bool,
) -> Result<Vec<String>, String> {
    let s = tilde_expansion(&word, is_assignment)?;
    let mut words = parameter_expansion(&s, user_env, is_assignment)?;
    words = words.into_iter().map(|s| quote_removal(&s)).collect();
    Ok(words)
}

pub fn expand(p: &mut Process, user_env: &UserEnv) -> Result<(), String> {
    let mut new_args = Vec::new();
    if p.prog.is_some() {
        let expanded = expand_word(p.prog.as_ref().unwrap(), user_env, false)?;
        p.prog = expanded.first().map(|s| s.clone());
        new_args = expanded[1..].iter().cloned().collect();
    }
    let expanded_args = p
        .args
        .iter()
        .map(|arg| expand_word(arg, user_env, false))
        .collect::<Result<Vec<_>, _>>()?;
    new_args.extend(expanded_args.into_iter().flat_map(|v| v));
    p.args = new_args;
    for ref mut prefix in p.env.iter_mut() {
        if let &mut CmdPrefix::Assignment { ref mut rhs, .. } = *prefix {
            // No field splitting on assignment fields, so this is just one word
            *rhs = expand_word(rhs, user_env, true).map(|ws| ws[0].clone())?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use env::UserEnv;
    use std::env;

    fn val() -> String {
        "value".to_string()
    }

    fn make_test_env() -> UserEnv {
        let mut user_env = UserEnv::new();
        user_env.set("TEST", &val());
        user_env.set("IFS", " \t\n");
        user_env
    }

    fn expand_basic(s: &String) -> String {
        let user_env = make_test_env();
        let mut words = expand_word(s, &user_env, false).unwrap();
        words.remove(0)
    }

    #[test]
    fn no_expansions() {
        let toks = "hello".to_string();
        let word = expand_basic(&toks);

        assert_eq!(word, toks);
    }

    #[test]
    fn expand_env_var() {
        let toks = "$HOME".to_string();
        let word = expand_basic(&toks);

        let home = env::var("HOME").unwrap_or(String::new());
        assert_eq!(word, home);
    }

    #[test]
    fn expand_user_var() {
        let toks = "$TEST".to_string();
        let word = expand_basic(&toks);
        assert_eq!(word, val());
    }

    #[test]
    fn expand_quoted() {
        let toks = r#""$TEST""#.to_string();
        let word = expand_basic(&toks);
        assert_eq!(word, val());
    }

    #[test]
    fn no_expand_var_in_single_quotes() {
        let toks = "'$TEST'".to_string();
        let word = expand_basic(&toks);

        assert_eq!(word, "$TEST".to_string());
    }

    #[test]
    fn expand_tilde() {
        let toks = "~".to_string();
        let word = expand_basic(&toks);
        let home = env::var("HOME").unwrap();
        assert_eq!(word, home);
    }

    #[test]
    fn expand_tilde_with_slash() {
        let toks = "~/.config".to_string();
        let word = expand_basic(&toks);
        let home = env::var("HOME").unwrap();
        assert_eq!(word, home + "/.config");
    }

    #[test]
    fn expand_tilde_user() {
        let toks = "~root/.config".to_string();
        let word = expand_basic(&toks);
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
    fn expand_tilde_after_semi_assignment() {
        let user_env = make_test_env();
        let toks = "a:~".to_string();
        let words = expand_word(&toks, &user_env, true).unwrap();

        let home = env::var("HOME").unwrap();
        assert_eq!(words[0], "a:".to_string() + &home);
    }

    #[test]
    fn expand_tilde_after_semi_with_name() {
        let user_env = make_test_env();
        let toks = "a:~root:b".to_string();
        let words = expand_word(&toks, &user_env, true).unwrap();

        assert_eq!(words[0], "a:/root:b".to_string());
    }

    #[test]
    fn no_expand_tilde_after_semi_assignment() {
        let toks = "a:~".to_string();
        let word = expand_basic(&toks);
        assert_eq!(word, "a:~");
    }

    #[test]
    fn expand_parameter() {
        let toks = "${TEST}".to_string();
        let word = expand_basic(&toks);

        assert_eq!(word, val());
    }

    #[test]
    fn expand_parameter_no_parameter() {
        let user_env = make_test_env();
        let toks = "${!}".to_string();
        let word = expand_word(&toks, &user_env, false);

        assert!(word.is_err());
    }

    #[test]
    fn expand_parameter_single_digit() {
        let mut user_env = make_test_env();
        user_env.set("0", "A");
        let toks = "$01".to_string();
        let word = expand_word(&toks, &user_env, false).unwrap();

        assert_eq!(word[0], "A1".to_string());
    }

    #[test]
    fn retain_escaped_dollar() {
        let toks = "\\$TEST".to_string();
        let word = expand_basic(&toks);

        assert_eq!(word, "$TEST".to_string());
    }

    #[test]
    fn split_expanded_var() {
        let mut user_env = make_test_env();
        user_env.set("X", "A           B");
        let toks = "$X".to_string();
        let words = expand_word(&toks, &user_env, false).unwrap();

        assert_eq!(words, vec!["A".to_string(), "B".to_string()]);
    }

    #[test]
    fn no_split_in_quotes() {
        let mut user_env = make_test_env();
        let val = "A  B".to_string();
        user_env.set("A", &val);
        let toks = r#""$A""#.to_string();
        let words = expand_word(&toks, &user_env, false).unwrap();

        assert_eq!(words, vec![val]);
    }

    #[test]
    fn split_command_into_args() {
        let mut user_env = make_test_env();
        user_env.set("X", "A           B");
        let mut p = Process::new(Some("$X".to_string()), Vec::new(), Vec::new(), Vec::new());
        let res = expand(&mut p, &user_env);

        assert!(res.is_ok());
        assert_eq!(p.prog, Some("A".to_string()));
        assert_eq!(p.args, vec!["B".to_string()]);
    }
}

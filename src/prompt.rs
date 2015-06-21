use builtin;
use builtin::BuiltinMap;
use parser;
use parser::AST;
use readline::*;
use std::env;
use std::io;
use std::process::Command;

static WAVE_EMOJI: &'static str = "\u{1F30A}";

pub fn input_loop() {
    let mut builtins = builtin::init_builtins();
    loop {
        let input = readline(&get_prompt_string());
        let line = match input {
            Ok(l) => l,
            // ^D
            Err(_) => return,
        };

        add_history(&line);

        let parsed = parser::parse_command(&line);
        if let Err(e) = parsed {
            println!("Error: {:?}", e.errors);
            continue;
        }

        let tokens: Vec<String>;
        match parsed.unwrap() {
            Some(ts) => {
                tokens = ts.into_iter().map(|t| {
                    match t {
                        AST::Word(s) => s,
                    }
                }).collect();
            },
            None => {
                continue;
            }
        }

        if tokens.len() == 0 {
            continue;
        }

        if let Err(e) = execute(&mut builtins, &tokens) {
            println!("{}", e);
        }
    }
}

fn get_prompt_string() -> String {
    let pwd = env::var("PWD")
        .map(|v| {
            let home = env::var("HOME").unwrap_or("".to_string());
            v.replace(&home, "~") + " "
        }).unwrap_or("".to_string());
    format!("{}{}  ", pwd, WAVE_EMOJI)
}

fn execute(builtins: &mut BuiltinMap, args: &Vec<String>) -> io::Result<i32> {
    if let Some(cmd) = builtins.get_mut(&args[0]) {
        return cmd.run(&args[1..]);
    }

    let mut child = try!(Command::new(&args[0])
        .args(&args[1..])
        .spawn());

    child.wait()
        .and_then(|result| {
            Ok(result.code().unwrap_or(0))
        })
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::path::PathBuf;
    use super::{get_prompt_string, WAVE_EMOJI};
    use test_fixture::*;

    struct PromptTests;

    impl TestFixture for PromptTests {
        fn tests(&self) -> TestList<Self> {
            vec![test!("prompt, unset pwd", prompt_unset_pwd),
            test!("prompt, includes pwd", prompt_includes_pwd),
            test!("prompt, includes home", prompt_includes_home),
            ]
        }
    }

    impl PromptTests {
        fn prompt_unset_pwd(&mut self) {
            let pwd = "my_dir";
            env::set_var("PWD", pwd);
            env::remove_var("PWD");
            let prompt = get_prompt_string();
            assert_eq!(prompt, WAVE_EMOJI.to_string() + "  ");
        }

        fn prompt_includes_pwd(&mut self) {
            let pwd = "my_dir";
            env::set_var("PWD", pwd);
            let prompt = get_prompt_string();
            assert!(prompt.starts_with(pwd));
        }

        fn prompt_includes_home(&mut self) {
            let home = "my_home";
            env::set_var("HOME", home);
            let dir = "my_dir";
            env::set_var("PWD", join(home, dir));
            let prompt = get_prompt_string();
            assert!(prompt.starts_with(
                    &join("~", dir)));
        }
    }

    fn join<'a>(p1: &str, p2: &str) -> String {
        let mut path: PathBuf = PathBuf::from(p1);
        path.push(p2);
        String::from(path.to_str().unwrap())
    }

    #[test]
    fn prompt_tests() {
        let fixture = PromptTests;
        test_fixture_runner(fixture);
    }
}

use std::fs::File;
use std::io::{BufRead, BufReader};

use builtin::BuiltinMap;
use env::UserEnv;
use input::token::*;
use input::{parser, prompt, tokenizer};
use process;

#[derive(Debug)]
pub enum InputReader {
    File(BufReader<File>),
    Command(Vec<String>),
    Stdin,
}

fn getline(input_method: &mut InputReader, cont: bool) -> Option<String> {
    match input_method {
        &mut InputReader::File(ref mut reader) => {
            let mut s = String::new();

            let res = reader.read_line(&mut s);
            if res.is_err() || res.unwrap() == 0 {
                None
            } else {
                // read_line doesn't remove trailing '\n', we also don't want any trailing '\r's
                Some(s.trim_right().to_string())
            }
        }
        &mut InputReader::Command(ref mut lines) => {
            if lines.is_empty() {
                None
            } else {
                Some(lines.remove(0))
            }
        }
        &mut InputReader::Stdin => prompt::getline(cont),
    }
}

pub fn eval(mut input_reader: InputReader, mut builtins: BuiltinMap) {
    let mut user_env = UserEnv::new();
    let mut last_status = 0;
    let mut line = String::new();

    loop {
        let cont = !line.is_empty();
        if let Some(next_line) = getline(&mut input_reader, cont) {
            line.push_str(&next_line);
        } else {
            break;
        }

        let (tokens, unterminated) = tokenizer::tokenize(&line, false);
        if unterminated {
            line.push('\n');
            continue;
        }
        line = String::new();

        let mut input: Vec<String> = Vec::new();
        let mut here_docs: Vec<(Token, String)> = Vec::new();
        let mut i = 0;
        while i < tokens.len() {
            match tokens[i] {
                Token::DLESS | Token::DLESSDASH => {
                    if let Token::Word(ref s) = tokens[i + 1] {
                        here_docs.push((tokens[i].clone(), s.clone()));
                    } else {
                        print_err!("here docs must be strings");
                        continue;
                    }
                    i += 2;
                }
                _ => {
                    i += 1;
                }
            }
        }
        for (kind, here_doc) in here_docs {
            let mut content = String::new();
            loop {
                if let Some(mut s) = getline(&mut input_reader, true) {
                    if kind == Token::DLESSDASH {
                        s = s
                            .chars()
                            .skip_while(|c| c.is_whitespace())
                            .collect::<String>();
                    }
                    if s == here_doc {
                        input.push(content);
                        break;
                    }
                    content.push_str(&s);
                    content.push_str("\n");
                } else {
                    // Replicate other shells' behaviour, just ignore this heredoc
                    print_err!("warning: here-document delimited by end-of-file (wanted `EOF')");
                    input.push(content);
                    break;
                }
            }
        }

        let parsed = parser::parse(tokens, &mut input);

        if let Err(e) = parsed {
            print_err!("{}", e);
            continue;
        }

        let commands = parsed.unwrap();
        if commands.is_empty() {
            continue;
        }

        for command in commands {
            let res = process::run_processes(&mut builtins, command, &mut user_env);
            match res {
                Err(e) => {
                    print_err!("{}", e);
                }
                Ok(n) => {
                    last_status = n;
                }
            };
        }
    }

    ::std::process::exit(last_status);
}

use std::process::Command;
use std::env;

fn run_command(cmd: &str) -> (i32, String) {
    let build_dir = env::var("SPLASH_BUILD_DIR").unwrap_or(String::from("target/debug"));
    let output = Command::new("sh")
        .arg("-c")
        .arg(format!("echo \"{}\" | splash", cmd))
        .env("PATH", format!("/bin:/usr/bin:{}", build_dir))
        .output()
        .unwrap_or_else(|e| { panic!("failed to execute process: {}", e) });

    let stdout = String::from_utf8(output.stdout).unwrap();
    (output.status.code().unwrap(), stdout)
}

#[test]
fn foo() {
    let (ecode, output) = run_command("pwd");
    println!("ecode: {}", ecode);
    println!("output: {}", output);
}

#[test]
fn run_builtin() {
    let (ecode, output) = run_command("echo hello");
    assert_eq!(ecode, 0);
    assert_eq!(output, String::from("hello\n"));
}

#[test]
fn run_external() {
    let (ecode, output) = run_command("expr 1 + 1");
    assert_eq!(ecode, 0);
    assert_eq!(output, String::from("2\n"));
}

#[test]
fn pipe_from_builtin_to_ext() {
    let (ecode, output) = run_command("echo hello | sed 's/$/ world/'");
    assert_eq!(ecode, 0);
    assert_eq!(output, String::from("hello world\n"));
}

#[test]
fn pipe_from_ext_to_ext() {
    let (ecode, output) = run_command("expr 1 + 1 | sed 's/.*/& &/'");
    assert_eq!(ecode, 0);
    assert_eq!(output, String::from("2 2\n"));
}

#[test]
fn continue_pipe_with_failure() {
    let (ecode, output) = run_command("not_a_program | echo hello");
    assert_eq!(ecode, 0);
    assert_eq!(output, String::from("hello\n"));
}

#[test]
fn continue_pipe_with_exit_nonzero() {
    let (ecode, output) = run_command("false | echo hello");
    assert_eq!(ecode, 0);
    assert_eq!(output, String::from("hello\n"));
}

#[test]
fn command_not_found() {
    let (ecode, output) = run_command("not_a_program");
    assert_eq!(ecode, 127);
    assert_eq!(output, String::from(""));
}

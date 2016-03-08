extern crate tempfile;

use std::env;
use std::io::{Read, Write};
use std::process::{Command, Output};
use tempfile::NamedTempFile;

#[allow(dead_code)]
/// Useful for debugging, only needs to be called in that situation
fn print_extra_info(output: &Output) {
    println!("stderr: {}", String::from_utf8(output.stderr.clone()).unwrap());
}

fn run_command<S: Into<String>>(cmd: S) -> (i32, String) {
    let build_dir = env::var("SPLASH_BUILD_DIR").unwrap_or(String::from("target/debug"));

    let output = Command::new("sh")
                     .arg("-c")
                     .arg(format!("echo \"{}\" | splash", cmd.into()))
                     .env("PATH", format!("/bin:/usr/bin:{}", build_dir))
                     .output()
                     .unwrap_or_else(|e| panic!("failed to execute process: {}", e));

    let stdout = String::from_utf8(output.stdout).unwrap();
    (output.status.code().unwrap(), stdout)
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
fn pipe_from_ext_to_builtin() {
    let (ecode, output) = run_command("expr 1 + 1 | echo 'hello world'");
    assert_eq!(ecode, 0);
    assert_eq!(output, String::from("hello world\n"));
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

#[test]
fn redirect_out() {
    let mut f = NamedTempFile::new().unwrap();

    let (ecode, output) = run_command(format!("echo 'foo' > {}", f.path().display()));

    assert_eq!(ecode, 0);
    assert_eq!(output, String::from(""));

    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();
    assert_eq!(contents, "foo\n");
}

#[test]
fn redirect_in() {
    let mut f = NamedTempFile::new().unwrap();
    f.write(b"hello\n").unwrap();
    f.flush().unwrap();

    let (ecode, output) = run_command(format!("cat < {}", f.path().display()));

    assert_eq!(ecode, 0);
    assert_eq!(output, String::from("hello\n"));
}

#[test]
fn redirect_overrides_pipe() {
    let mut f = NamedTempFile::new().unwrap();
    f.write(b"hello\n").unwrap();
    f.flush().unwrap();

    let (ecode, output) = run_command(format!("echo 'hi' | cat < {}", f.path().display()));

    assert_eq!(output, String::from("hello\n"));
    assert_eq!(ecode, 0);
}

#[test]
fn redirect_in_other_fd() {
    let mut f = NamedTempFile::new().unwrap();
    f.write(b"hello\n").unwrap();
    f.flush().unwrap();

    let (ecode, output) = run_command(format!("cat /dev/fd/3 3< {}", f.path().display()));

    assert_eq!(output, String::from("hello\n"));
    assert_eq!(ecode, 0);
}

#[test]
fn redirect_out_other_fd() {
    let mut f = NamedTempFile::new().unwrap();

    let (ecode, output) = run_command(format!("echo 'hello' | cat 3> {} >&3", f.path().display()));

    assert_eq!(output, String::new());
    assert_eq!(ecode, 0);

    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();
    assert_eq!(contents, "hello\n");
}

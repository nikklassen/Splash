extern crate libc;
extern crate nix;
extern crate tempfile;

use std::collections::HashMap;
use std::env;
use std::io::{Read, Write};
use std::os::unix::process::ExitStatusExt;
use std::process::{Command, Output, Stdio};
use std::thread;
use std::time::Duration;

use nix::sys::signal;
use nix::unistd::Pid;
use tempfile::NamedTempFile;

#[allow(dead_code)]
/// Useful for debugging, only needs to be called in that situation
fn print_extra_info(output: &Output) {
    println!(
        "stderr: {}",
        String::from_utf8(output.stderr.clone()).unwrap()
    );
}

fn set_build_dir(cmd: &mut Command) -> &mut Command {
    let build_dir = env::var("SPLASH_BUILD_DIR").unwrap_or(String::from("target/debug"));
    cmd.env("PATH", format!("/bin:/usr/bin:{}", build_dir))
}

struct CmdResult {
    pub status: i32,
    pub stdout: String,
    pub stderr: String,
}

fn run_in_splash_with_env(shell_cmd: &str, env: HashMap<String, String>) -> CmdResult {
    let mut input = NamedTempFile::new().unwrap();
    input.write(shell_cmd.as_bytes()).unwrap();
    input.flush().unwrap();

    let mut cmd = Command::new("splash");
    let _ = cmd
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .arg(&input.path());

    set_build_dir(&mut cmd);
    for (var, val) in env {
        cmd.env(var, val);
    }

    let output = cmd
        .output()
        .unwrap_or_else(|e| panic!("failed to execute process: {}", e));

    let stdout = String::from_utf8(output.stdout).unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();

    CmdResult {
        status: output.status.code().unwrap(),
        stdout: stdout,
        stderr: stderr,
    }
}

fn run_in_splash(shell_cmd: &str) -> CmdResult {
    run_in_splash_with_env(shell_cmd, HashMap::new())
}

#[test]
fn run_builtin() {
    let result = run_in_splash("echo hello");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "hello\n");
}

#[test]
fn run_external() {
    let result = run_in_splash("expr 1 + 1");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "2\n");
}

#[test]
fn pipe_from_builtin_to_ext() {
    let result = run_in_splash("echo hello | sed 's/$/ world/'");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "hello world\n");
}

#[test]
fn pipe_from_ext_to_ext() {
    let result = run_in_splash("expr 1 + 1 | sed 's/.*/& &/'");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "2 2\n");
}

#[test]
fn pipe_from_ext_to_builtin() {
    let result = run_in_splash("expr 1 + 1 | echo 'hello world'");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "hello world\n");
}

#[test]
fn continue_pipe_with_failure() {
    let result = run_in_splash("not_a_program | echo hello");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "hello\n");
}

#[test]
fn continue_pipe_with_exit_nonzero() {
    let result = run_in_splash("false | echo hello");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "hello\n");
}

#[test]
fn command_not_found() {
    let result = run_in_splash("not_a_program");
    assert_eq!(result.status, 127);
    assert_eq!(result.stdout, "");
}

#[test]
fn redirect_out() {
    let mut f = NamedTempFile::new().unwrap();

    let result = run_in_splash(&format!("echo 'foo' > {}", f.path().display()));

    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "");

    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();
    assert_eq!(contents, "foo\n");
}

#[test]
fn redirect_in() {
    let mut f = NamedTempFile::new().unwrap();
    f.write(b"hello\n").unwrap();
    f.flush().unwrap();

    let result = run_in_splash(&format!("cat < {}", f.path().display()));

    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "hello\n");
}

#[test]
fn redirect_overrides_pipe() {
    let mut f = NamedTempFile::new().unwrap();
    f.write(b"hello\n").unwrap();
    f.flush().unwrap();

    let result = run_in_splash(&format!("echo 'hi' | cat < {}", f.path().display()));

    assert_eq!(result.stdout, "hello\n");
    assert_eq!(result.status, 0);
}

#[test]
fn redirect_in_other_fd() {
    let mut f = NamedTempFile::new().unwrap();
    f.write(b"hello\n").unwrap();
    f.flush().unwrap();

    let result = run_in_splash(&format!("cat /dev/fd/3 3< {}", f.path().display()));

    assert_eq!(result.stdout, "hello\n");
    assert_eq!(result.status, 0);
}

#[test]
fn redirect_out_other_fd() {
    let mut f = NamedTempFile::new().unwrap();

    let result = run_in_splash(&format!("echo 'hello' | cat 3> {} >&3", f.path().display()));

    assert_eq!(result.stdout, "");
    assert_eq!(result.status, 0);

    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();
    assert_eq!(contents, "hello\n");
}

#[test]
fn redirect_out_err() {
    let mut f = NamedTempFile::new().unwrap();

    let result = run_in_splash(&format!("echo 'hello' | cat 2> {} >&2", f.path().display()));

    assert_eq!(result.stdout, "");
    assert_eq!(result.stderr, "");
    assert_eq!(result.status, 0);

    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();
    assert_eq!(contents, "hello\n");
}

#[test]
fn heredoc_in() {
    let result = run_in_splash(&"cat <<EOF\nabc\nEOF");

    assert_eq!(result.stdout, "abc\n");
    assert_eq!(result.status, 0);
}

#[test]
fn heredoc_in_no_whitespace() {
    let result = run_in_splash(&"cat <<-EOF\n   abc\n  EOF");

    assert_eq!(result.stdout, "abc\n");
    assert_eq!(result.status, 0);
}

#[test]
fn no_stop_with_sigtstp() {
    let mut splash_cmd = Command::new("splash");
    let mut splash = set_build_dir(&mut splash_cmd).spawn().unwrap();

    let pid = Pid::from_raw(splash.id() as i32);

    signal::kill(pid, signal::SIGTSTP).unwrap();
    thread::sleep(Duration::from_millis(100));
    splash.kill().unwrap();

    let status = splash.wait().unwrap();
    assert_eq!(status.signal().unwrap(), 9);
}

#[test]
fn delayed_expansion_of_words() {
    let mut env = HashMap::new();
    env.insert("A".to_string(), "A".to_string());
    let result = run_in_splash_with_env("A=B | echo $A", env);
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "B\n");
}

#[test]
fn and_short_circuit() {
    let result = run_in_splash("false && echo hi");
    assert_eq!(result.status, 1);
    assert_eq!(result.stdout, "");
}

#[test]
fn and() {
    let result = run_in_splash("true && echo hi");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "hi\n");
}

#[test]
fn or_short_circuit() {
    let result = run_in_splash("true || echo hi");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "");
}

#[test]
fn or() {
    let result = run_in_splash("false || echo hi");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "hi\n");
}

#[test]
fn expand_command() {
    let result = run_in_splash("echo $(echo hi)");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "hi\n\n");
}

#[test]
fn if_true_branch() {
    let result = run_in_splash("if true; then echo true; else echo false; fi");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "true\n");
}

#[test]
fn if_false_branch() {
    let result = run_in_splash("if false; then echo true; else echo false; fi");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "false\n");
}

#[test]
fn if_elif_branch() {
    let result = run_in_splash("if false; then echo true; elif true; then echo true2; fi");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "true2\n");
}

#[test]
fn brace_group_vars() {
    let result = run_in_splash("A=B; { A=C; echo $A; }; echo $A");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "C\nC\n");
}

#[test]
fn subshell_vars() {
    let result = run_in_splash("A=B; ( A=C; echo $A; ); echo $A");
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout, "C\nB\n");
}

#[test]
fn subshell_pwd() {
    let result = run_in_splash(
        r#"
    (cd ..)
    echo "$PWD"
    "#,
    );
    assert_eq!(result.status, 0);
    assert_eq!(result.stdout.trim(), env::var("PWD").unwrap());
}

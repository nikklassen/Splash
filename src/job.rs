use std::collections::VecDeque;
use std::io::{stderr, Write, Error};

use lazy_static;
use libc::STDIN_FILENO;
use nix::errno::Errno;
use nix::sys::signal;
use nix::sys::wait::{self, WaitStatus};
use nix::{self, unistd};

use process::Process;
use util::{self, SharedTable};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum JobStatus {
    New,
    // Foreground
    Running,
    Stopped,
    // Error code
    Done(i32),
}

#[derive(Debug, Clone)]
pub struct Job {
    pub id: i32,
    pub pid: i32,
    pub pgid: i32,
    // TODO this should be set from the original user input
    pub cmd: String,
    pub status: JobStatus,
    // Save this separately so we can know if the job finished in the foreground
    pub foreground: bool,
}

impl Job {
    pub fn new(pid: i32, pgid: i32, cmd: &str, async: bool) -> Self {
        let status = if pid == unistd::getpid() {
            // The job can't still be running if we're at this point
            JobStatus::Done(0)
        } else {
            // Technically the job is already running, but we haven't specified how we want it to run yet, so it doesn't really count
            JobStatus::New
        };
        Job {
            id: 0, // No id assigned yet
            pid: pid, // The pid of the last process in the group
            pgid: pgid,
            cmd: cmd.to_string(),
            status: status,
            foreground: !async,
        }
    }

    pub fn update_status(&mut self) {
        if let JobStatus::Done(_) = self.status {
            // There's no coming back from done, we'll just get an ECHLD when we wait
            return;
        }

        self.status = match get_job_status(self.pgid) {
            Ok(WaitStatus::Exited(_, exit_code)) => JobStatus::Done(exit_code as i32),
            Ok(WaitStatus::Signaled(_, sig, _)) => JobStatus::Done(128 + sig as i32),
            Ok(WaitStatus::Stopped(_, _)) => JobStatus::Stopped,
            Ok(WaitStatus::Continued(_)) => {
                self.foreground = unistd::tcgetpgrp(STDIN_FILENO)
                    .and_then(|pgid| Ok(self.pgid == pgid))
                    .unwrap_or(false);
                JobStatus::Running
            }
            // Process is shy and doesn't want to say anything (that means no change) or this failed for some reason
            // (don't change anything)
            Ok(WaitStatus::StillAlive) | Ok(WaitStatus::PtraceEvent(_, _, _)) | Err(_) => self.status,
        };
    }
}

#[derive(Clone)]
pub struct JobTable {
    jobs: VecDeque<Job>,
}

lazy_static! {
    static ref JOB_TABLE: SharedTable<JobTable> = SharedTable::new(JobTable::new());
}

pub fn initialize_job_table() {
    // Prevents a deadlock that can occur in the SIGCHLD handler under some
    // conditions if it is accessing the job table for the first time
    lazy_static::initialize(&JOB_TABLE);
}

impl JobTable {
    pub fn new() -> Self {
        JobTable {
            jobs: VecDeque::new(),
        }
    }

    pub fn update_job_list(&mut self) {
        for i in 0..self.jobs.len() {
            let status = self.jobs[i].status;
            if is_match!(status, JobStatus::Done(_)) {
                let job = &self.jobs[i];
                // Don't print info for jobs that were never managed or terminated in the foreground
                // TODO killed notification (exit reasons can be something other than "finished")
                if job.id != 0 && !job.foreground {
                    println!("\n[{}] done\t{}", job.id, job.cmd);
                }
                continue;
            }
            if status != JobStatus::Stopped {
                continue;
            }

            if self.jobs[i].id == 0 {
                self.set_job_id(i);
                let job = &self.jobs[i];
                println!("suspended\t{}", job.cmd);
            }
        }

        self.jobs.retain(|j| !is_match!(j.status, JobStatus::Done(_)));
    }

    pub fn update_jobs(&mut self) {
        for job in self.jobs.iter_mut() {
            job.update_status();
        }
        self.update_job_list();
    }

    fn set_job_id(&mut self, job_num: usize) -> i32 {
        let mut i = 1;
        // Find the first available job number (job table may have "holes")
        loop {
            let mut has_key = false;
            for job in self.jobs.iter() {
                if job.id == i {
                    has_key = true;
                    break;
                }
            }
            if !has_key {
                self.jobs[job_num].id = i;
                break;
            }
            i += 1;
        }
        self.jobs[job_num].id
    }

    pub fn add_job(&mut self, job: Job) {
        self.jobs.push_back(job);
        let job_ref = self.jobs.back().unwrap().clone();
        if !job_ref.foreground {
            let id = self.set_job_id(job_ref.id as usize);
            println!("[{}] {}", id, job_ref.pid);
        }
        unsafe {
            use libc;
            use bindings::readline;
            libc::fflush(readline::rl_outstream);
        }
    }

    pub fn get_job_mut(&mut self, job_id: i32) -> Option<&mut Job> {
        if job_id < 1 {
            return None;
        }
        self.jobs.iter_mut().find(|j| j.id == job_id)
    }

    pub fn last_job_mut(&mut self) -> Option<&mut Job> {
        self.jobs.back_mut()
    }

    pub fn last_job(&self) -> Option<&Job> {
        self.jobs.back()
    }
}

pub fn print_jobs() {
    let job_table = JOB_TABLE.get_inner();
    let mut managed_jobs: Vec<&Job> = job_table.jobs.iter().filter(|j| j.id != 0).collect();
    managed_jobs.sort_by(|a, b| a.id.cmp(&b.id));
    for job in managed_jobs {
        let status = match job.status {
            JobStatus::Done(_) => "done",
            JobStatus::Stopped => "suspended",
            // These two shouldn't appear with the jobs command, but they may be useful for debugging
            JobStatus::Running => "running",
            JobStatus::New => "new",
        };
        println!("[{}] {}\t{}", job.id, status, job.cmd);
    }
}

pub fn start_job(foreground: bool) -> Result<i32, Error> {
    let job_entry = {
        let job_table = JOB_TABLE.get_inner();
        job_table.last_job().map(|j| j.clone())
    };
    if let Some(ref job) = job_entry {
        println!("[{}]  {} continued\t{}", job.id,
              if foreground { "-" } else { "+" }, job.cmd);
        let res = if foreground {
            foreground_job(job)
        } else {
            background_job(job)
        };
        if let Err(e) = res {
            print_err!("{}", e);
            Ok(2)
        } else {
            Ok(0)
        }
    } else {
        writeln!(stderr(), "{}: no current job",
              if foreground { "fg" } else { "bg" }).unwrap();
        Ok(1)
    }
}

pub fn add_job(p: &Process) -> Result<Job, String> {
    if p.prog == None {
        print_err!("bad process to job: {:?}", p);
        return Err("Empty process can't be jobbed".to_string());
    }

    let cmd = format!("{}", p);
    let new_job = Job::new(p.pid, p.pgid, &cmd, p.async);
    JOB_TABLE.get_inner().add_job(new_job.clone());

    Ok(new_job)
}

fn update_job<F>(job_id: i32, f: F) -> bool
where F: FnOnce(&mut Job) -> () {
    let mut inner = JOB_TABLE.get_inner();
    let maybe_job = inner.jobs.iter_mut().find(|j| j.id == job_id);
    let ret = maybe_job.is_some();
    if let Some(mut job) = maybe_job {
        f(job);
    }
    ret
}

pub fn foreground_job(job: &Job) -> Result<i32, String> {
    if job.status == JobStatus::Running && job.foreground {
        return Err("Job is already in the foreground".to_string());
    }
    let shell_pgid = unistd::getpgrp();
    try!(unistd::tcsetpgrp(STDIN_FILENO, job.pgid).or_else(util::show_err));

    if job.status == JobStatus::Stopped {
        resume_job(job)?;
    }

    let ret = wait_for_job(job);

    unistd::tcsetpgrp(STDIN_FILENO, shell_pgid).expect("Unable to foreground splash");

    ret
}

pub fn background_job(job: &Job) -> Result<i32, String>  {
    if job.status != JobStatus::New {
        resume_job(job)?;
    }

    update_job(job.id, |mut_job| {
        mut_job.status = JobStatus::Running;
        mut_job.foreground = false;
    });
    Ok(0)
}

pub fn wait_for_job(job: &Job) -> Result<i32, String> {
    update_job(job.id, |mut_job| {
        mut_job.status = JobStatus::Running;
        mut_job.foreground = true;
    });

    let status = join_job(job);

    update_job(job.id, |mut_job| {
        mut_job.status = status;
    });

    if let JobStatus::Done(exit_code) = status {
        Ok(exit_code)
    } else {
        Ok(128 + (signal::SIGTSTP as i32))
    }
}

fn join_job(job: &Job) -> JobStatus {
    let mut status = JobStatus::New;
    loop {
        match wait::waitpid(job.pid, Some(wait::WUNTRACED)) {
            Ok(WaitStatus::Exited(_pid, prog_exit_code)) => {
                let exit_code = if prog_exit_code < 0 {
                    match Errno::from_i32(!prog_exit_code as i32) {
                        Errno::ENOENT => {
                            writeln!(stderr(), "{}: command not found", job.cmd).unwrap();
                        }
                        Errno::EACCES => {
                            print_err!("permission denied: {}", job.cmd)
                        }
                        Errno::ENOTDIR => {
                            print_err!("not a directory: {}", job.cmd)
                        }
                        e => {
                            writeln!(stderr(), "{}: {}", e.desc(), job.cmd).unwrap();
                        }
                    }
                    127
                } else {
                    prog_exit_code as i32
                };
                status = JobStatus::Done(exit_code);
                break;
            }
            Ok(WaitStatus::Signaled(_pid, sig, _)) => {
                let exit_code = 128 + (sig as i32);
                status = JobStatus::Done(exit_code);
                break;
            }
            Ok(WaitStatus::StillAlive) | Ok(WaitStatus::Continued(_)) => {
                unreachable!("Not sure how we finished waiting for a job that's still alive")
            }
            Ok(WaitStatus::Stopped(_pid, _sig)) => {
                status = JobStatus::Stopped;
                break;
            }
            Ok(WaitStatus::PtraceEvent(_, _, _)) => continue,
            Err(nix::Error::Sys(nix::Errno::ECHILD)) => {
                // This is bad, I don't know how we could get in this state
                panic!("No child to wait for");
            }
            Err(nix::Error::Sys(nix::Errno::EINTR)) => {
                // This is not normal but possible, try again
                continue;
            }
            Err(e) => {
                print_err!("could not wait for pid {} due to {}", job.pid, e);
                break;
            }
        }
    }
    status
}

fn resume_job(job: &Job) -> Result<(), String> {
    signal::kill(job.pgid, signal::SIGCONT)
        .or(Err(format!("Failed to continue job {}", job.id)))
}

fn get_job_status(pid: i32) -> nix::Result<WaitStatus> {
    wait::waitpid(pid, Some(wait::WUNTRACED | wait::WNOHANG))
}

pub fn update_job_list() {
    let mut job_table = JOB_TABLE.get_inner();
    job_table.update_job_list();
}

pub fn update_job_status(job_id: i32) {
    update_job(job_id, |mut_job| {
        mut_job.update_status();
    });
}

pub fn update_jobs() {
    let mut job_table = JOB_TABLE.get_inner();
    job_table.update_jobs();
}

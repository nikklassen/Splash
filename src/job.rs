use std::borrow::Borrow;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex, MutexGuard};

use libc::STDIN_FILENO;
use nix::errno::Errno;
use nix::sys::signal;
use nix::sys::wait::{self, WaitStatus};
use nix::{self, unistd};

use bindings::nix::{tcgetpgrp, tcsetpgrp, getpgrp};
use util;

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
    pub fn new(pid: i32, pgid: i32, cmd: &str) -> Self {
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
            foreground: true,
        }
    }

    pub fn foreground(&mut self) -> Result<i32, String> {
        if self.status == JobStatus::Running && self.foreground {
            return Err("Job is already in the foreground".to_string());
        }
        let shell_pgid = try!(getpgrp().or_else(util::show_err));
        try!(tcsetpgrp(STDIN_FILENO, self.pgid).or_else(util::show_err));

        if self.status == JobStatus::Stopped {
            try!(self.resume());
        }
        self.status = JobStatus::Running;
        self.foreground = true;

        self.join();

        tcsetpgrp(STDIN_FILENO, shell_pgid).expect("Unable to foreground splash");

        if let JobStatus::Done(exit_code) = self.status {
            Ok(exit_code)
        } else {
            Ok(128 + (signal::SIGTSTP as i32))
        }
    }

    pub fn background(&mut self) -> Result<i32, String> {
        self.resume().and_then(|_| {
            self.status = JobStatus::Running;
            self.foreground = false;
            Ok(0)
        })
    }

    pub fn join(&mut self) {
        loop {
            match wait::waitpid(self.pid, Some(wait::WUNTRACED)) {
                Ok(WaitStatus::Exited(_pid, exit_code)) => {
                    let exit_code = if exit_code < 0 {
                        match Errno::from_i32(!exit_code as i32) {
                            Errno::ENOENT => {
                                warn!("{}: command not found", self.cmd)
                            }
                            Errno::EACCES => {
                                warn!("permission denied: {}", self.cmd)
                            }
                            Errno::ENOTDIR => {
                                warn!("not a directory: {}", self.cmd)
                            }
                            e => warn!("{}: {}", e.desc(), self.cmd),
                        }
                        127
                    } else {
                        exit_code as i32
                    };
                    self.status = JobStatus::Done(exit_code);
                    break;
                }
                Ok(WaitStatus::Signaled(_pid, sig, _)) => {
                    let exit_code = 128 + (sig as i32);
                    self.status = JobStatus::Done(exit_code);
                    break;
                }
                Ok(WaitStatus::StillAlive) | Ok(WaitStatus::Continued(_)) => {
                    unreachable!("Not sure how we finished waiting for a job that's still alive")
                }
                Ok(WaitStatus::Stopped(_pid, _sig)) => {
                    self.status = JobStatus::Stopped;
                    break;
                }
                Err(nix::Error::Sys(nix::Errno::ECHILD)) => {
                    // This is bad, I don't know how we could get in this state
                    panic!("No child to wait for");
                }
                Err(nix::Error::Sys(nix::Errno::EINTR)) => {
                    // This is not normal but possible, try again
                    debug!("Child stopped");
                    continue;
                }
                Err(e) => {
                    error!("could not wait for pid {} due to {}", self.pid, e);
                    break;
                }
            }
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
                self.foreground = tcgetpgrp(STDIN_FILENO)
                    .and_then(|pgid| Ok(self.pgid == pgid))
                    .unwrap_or(false);
                JobStatus::Running
            }
            // Process is shy and doesn't want to say anything, that means no change
            Ok(WaitStatus::StillAlive) => self.status,
            Err(e) => {
                error!("Failed to update status for {:?} due to {:?}", self, e);
                self.status
            }
        };
    }

    fn resume(&mut self) -> Result<(), String> {
        signal::kill(self.pgid, signal::SIGCONT)
            .or(Err(format!("Failed to continue job {}", self.id)))

    }
}

pub struct JobTable {
    jobs: VecDeque<Job>,
}

#[derive(Clone)]
pub struct SharedJobTable {
    job_table_ref: Arc<Mutex<JobTable>>,
}

impl SharedJobTable {
    pub fn new() -> Self {
        SharedJobTable {
            job_table_ref: Arc::new(Mutex::new(JobTable::new()))
        }
    }

    pub fn get_inner(&self) -> MutexGuard<JobTable> {
        // If something went wrong and poisoned the lock we're screwed anyways so may as well just panic in all the threads
        (self.job_table_ref.borrow() as &Mutex<JobTable>).lock().unwrap()
    }
}

lazy_static! {
    pub static ref JOB_TABLE: SharedJobTable = SharedJobTable::new();
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
                    info!("[{}] done\t{}", job.id, job.cmd);
                }
                continue;
            }

            if self.jobs[i].id == 0 {
                self.set_job_id(i);
                let job = &self.jobs[i];
                info!("suspended\t{}", job.cmd);
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

    fn set_job_id(&mut self, job_num: usize) {
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
    }

    pub fn print_jobs(&self) {
        let mut managed_jobs: Vec<&Job> = self.jobs.iter().filter(|j| j.id != 0).collect();
        managed_jobs.sort_by(|a, b| a.id.cmp(&b.id));
        for job in managed_jobs {
            let status = match job.status {
                JobStatus::Done(_) => "done",
                JobStatus::Stopped => "suspended",
                // These two shouldn't appear with the jobs command, but they may be useful for debugging
                JobStatus::Running => "running",
                JobStatus::New => "new",
            };
            info!("[{}] {}\t{}", job.id, status, job.cmd);
        }
    }

    pub fn add_job(&mut self, job: Job) -> &mut Job {
        self.jobs.push_back(job);
        self.jobs.back_mut().unwrap()
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
}

pub fn get_job_status(pid: i32) -> nix::Result<WaitStatus> {
    wait::waitpid(pid, Some(wait::WUNTRACED | wait::WNOHANG))
}

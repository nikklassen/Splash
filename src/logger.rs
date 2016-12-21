use std::io::{self, Write};

pub use log::LogLevel;
use log::{self, LogRecord, LogMetadata, LogLevelFilter, SetLoggerError};

struct SimpleLogger {
    name: String,
    log_level: LogLevel,
}

impl log::Log for SimpleLogger {

    fn enabled(&self, metadata: &LogMetadata) -> bool {
        metadata.level() <= self.log_level
    }

    fn log(&self, record: &LogRecord) {
        if !self.enabled(record.metadata()) {
            return;
        }

        if record.level() <= LogLevel::Warn {

            let stderr = io::stderr();
            let mut stderr_locked = stderr.lock();
            if record.level() == LogLevel::Error {
                // Internal errors
                let _ = write!(stderr_locked, "{}: ", self.name);
            }
            let _ = writeln!(stderr_locked, "{}", record.args());
        } else {
            println!("{}", record.args());
        }
    }
}

pub fn init(name: &str, log_level: LogLevel) -> Result<(), SetLoggerError> {
    log::set_logger(|max_log_level| {
        max_log_level.set(LogLevelFilter::Trace);
        Box::new(SimpleLogger {
            name: name.to_string(),
            log_level: log_level,
        })
    })
}

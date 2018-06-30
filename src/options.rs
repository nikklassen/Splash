use libc::STDIN_FILENO;
use nix::unistd::isatty;
use std::collections::HashMap;

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum SOpt {
    Interactive,
}

pub struct OptionTable(HashMap<SOpt, bool>);

impl OptionTable {
    pub fn new() -> Self {
        let mut options = HashMap::new();
        // Set all options to their default
        options.insert(SOpt::Interactive, true);
        OptionTable(options)
    }

    pub fn get(&self, opt: SOpt) -> bool {
        // All options will always be set
        *self.0.get(&opt).unwrap()
    }

    pub fn set(&mut self, opt: SOpt, value: bool) {
        self.0.insert(opt, value);
    }
}

pub fn is_interactive(opts: &OptionTable) -> bool {
    opts.get(SOpt::Interactive) && isatty(STDIN_FILENO).unwrap_or(false)
}

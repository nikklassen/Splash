use lazy_static;
use libc::STDIN_FILENO;
use nix::unistd::isatty;
use std::collections::HashMap;
use util::SharedTable;

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum SOpt {
    Interactive,
}

pub type OptionTable = HashMap<SOpt, bool>;

lazy_static! {
    static ref OPTIONS: SharedTable<OptionTable> = SharedTable::new(HashMap::new());
}

pub fn initialize_options() {
    lazy_static::initialize(&OPTIONS);

    let mut options = OPTIONS.get_inner();
    options.insert(SOpt::Interactive, true);
}

pub fn get_opt(opt: SOpt) -> bool {
    *OPTIONS.get_inner().get(&opt).unwrap()
}

pub fn set_opt(opt: SOpt, value: bool) {
    let _ = OPTIONS.get_inner().insert(opt, value);
}

pub fn is_interactive() -> bool {
    get_opt(SOpt::Interactive) && isatty(STDIN_FILENO).unwrap_or(false)
}

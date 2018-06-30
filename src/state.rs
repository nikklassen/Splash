use builtin::{self, BuiltinMap};
use env::UserEnv;
use options::OptionTable;

/// The state of the current (sub)shell
pub struct ShellState {
    pub builtins: BuiltinMap,
    pub env: UserEnv,
    pub opts: OptionTable,
}

impl ShellState {
    pub fn new() -> Self {
        ShellState {
            builtins: builtin::init_builtins(),
            env: UserEnv::new(),
            opts: OptionTable::new(),
        }
    }
}

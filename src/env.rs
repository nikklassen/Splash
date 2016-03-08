use std::collections::HashMap;

pub struct UserEnv {
    pub vars: HashMap<String, String>,
}

impl UserEnv {
    pub fn new() -> Self {
        UserEnv {
            vars: HashMap::new()
        }
    }
}


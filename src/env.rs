use std::collections::HashMap;
use std::env;

pub struct UserEnv {
    pub vars: HashMap<String, String>,
}

impl UserEnv {
    pub fn new() -> Self {
        UserEnv {
            vars: HashMap::new(),
        }
    }

    pub fn get_var(&self, var: &str) -> String {
        self.vars
            .get(var)
            .map(|value| value.clone())
            .unwrap_or_else(|| env::var(var).unwrap_or(String::new()))
    }
}

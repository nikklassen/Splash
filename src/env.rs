use std::collections::HashMap;
use std::env;

#[derive(Clone)]
pub struct UserEnv {
    vars: HashMap<String, String>,
}

impl UserEnv {
    pub fn new() -> Self {
        UserEnv {
            vars: HashMap::new(),
        }
    }

    pub fn get<S: AsRef<str>>(&self, var: S) -> String {
        self.vars
            .get(var.as_ref())
            .map(|value| value.clone())
            .unwrap_or_else(|| env::var(var.as_ref()).unwrap_or(String::new()))
    }

    pub fn set<S: AsRef<str>>(&mut self, var: S, value: S) {
        self.vars
            .insert(var.as_ref().to_owned(), value.as_ref().to_owned());
    }
}

use crate::token::ctypes::Var;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Env {
    pub vars: HashMap<String, Var>,
    pub next: Option<Box<Env>>,
}

impl Env {
    pub fn new(next: Option<Box<Env>>) -> Self {
        Env {
            vars: HashMap::new(),
            next,
        }
    }
}

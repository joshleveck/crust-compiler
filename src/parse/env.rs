use std::collections::HashMap;

use crate::token::ctypes::Type;

#[derive(Debug, Clone)]
pub struct Env {
    pub tags: HashMap<String, Type>,
    pub typedefs: HashMap<String, Type>,
    pub next: Option<Box<Env>>,
}

impl Env {
    pub fn new(next: Option<Box<Env>>) -> Self {
        Env {
            next,
            tags: HashMap::new(),
            typedefs: HashMap::new(),
        }
    }
}

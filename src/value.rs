//! Runtime values stored in registers.

use crate::Pid;

/// Runtime value stored in registers
#[derive(Clone)]
pub enum Value {
    Pid(Pid),
    String(String),
    None,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Pid(p) => write!(f, "Pid({})", p.0),
            Value::String(s) => write!(f, "{:?}", s),
            Value::None => write!(f, "None"),
        }
    }
}

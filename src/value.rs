//! Runtime values stored in registers.

use crate::Pid;

/// Runtime value stored in registers
#[derive(Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Pid(Pid),
    String(String),
    None,
}

impl Value {
    /// Try to extract an integer from this value
    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Int(n) => Some(*n),
            _ => None,
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Pid(p) => write!(f, "Pid({})", p.0),
            Value::String(s) => write!(f, "{:?}", s),
            Value::None => write!(f, "None"),
        }
    }
}

impl From<i64> for Value {
    fn from(n: i64) -> Self {
        Value::Int(n)
    }
}

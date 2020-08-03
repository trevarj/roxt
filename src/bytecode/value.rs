use std::fmt::Display;

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f32),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", b),
        }
    }
}

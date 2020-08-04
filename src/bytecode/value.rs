use std::fmt::Display;

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f32),
    String(usize),
    Object(usize),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Object(pointer) => write!(f, "<obj:{}>", pointer),
            Value::String(pointer) => write!(f, "<string:{}>", pointer),
        }
    }
}

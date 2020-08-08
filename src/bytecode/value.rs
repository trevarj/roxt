use crate::object::Object;
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

impl PartialEq<bool> for Value {
    fn eq(&self, other: &bool) -> bool {
        match self {
            Value::Nil => false == *other,
            Value::Bool(b) => b == other,
            Value::Number(n) => (*n > 0.) == *other,
            Value::String(_) => true == *other,
            Value::Object(_) => true == *other,
        }
    }
}

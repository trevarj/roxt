use super::super::environment::Spaghetti;
use super::Stmt;
use std::fmt::Display;
#[derive(Clone, Debug, PartialEq)]
pub enum Atom {
    Nil,
    Number(f32),
    String(String),
    Boolean(bool),
    Identifier(String, Scope),
    Function(String, Vec<String>, Box<Stmt>, Spaghetti),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Scope {
    Global,
    Distance(usize),
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Nil => write!(f, "Nil"),
            Atom::Number(n) => write!(f, "{}", n),
            Atom::String(s) => write!(f, "{}", s),
            Atom::Boolean(b) => write!(f, "{}", b),
            Atom::Identifier(id, _) => write!(f, "{}", id),
            Atom::Function(id, _, _, _) => write!(f, "<function: {}>", id),
        }
    }
}

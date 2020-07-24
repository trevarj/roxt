use std::fmt::Display;
#[derive(Clone, Debug, PartialEq)]
pub enum Atom {
    Nil,
    Number(f32),
    String(String),
    Boolean(bool),
    Identifier(String),
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Nil => write!(f, "Nil"),
            Atom::Number(n) => write!(f, "{}", n),
            Atom::String(s) => write!(f, "{}", s),
            Atom::Boolean(b) => write!(f, "{}", b),
            Atom::Identifier(id) => write!(f, "{}", id),
        }
    }
}

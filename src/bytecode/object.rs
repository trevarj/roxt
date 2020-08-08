use crate::chunk::Chunk;
use std::fmt::Display;

#[derive(Debug)]
pub enum Object {
    String(String),
    Function(Function),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", s),
            Object::Function(fun) => write!(f, "{}", fun),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    arity: usize,
    chunk: Chunk,
}

impl Function {
    pub fn new(name: String, arity: usize, chunk: Chunk) -> Function {
        Function { name, arity, chunk }
    }

    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "<fn:{}>", self.name)
    }
}

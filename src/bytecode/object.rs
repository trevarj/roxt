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
    fun_type: FunctionType,
    arity: usize,
    chunk: Chunk,
}

#[derive(Debug, Copy, Clone)]
pub enum FunctionType {
    Script,
    Function,
}

impl Function {
    pub fn new(name: String, fun_type: FunctionType, arity: usize, chunk: Chunk) -> Function {
        Function {
            name,
            fun_type,
            arity,
            chunk,
        }
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    pub fn fun_type(&self) -> &FunctionType {
        &self.fun_type
    }

    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn inc_arity(&mut self) {
        self.arity += 1;
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

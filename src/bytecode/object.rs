use crate::{chunk::Chunk, value::Value};
use std::fmt::{Debug, Display};

pub enum Object {
    String(String),
    Function(Function),
    Native(Box<dyn Fn(usize, Vec<Value>) -> Value>),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", s),
            Object::Function(fun) => write!(f, "{}", fun),
            Object::Native(_) => write!(f, "<native fn>"),
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => std::fmt::Debug::fmt(s, f),
            Object::Function(fun) => std::fmt::Debug::fmt(fun, f),
            Object::Native(_) => write!(f, "<native fn>"),
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

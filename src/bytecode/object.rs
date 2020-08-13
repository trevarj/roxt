use crate::{chunk::Chunk, value::Value};
use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    rc::Rc,
};

// #[derive(Clone)]
pub enum Object {
    String(String),
    UpValue(UpValueObj),
    Function(Rc<Function>),
    Closure(Closure),
    Native(Box<dyn Fn(usize, Vec<Value>) -> Value>),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", s),
            Object::Function(fun) => write!(f, "{}", fun),
            Object::Closure(clo) => write!(f, "{}", clo),
            Object::Native(_) => write!(f, "<native fn>"),
            Object::UpValue(_) => write!(f, "<upvalue>"),
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => std::fmt::Debug::fmt(s, f),
            Object::Function(fun) => std::fmt::Debug::fmt(fun, f),
            Object::Closure(clo) => std::fmt::Debug::fmt(clo, f),
            Object::Native(_) => write!(f, "<native fn>"),
            Object::UpValue(_) => write!(f, "<upvalue>"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UpValueObj {
    location: *const Value,
    value: Rc<RefCell<Value>>,
}

impl UpValueObj {
    pub fn new(location: *const Value, value: Value) -> UpValueObj {
        UpValueObj {
            location,
            value: Rc::new(RefCell::new(value)),
        }
    }

    pub fn location(&self) -> *const Value {
        self.location
    }

    pub fn set_value(&mut self, value: Value) {
        *self.value.borrow_mut() = value
    }

    pub fn value(&self) -> &Rc<RefCell<Value>> {
        &self.value
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub upvalues: Vec<UpValueObj>,
    pub function: Rc<Function>,
}

impl Closure {
    pub fn new(upvalues: Vec<UpValueObj>, function: Rc<Function>) -> Closure {
        Closure { upvalues, function }
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "<closure: fn:{}>", self.function.name())
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

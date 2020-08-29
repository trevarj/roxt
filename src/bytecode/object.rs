use crate::{chunk::Chunk, value::Value};
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

// #[derive(Clone)]
pub enum Object {
    String(String),
    UpValue(UpValueObj),
    Class(ClassObj),
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
            Object::Class(class) => write!(f, "{}", class),
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
            Object::UpValue(upval) => std::fmt::Debug::fmt(upval, f),
            Object::Class(class) => std::fmt::Debug::fmt(class, f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ClassObj {
    name: String,
}

impl ClassObj {
    pub fn new(name: String) -> ClassObj {
        ClassObj { name }
    }
}

impl Display for ClassObj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "<class: {}>", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    class: Rc<ClassObj>,
    fields: HashMap<String, Value>,
}

impl Instance {
    pub fn new(class: Rc<ClassObj>) -> Instance {
        Instance {
            class,
            fields: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UpValueObj {
    pub location: *mut Value,
    pub closed: Value,
}

impl UpValueObj {
    pub fn new(location: *mut Value) -> UpValueObj {
        UpValueObj {
            location,
            closed: Value::Nil,
        }
    }

    pub fn location(&self) -> *mut Value {
        self.location
    }

    pub fn set_value(&mut self, value: Value) {
        self.closed = value
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

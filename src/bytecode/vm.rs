use super::chunk::{Chunk, OpCode};
use super::value::Value;
use thiserror::Error;

pub struct VM {
    stack: Vec<Value>,
}

#[derive(Error, Debug)]
pub enum InterpretError {
    #[error("Compile time error")]
    InterpretCompileErr,
    #[error("Runtime error")]
    InterpretRuntimeErr,
}

impl VM {
    pub fn new() -> VM {
        VM {
            stack: Vec::with_capacity(256),
        }
    }

    pub fn reset_stack(&mut self) {
        self.stack.clear()
    }

    pub fn interpret(&self, input: &str) -> Result<(), InterpretError> {
        // Compile code!
        todo!()
    }

    pub fn run(&mut self, chunk: &Chunk) -> Result<(), InterpretError> {
        Ok(for op in chunk.iter() {
            match op {
                OpCode::OpConstantLong(_) => todo!(),
                OpCode::OpConstant(const_idx) => {
                    let val = chunk.get_constant(*const_idx as usize);
                    self.stack.push(val);
                }
                OpCode::OpAdd => self.binary_op('+')?,
                OpCode::OpSubtract => self.binary_op('-')?,
                OpCode::OpMultiply => self.binary_op('*')?,
                OpCode::OpDivide => self.binary_op('/')?,
                OpCode::OpNegate => {
                    let val = self
                        .stack
                        .last_mut()
                        .ok_or(InterpretError::InterpretRuntimeErr)?;
                    *val = -*val;
                }
                OpCode::OpReturn => {
                    if let Some(val) = self.stack.pop() {
                        println!("{}", val);
                    }
                    break;
                }
            };
        })
    }

    fn binary_op(&mut self, op: char) -> Result<(), InterpretError> {
        let b = self
            .stack
            .pop()
            .ok_or(InterpretError::InterpretRuntimeErr)?;
        let a = self
            .stack
            .pop()
            .ok_or(InterpretError::InterpretRuntimeErr)?;
        Ok(match op {
            '+' => self.stack.push(a + b),
            '-' => self.stack.push(a - b),
            '*' => self.stack.push(a * b),
            '/' => self.stack.push(a / b),
            _ => panic!("Unsupported operation"),
        })
    }
}

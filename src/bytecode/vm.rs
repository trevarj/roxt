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
    #[error("Invalid Operand for negation on line {line:?}.")]
    RuntimeErrorInvalidOperandNegation { line: usize },
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
        todo!()
    }

    pub fn run(&mut self, chunk: &Chunk) -> Result<(), InterpretError> {
        Ok(for (op_idx, op) in chunk.iter().enumerate() {
            match op {
                OpCode::OpConstant(const_idx) => {
                    let val = chunk.get_constant(*const_idx as usize);
                    self.stack.push(val);
                }
                OpCode::OpBool(b) => self.stack.push(Value::Bool(*b)),
                OpCode::OpNil => self.stack.push(Value::Nil),
                OpCode::OpAdd => self.binary_op('+')?,
                OpCode::OpSubtract => self.binary_op('-')?,
                OpCode::OpMultiply => self.binary_op('*')?,
                OpCode::OpDivide => self.binary_op('/')?,

                OpCode::OpNegate => {
                    if let Value::Number(n) = self
                        .stack
                        .last_mut()
                        .ok_or(InterpretError::InterpretRuntimeErr)?
                    {
                        *n = -*n;
                    } else {
                        // operand needs to be number for negation
                        return Err(InterpretError::RuntimeErrorInvalidOperandNegation {
                            line: chunk.get_line_by_idx(op_idx),
                        });
                    }
                }
                OpCode::OpReturn => {
                    if let Some(val) = self.stack.pop() {
                        println!("{}", val);
                    }
                    break;
                }
                OpCode::OpNot => {
                    if let Value::Bool(b) = self
                        .stack
                        .last_mut()
                        .ok_or(InterpretError::InterpretRuntimeErr)?
                    {
                        *b = !*b;
                    }
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
        match (a, b) {
            (Value::Number(a), Value::Number(b)) => {
                let result = match op {
                    '+' => a + b,
                    '-' => a - b,
                    '*' => a * b,
                    '/' => a / b,
                    _ => panic!("Unsupported operation"),
                };
                self.stack.push(Value::Number(result));
            }
            _ => todo!(),
        }
        Ok(())
    }
}

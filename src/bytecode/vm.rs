use super::chunk::{Chunk, OpCode};
use super::value::Value;
use thiserror::Error;
use crate::{memory::Memory, object::Object};

pub struct VM<'mem> {
    stack: Vec<Value>,
    heap: &'mem mut Memory,
}

#[derive(Error, Debug)]
pub enum InterpretError {
    #[error("Compile time error")]
    InterpretCompileErr,
    #[error("Runtime error")]
    InterpretRuntimeErr,
    #[error("Invalid Operand for negation operation on line {line:?}.")]
    RuntimeErrorInvalidOperandNegation { line: usize },
    #[error("Invalid Operand for not operation on line {line:?}.")]
    RuntimeErrorInvalidOperandNot { line: usize },
    #[error("Unsupported operation on booleans, {lhs:?} {op:?} {rhs:?}")]
    RuntimeErrorUnsupportBinaryOperation {
        lhs: String,
        rhs: String,
        op: OpCode,
        line: usize,
    },
}

impl<'mem> VM<'_> {
    pub fn new(memory: &'mem mut Memory) -> VM {
        VM {
            stack: Vec::with_capacity(128),
            heap: memory
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
                // PRIMITIVES
                OpCode::OpConstant(const_idx) => {
                    let val = chunk.get_constant(*const_idx as usize);
                    self.stack.push(val);
                }
                OpCode::OpBool(b) => self.stack.push(Value::Bool(*b)),
                OpCode::OpNil => self.stack.push(Value::Nil),

                // BINARY OPERATIONS
                OpCode::OpAdd
                | OpCode::OpSubtract
                | OpCode::OpMultiply
                | OpCode::OpDivide
                | OpCode::OpLess
                | OpCode::OpLessEqual
                | OpCode::OpGreater
                | OpCode::OpGreatEqual
                | OpCode::OpNotEqual
                | OpCode::OpEqual => self.binary_op(op, chunk.get_line_by_idx(op_idx))?,

                // UNARY OPERATIONS
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
                OpCode::OpNot => {
                    if let Value::Bool(b) = self
                        .stack
                        .last_mut()
                        .ok_or(InterpretError::InterpretRuntimeErr)?
                    {
                        *b = !*b;
                    } else {
                        // operand needs to be boolean for not
                        return Err(InterpretError::RuntimeErrorInvalidOperandNot {
                            line: chunk.get_line_by_idx(op_idx),
                        });
                    }
                }

                OpCode::OpReturn => {
                    if let Some(val) = self.stack.pop() {
                        match val {
                            Value::String(ptr) => {
                                let str = self.heap.get_object_pointer(ptr);
                                println!("{}", str)
                            },
                            _ => println!("{}", val)
                        }
                    }
                    break;
                }
            };
        })
    }

    fn binary_op(&mut self, op: &OpCode, line: usize) -> Result<(), InterpretError> {
        let b = &self
            .stack
            .pop()
            .ok_or(InterpretError::InterpretRuntimeErr)?;
        let a = &self
            .stack
            .pop()
            .ok_or(InterpretError::InterpretRuntimeErr)?;
        let result = match op {
            OpCode::OpAdd => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
                (Value::String(a), Value::String(b)) => {
                    let str_a = self.heap.get_object_pointer(*a);
                    let str_b = self.heap.get_object_pointer(*b);
                    match (str_a, str_b) {
                        (Object::String(a), Object::String(b)) => {
                            let new_str = a.clone() + b;
                            let new_str_ptr = self.heap.add_object(Object::String(new_str));
                            Value::String(new_str_ptr)
                        },
                        _ => return Err(InterpretError::InterpretRuntimeErr)
                    }
                },
                _ => {
                    return Err(InterpretError::RuntimeErrorUnsupportBinaryOperation {
                        lhs: a.to_string(),
                        rhs: b.to_string(),
                        op: *op,
                        line,
                    })
                }
            },
            OpCode::OpSubtract => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Value::Number(a - b),
                _ => {
                    return Err(InterpretError::RuntimeErrorUnsupportBinaryOperation {
                        lhs: a.to_string(),
                        rhs: b.to_string(),
                        op: *op,
                        line,
                    })
                }
            },
            OpCode::OpMultiply => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Value::Number(a * b),
                _ => {
                    return Err(InterpretError::RuntimeErrorUnsupportBinaryOperation {
                        lhs: a.to_string(),
                        rhs: b.to_string(),
                        op: *op,
                        line,
                    })
                }
            },
            OpCode::OpDivide => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Value::Number(a / b),
                _ => {
                    return Err(InterpretError::RuntimeErrorUnsupportBinaryOperation {
                        lhs: a.to_string(),
                        rhs: b.to_string(),
                        op: *op,
                        line,
                    })
                }
            },
            OpCode::OpLess => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Value::Bool(a < b),
                _ => {
                    return Err(InterpretError::RuntimeErrorUnsupportBinaryOperation {
                        lhs: a.to_string(),
                        rhs: b.to_string(),
                        op: *op,
                        line,
                    })
                }
            },
            OpCode::OpLessEqual => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Value::Bool(a <= b),
                _ => {
                    return Err(InterpretError::RuntimeErrorUnsupportBinaryOperation {
                        lhs: a.to_string(),
                        rhs: b.to_string(),
                        op: *op,
                        line,
                    })
                }
            },
            OpCode::OpGreater => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Value::Bool(a > b),
                _ => {
                    return Err(InterpretError::RuntimeErrorUnsupportBinaryOperation {
                        lhs: a.to_string(),
                        rhs: b.to_string(),
                        op: *op,
                        line,
                    })
                }
            },
            OpCode::OpGreatEqual => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Value::Bool(a >= b),
                _ => {
                    return Err(InterpretError::RuntimeErrorUnsupportBinaryOperation {
                        lhs: a.to_string(),
                        rhs: b.to_string(),
                        op: *op,
                        line,
                    })
                }
            },
            OpCode::OpEqual => match (a, b) {
                (Value::String(a), Value::String(b)) => {
                    let str_a = self.heap.get_object_pointer(*a);
                    let str_b = self.heap.get_object_pointer(*b);
                    match (str_a, str_b) {
                        (Object::String(a), Object::String(b)) => {
                            Value::Bool(a == b)
                        },
                        _ => return Err(InterpretError::InterpretRuntimeErr)
                    }
                },
                _ => Value::Bool(a == b),
            },
            OpCode::OpNotEqual => match (a, b) {
                (Value::String(a), Value::String(b)) => {
                    let str_a = self.heap.get_object_pointer(*a);
                    let str_b = self.heap.get_object_pointer(*b);
                    match (str_a, str_b) {
                        (Object::String(a), Object::String(b)) => {
                            Value::Bool(a != b)
                        },
                        _ => return Err(InterpretError::InterpretRuntimeErr)
                    }
                },
                _ => Value::Bool(a != b),
            },
            _ => {
                return Err(InterpretError::RuntimeErrorUnsupportBinaryOperation {
                    lhs: a.to_string(),
                    rhs: b.to_string(),
                    op: *op,
                    line,
                })
            }
        };
        self.stack.push(result);
        Ok(())
    }
}

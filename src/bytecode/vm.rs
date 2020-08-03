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
    #[error("Invalid Operand for negation operation on line {line:?}.")]
    RuntimeErrorInvalidOperandNegation { line: usize },
    #[error("Invalid Operand for not operation on line {line:?}.")]
    RuntimeErrorInvalidOperandNot { line: usize },
    #[error("Unsupported operation on numerics, {a:?} {op:?} {b:?}")]
    RuntimeErrorUnsupportNumericBinaryOperation { a: f32, op: OpCode, b: f32 },
    #[error("Unsupported operation on booleans, {a:?} {op:?} {b:?}")]
    RuntimeErrorUnsupportBooleanBinaryOperation { a: bool, op: OpCode, b: bool },
    #[error("Unsupported operation {line:?}, {child:?}.")]
    RuntimeErrorUnsupportBinaryOperation {
        child: Box<InterpretError>,
        line: usize,
    },
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
                | OpCode::OpEqual => {
                    if let Err(e) = self.binary_op(op) {
                        Err(InterpretError::RuntimeErrorUnsupportBinaryOperation {
                            child: Box::new(e),
                            line: chunk.get_line_by_idx(op_idx),
                        })?
                    }
                }

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
                        println!("hi");
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
                        println!("{}", val);
                    }
                    break;
                }
            };
        })
    }

    fn binary_op(&mut self, op: &OpCode) -> Result<(), InterpretError> {
        let b = self
            .stack
            .pop()
            .ok_or(InterpretError::InterpretRuntimeErr)?;
        let a = self
            .stack
            .pop()
            .ok_or(InterpretError::InterpretRuntimeErr)?;
        let result = match (a, b) {
            (Value::Number(a), Value::Number(b)) => match op {
                OpCode::OpAdd => Value::Number(a + b),
                OpCode::OpSubtract => Value::Number(a - b),
                OpCode::OpMultiply => Value::Number(a * b),
                OpCode::OpDivide => Value::Number(a / b),
                OpCode::OpLess => Value::Bool(a < b),
                OpCode::OpLessEqual => Value::Bool(a <= b),
                OpCode::OpGreater => Value::Bool(a > b),
                OpCode::OpGreatEqual => Value::Bool(a >= b),
                OpCode::OpEqual => Value::Bool(a == b),
                OpCode::OpNotEqual => Value::Bool(a != b),
                op => {
                    return Err(
                        InterpretError::RuntimeErrorUnsupportNumericBinaryOperation {
                            a,
                            b,
                            op: op.clone(),
                        },
                    )
                }
            },
            (Value::Bool(a), Value::Bool(b)) => match op {
                OpCode::OpEqual => Value::Bool(a == b),
                OpCode::OpNotEqual => Value::Bool(a != b),
                op => {
                    return Err(
                        InterpretError::RuntimeErrorUnsupportBooleanBinaryOperation {
                            a,
                            b,
                            op: op.clone(),
                        },
                    )
                }
            },
            _ => todo!(),
        };
        self.stack.push(result);
        Ok(())
    }
}

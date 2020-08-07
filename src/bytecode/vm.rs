use super::chunk::{Chunk, OpCode};
use super::value::Value;
use crate::{memory::Memory, object::Object};
use std::collections::HashMap;
use thiserror::Error;

pub struct VM<'mem> {
    pc: usize,
    stack: Vec<Value>,
    heap: &'mem mut Memory,
    globals: HashMap<String, Value>,
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
    #[error("Undefined variable {ident:?}.")]
    RuntimeErrorUndefinedVariable { ident: String },
}

impl<'mem> VM<'_> {
    pub fn new(memory: &'mem mut Memory) -> VM {
        VM {
            pc: 0,
            stack: Vec::with_capacity(128),
            heap: memory,
            globals: HashMap::new(),
        }
    }

    pub fn reset_stack(&mut self) {
        self.stack.clear()
    }

    pub fn interpret(&self, input: &str) -> Result<(), InterpretError> {
        todo!()
    }

    pub fn run(&mut self, chunk: &Chunk) -> Result<(), InterpretError> {
        let code = chunk.code();
        loop {
            let op = code[self.pc];
            match op {
                // PRIMITIVES
                OpCode::OpConstant(const_idx) => {
                    let val = chunk.get_constant(const_idx as usize);
                    self.stack.push(val);
                }
                OpCode::OpBool(b) => self.stack.push(Value::Bool(b)),
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
                | OpCode::OpEqual => self.binary_op(&op, chunk.get_line_by_idx(self.pc))?,

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
                            line: chunk.get_line_by_idx(self.pc),
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
                            line: chunk.get_line_by_idx(self.pc),
                        });
                    }
                }

                OpCode::OpReturn => return Ok(()),
                OpCode::OpPrint => {
                    if let Some(val) = self.stack.pop() {
                        match val {
                            Value::String(ptr) => {
                                let str = self.heap.get_object_by_ptr(ptr);
                                println!("{}", str)
                            }
                            _ => println!("{}", val),
                        }
                    }
                }
                OpCode::OpPop => {
                    self.stack.pop();
                }
                OpCode::OpDefineGlobal(var_ident_ptr) => {
                    if let Object::String(ident) = self.heap.get_object_by_ptr(var_ident_ptr) {
                        if let Some(value) = self.stack.pop() {
                            self.globals.insert(ident.to_string(), value);
                        }
                    }
                }
                OpCode::OpGetGlobal(var_ident_ptr) => {
                    if let Object::String(ident) = self.heap.get_object_by_ptr(var_ident_ptr) {
                        if let Some(value) = self.globals.get(ident).copied() {
                            self.stack.push(value);
                        } else {
                            return Err(InterpretError::RuntimeErrorUndefinedVariable {
                                ident: ident.clone(),
                            });
                        }
                    }
                }
                OpCode::OpSetGlobal(var_ident_ptr) => {
                    if let Object::String(ident) = self.heap.get_object_by_ptr(var_ident_ptr) {
                        if let Some(new_val) = self.stack.last().copied() {
                            if let Some(curr_val) = self.globals.get_mut(ident) {
                                *curr_val = new_val;
                            } else {
                                return Err(InterpretError::RuntimeErrorUndefinedVariable {
                                    ident: ident.clone(),
                                });
                            }
                        }
                    }
                }
                OpCode::OpGetLocal(slot) => {
                    // get the variable at slot
                    let value = self.stack.get(slot).copied().unwrap();
                    // push value at top of stack
                    self.stack.push(value);
                }
                OpCode::OpSetLocal(slot) => {
                    // peek at the top of stack
                    let peeked = self.stack.last().copied().unwrap();
                    // get value at slot
                    let value = self.stack.get_mut(slot).unwrap();
                    // update value at slot
                    *value = peeked;
                }
                // the continues for jump/loop avoid the increment of the pc
                OpCode::OpJumpIfFalse(offset) => {
                    if let Some(condition) = self.stack.last() {
                        if *condition == false {
                            self.pc += offset;
                            continue;
                        }
                    }
                }
                OpCode::OpJump(offset) => {
                    self.pc += offset;
                    continue;
                }
                OpCode::OpLoop(offset) => {
                    self.pc -= offset;
                    continue;
                }
            };
            self.pc += 1;
        }
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
                    let str_a = self.heap.get_object_by_ptr(*a);
                    let str_b = self.heap.get_object_by_ptr(*b);
                    match (str_a, str_b) {
                        (Object::String(a), Object::String(b)) => {
                            let new_str = a.clone() + b;
                            let new_str_ptr = self.heap.add_object(Object::String(new_str));
                            Value::String(new_str_ptr)
                        }
                        _ => return Err(InterpretError::InterpretRuntimeErr),
                    }
                }
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
                    let str_a = self.heap.get_object_by_ptr(*a);
                    let str_b = self.heap.get_object_by_ptr(*b);
                    match (str_a, str_b) {
                        (Object::String(a), Object::String(b)) => Value::Bool(a == b),
                        // _ => return Err(InterpretError::InterpretRuntimeErr),
                    }
                }
                _ => Value::Bool(a == b),
            },
            OpCode::OpNotEqual => match (a, b) {
                (Value::String(a), Value::String(b)) => {
                    let str_a = self.heap.get_object_by_ptr(*a);
                    let str_b = self.heap.get_object_by_ptr(*b);
                    match (str_a, str_b) {
                        (Object::String(a), Object::String(b)) => Value::Bool(a != b),
                        // _ => return Err(InterpretError::InterpretRuntimeErr),
                    }
                }
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

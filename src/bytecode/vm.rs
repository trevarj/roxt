use super::chunk::{Chunk, OpCode};
use super::value::Value;
use crate::{
    compiler::Compiler,
    memory::Memory,
    object::{Function, Object},
};
use std::collections::HashMap;
use thiserror::Error;

pub struct VM<'mem> {
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    heap: &'mem mut Memory,
    globals: HashMap<String, Value>,
}

#[derive(Debug)]
struct CallFrame {
    pc: usize,
    stack_base: usize,
    function: Function,
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

impl<'mem> VM<'mem> {
    pub fn new(memory: &'mem mut Memory) -> VM {
        VM {
            frames: Vec::with_capacity(128),
            stack: Vec::with_capacity(128),
            heap: memory,
            globals: HashMap::new(),
        }
    }

    fn current_frame(&self) -> Result<&CallFrame, InterpretError> {
        self.frames
            .last()
            .ok_or(InterpretError::InterpretRuntimeErr)
    }

    fn current_frame_mut(&mut self) -> Result<&mut CallFrame, InterpretError> {
        self.frames
            .last_mut()
            .ok_or(InterpretError::InterpretRuntimeErr)
    }

    pub fn reset_stack(&mut self) {
        self.stack.clear();
        self.frames.clear();
    }

    pub fn interpret(&mut self, input: &str) -> Result<(), InterpretError> {
        let function = Function::new("main".to_string(), 0, Chunk::new("main".to_string()));
        let mut compiler = Compiler::new(input, self.heap, function);

        match compiler.compile() {
            Ok(fun) => {
                println!("{}", fun.chunk());
                self.frames.push(CallFrame {
                    function: fun,
                    pc: 0,
                    stack_base: 0,
                });
                self.run()
            }
            Err(err) => {
                eprintln!("{}", err);
                Err(InterpretError::InterpretCompileErr)
            }
        }
    }

    fn run(&mut self) -> Result<(), InterpretError> {
        loop {
            let op = if let Some(op) = self
                .current_frame()?
                .function
                .chunk()
                .code()
                .get(self.current_frame()?.pc)
                .copied()
            {
                op
            } else {
                // program done
                return Ok(());
            };
            match op {
                // PRIMITIVES
                OpCode::OpConstant(const_idx) => {
                    let val = self
                        .current_frame()?
                        .function
                        .chunk()
                        .get_constant(const_idx as usize);
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
                | OpCode::OpEqual => self.binary_op(
                    &op,
                    self.current_frame()?
                        .function
                        .chunk()
                        .get_line_by_idx(self.current_frame()?.pc),
                )?,

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
                            line: self
                                .current_frame()?
                                .function
                                .chunk()
                                .get_line_by_idx(self.current_frame()?.pc),
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
                            line: self
                                .current_frame()?
                                .function
                                .chunk()
                                .get_line_by_idx(self.current_frame()?.pc),
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
                    // calculate index within frame
                    let index = self.current_frame()?.stack_base + slot;
                    // get the variable at slot
                    // push value at top of frame's stack (relative position in vm stack)
                    self.stack.push(self.stack[index]);
                }
                OpCode::OpSetLocal(slot) => {
                    let index = self.current_frame()?.stack_base + slot;
                    // peek at the top of stack
                    let peeked = self.stack.last().unwrap();
                    // get value at slot and update
                    self.stack[index] = *peeked;
                }
                // the continues for jump/loop avoid the increment of the pc
                OpCode::OpJumpIfFalse(offset) => {
                    if let Some(condition) = self.stack.last() {
                        if *condition == false {
                            self.current_frame_mut()?.pc += offset;
                            continue;
                        }
                    }
                }
                OpCode::OpJump(offset) => {
                    self.current_frame_mut()?.pc += offset;
                    continue;
                }
                OpCode::OpLoop(offset) => {
                    self.current_frame_mut()?.pc -= offset;
                    continue;
                }
            };
            self.current_frame_mut()?.pc += 1;
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
                        _ => return Err(InterpretError::InterpretRuntimeErr),
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
                        _ => return Err(InterpretError::InterpretRuntimeErr),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_expressions() {
        let mut mem = Memory::new();
        let mut vm = VM::new(&mut mem);

        let input = r#"
        var i = 0;
        while (i < 3) {
            print i;
            i = i + 1;
        }

        for ( var j = 1; j < 5; j = j + 1) {
            print j;
        }
        "#;
        vm.interpret(&input).unwrap();
    }
}

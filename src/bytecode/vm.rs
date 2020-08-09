use super::chunk::{Chunk, OpCode};
use super::value::Value;
use crate::{
    compiler::Compiler,
    memory::Memory,
    object::{Function, FunctionType, Object},
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
    #[error("Function {ident:?} requires {args:?} arguments.")]
    RuntimeErrorFunctionArity { ident: String, args: usize },
    #[error("Stack Overflow.")]
    RuntimeErrorStackOverflow,
    #[error("Attempted to call function on non-callable type - {ttype:?}.")]
    RuntimeErrorNonCallableType { ttype: String },
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
        let function = Function::new(
            "main".to_string(),
            FunctionType::Script,
            0,
            Chunk::new("main".to_string()),
        );
        let mut compiler = Compiler::new(input, self.heap, function);

        match compiler.compile() {
            Ok(fun) => {
                println!("{}", fun.chunk());
                let func_ptr = self.heap.add_object(Object::Function(fun));
                self.stack.push(Value::Object(func_ptr));
                self.call_value(Value::Object(func_ptr), 0)?;
                if let Err(err) = self.run() {
                    eprintln!("{}", err);
                    for (count, cf) in self.frames.iter().enumerate() {
                        let line = cf.function.chunk().get_line_by_idx(cf.pc);
                        let func_name = cf.function.name();
                        eprintln!("{:1$}{} {}():{}", "", count, func_name, line);
                    }
                    Ok(())
                } else {
                    Ok(())
                }
            }
            Err(err) => {
                eprintln!("{}", err);
                // Err(InterpretError::InterpretCompileErr)
                Ok(())
            }
        }
    }

    fn run(&mut self) -> Result<(), InterpretError> {
        loop {
            // println!("stack: {:?}", self.stack);
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
            // println!("op: {:?}", op);
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

                OpCode::OpReturn => {
                    let result = self.stack.pop().unwrap();
                    let old_frame = self.frames.pop().unwrap();
                    if self.frames.len() == 0 {
                        self.stack.pop();
                        return Ok(());
                    }
                    let old_stack_base = old_frame.stack_base;
                    // drop all the callee's params/locals
                    self.stack.drain(old_stack_base - 1..);
                    self.stack.push(result)
                }
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
                OpCode::OpCall(arg_count) => {
                    let callee_idx = self.stack.len() - 1 - arg_count;
                    let callee = self.stack[callee_idx];
                    self.call_value(callee, arg_count)?;
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
                (Value::Object(a), Value::String(b)) => {
                    let str_a = self.heap.get_object_by_ptr(*a);
                    let str_b = self.heap.get_object_by_ptr(*b);
                    println!("a {:?} b {:?}", str_a, str_b);
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

    fn call_value(&mut self, callee: Value, arg_count: usize) -> Result<(), InterpretError> {
        // println!("val {:?}", callee);
        if let Value::Object(ptr) = callee {
            let callable = self.heap.get_object_by_ptr(ptr);
            // println!("heap {:?}", self.heap);
            if let Object::Function(fun) = callable {
                // call function
                // println!("calling {}", fun.chunk());
                let function = fun.clone();
                self.call(function, arg_count)?;
            } else {
                return Err(InterpretError::RuntimeErrorNonCallableType {
                    ttype: callable.to_string(),
                });
            }
        } else {
            return Err(InterpretError::RuntimeErrorNonCallableType {
                ttype: callee.to_string(),
            });
        }
        Ok(())
    }

    fn call(&mut self, function: Function, arg_count: usize) -> Result<(), InterpretError> {
        if arg_count != function.arity() {
            return Err(InterpretError::RuntimeErrorFunctionArity {
                ident: function.name().to_string(),
                args: function.arity(),
            });
        }

        if self.frames.len() == 256 {
            return Err(InterpretError::RuntimeErrorStackOverflow);
        }

        // create new callframe
        self.frames.push(CallFrame {
            function,
            pc: 0,
            stack_base: self.stack.len() - arg_count,
        });
        // println!("frames: {:?}", self.frames);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::*;

    #[test]
    fn test_basic() {
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

    #[test]
    fn test_function_call() {
        let mut mem = Memory::new();
        let mut vm = VM::new(&mut mem);

        let input = r#"
        fun sum(a, b) {
            print a + b;
        }
        sum(1, 2);

        fun gimme() {
            return "5 bucks";
        }
        print "here is " + gimme() + "!";
        "#;
        vm.interpret(&input).unwrap();
    }

    #[test]
    fn test_fib() {
        let mut mem = Memory::new();
        let mut vm = VM::new(&mut mem);

        let input = r#"
        fun fib(n) {
            if (n < 2) return n;
            return fib(n - 2) + fib(n - 1);
        }
        print fib(30);
        "#;
        let start = Instant::now();
        vm.interpret(&input).unwrap();
        let end = Instant::now().checked_duration_since(start).unwrap();
        println!("duration: {}", end.as_secs());
    }
}

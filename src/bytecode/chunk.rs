use crate::{
    compiler::UpValue,
    memory::Memory,
    object::{Function, Object},
    value::Value,
};

#[derive(Debug, Clone)]
pub enum OpCode {
    OpConstant(u16),
    OpBool(bool),
    OpNil,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNot,
    OpNegate,
    OpLess,
    OpLessEqual,
    OpGreater,
    OpGreatEqual,
    OpEqual,
    OpNotEqual,
    OpPrint,
    OpPop,
    OpDefineGlobal(usize),
    OpGetGlobal(usize),
    OpSetGlobal(usize),
    OpGetLocal(usize),
    OpSetLocal(usize),
    OpGetUpValue(usize),
    OpSetUpValue(usize),
    OpCloseUpValue,
    OpJumpIfFalse(usize),
    OpJump(usize),
    OpLoop(usize),
    OpCall(usize),
    OpClosure(usize, Vec<UpValue>),
    OpReturn,
}

/// A Chunk of code that will be sent to the VM
#[derive(Debug, Clone, Default)]
pub struct Chunk {
    name: String,
    code: Vec<OpCode>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new(name: String) -> Chunk {
        Chunk {
            name,
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn get_line_by_idx(&self, index: usize) -> usize {
        self.lines.get(index).copied().unwrap_or(0)
    }

    pub fn iter(&self) -> std::slice::Iter<OpCode> {
        self.code.iter()
    }

    pub fn code(&self) -> &Vec<OpCode> {
        &self.code
    }

    pub fn set_code(&mut self, offset: usize, new: OpCode) {
        if let Some(old) = self.code.get_mut(offset) {
            *old = new;
        }
    }

    /// Writes OpCode to Chunk
    pub fn write(&mut self, op: OpCode, line: usize) {
        self.code.push(op);
        self.lines.push(line);
    }

    /// Adds constant value to constant pool
    /// Returns index
    pub fn add_constant(&mut self, constant: Value) -> usize {
        self.constants.push(constant);
        self.constants.len() - 1
    }

    pub fn get_constant(&self, const_idx: usize) -> Value {
        *self.constants.get(const_idx).unwrap()
    }

    pub fn dump(&self, memory: &Memory) {
        println!("line {:=<16}[{}]{:=>16}", "", self.name, "");
        let mut functions: Vec<&Function> = Vec::new();
        for (op, line) in self.code.iter().zip(&self.lines) {
            match op {
                OpCode::OpConstant(const_idx) => {
                    // constant values known at compile time, safe to unwrap.
                    let constant = self.constants.get(*const_idx as usize).unwrap();
                    let mut const_str = constant.to_string();
                    if let Value::String(idx) = constant {
                        const_str = memory.get_object_by_ptr(*idx).to_string();
                    }
                    println!(
                        "{:<4} {:<20} {:<4} {}",
                        line, "OP_CONSTANT", const_idx, const_str
                    )
                }
                OpCode::OpBool(b) => println!("{:<4} {:<20} {:<4} {:<10}", line, b, "OP_BOOL", ""),
                OpCode::OpNil => println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_NIL", "nil", ""),
                OpCode::OpAdd => println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_ADD", "", ""),
                OpCode::OpSubtract => {
                    println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_SUBTRACT", "", "")
                }
                OpCode::OpMultiply => {
                    println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_MULTIPLY", "", "")
                }
                OpCode::OpDivide => {
                    println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_DIVIDE", "", "")
                }
                OpCode::OpNegate => {
                    println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_NEGATE", "", "")
                }
                OpCode::OpLess => println!("{:<4}{:<20} {:<4}  {:<10}", line, "OP_LESS", "", ""),
                OpCode::OpLessEqual => {
                    println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_LESSEQ", "", "")
                }
                OpCode::OpGreater => {
                    println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_GREATER", "", "")
                }
                OpCode::OpGreatEqual => {
                    println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_GREATEREQ", "", "")
                }
                OpCode::OpEqual => println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_EQUAL", "", ""),
                OpCode::OpNotEqual => {
                    println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_NOTEQUAL", "", "")
                }
                OpCode::OpReturn => {
                    println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_RETURN", "", "")
                }
                OpCode::OpNot => println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_NOT", "", ""),
                OpCode::OpPrint => println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_PRINT", "", ""),
                OpCode::OpPop => println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_POP", "", ""),
                OpCode::OpDefineGlobal(var_ident_ptr) => println!(
                    "{:<4} {:<20} {:<4} {:>10}",
                    line,
                    "OP_DEFINE_GLOBAL",
                    var_ident_ptr,
                    memory.get_object_by_ptr(*var_ident_ptr)
                ),
                OpCode::OpGetGlobal(var_ident_ptr) => println!(
                    "{:<4} {:<20} {:<4} {:>10}",
                    line,
                    "OP_GET_GLOBAL",
                    var_ident_ptr,
                    memory.get_object_by_ptr(*var_ident_ptr)
                ),
                OpCode::OpSetGlobal(var_ident_ptr) => println!(
                    "{:<4} {:<20} {:<4} {:<10}",
                    line,
                    "OP_SET_GLOBAL",
                    var_ident_ptr,
                    memory.get_object_by_ptr(*var_ident_ptr)
                ),
                OpCode::OpGetLocal(stack_ptr) => println!(
                    "{:<4} {:<20} {:<4} {:<10}",
                    line, "OP_GET_LOCAL", stack_ptr, ""
                ),
                OpCode::OpSetLocal(stack_ptr) => println!(
                    "{:<4} {:<20} {:<4} {:<10}",
                    line, "OP_SET_LOCAL", stack_ptr, ""
                ),
                OpCode::OpJumpIfFalse(offset) => println!(
                    "{:<4} {:<20} {:<4} {:<10}",
                    line, "OP_JUMP_FALSE", offset, ""
                ),
                OpCode::OpJump(offset) => {
                    println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_JUMP", offset, "")
                }
                OpCode::OpLoop(offset) => {
                    println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_LOOP", offset, "")
                }
                OpCode::OpCall(arg_count) => {
                    println!("{:<4} {:<20} {:<4} {:<10}", line, "OP_CALL", arg_count, "")
                }
                OpCode::OpClosure(fun_ptr, upvalues) => {
                    let fun_val = self.get_constant(*fun_ptr);
                    if let Value::Object(idx) = fun_val {
                        let function = memory.get_object_by_ptr(idx);
                        if let Object::Function(fun) = function {
                            functions.push(&fun);
                            println!(
                                "{:<4} {:<20} {:<4} {:<?}",
                                line, "OP_CLOSURE", fun_ptr, upvalues,
                            )
                        }
                    }
                }
                OpCode::OpGetUpValue(upval_ptr) => println!(
                    "{:<4} {:<20} {:<4} {:<10}",
                    line, "OP_GET_UPVALUE", upval_ptr, ""
                ),
                OpCode::OpSetUpValue(upval_ptr) => println!(
                    "{:<4} {:<20} {:<4} {:<10}",
                    line, "OP_SET_UPVALUE", upval_ptr, ""
                ),
                OpCode::OpCloseUpValue => println!(
                    "{:<4} {:<20} {:<4} {:<10}",
                    line, "OP_CLOSE_UPVALUE", "", ""
                ),
            }
        }

        for f in functions {
            f.chunk().dump(memory)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;
    #[test]
    fn simple_opcode_print() {
        let mut c = Chunk::new("test chunk".to_string());
        c.write(OpCode::OpReturn, 123);
        let idx = c.add_constant(Value::Number(1.2));
        c.write(OpCode::OpConstant(idx as u16), 123);
        let idx = c.add_constant(Value::Number(55.5));
        c.write(OpCode::OpConstant(idx as u16), 123);
    }

    #[test]
    fn test_size_of_ops() {
        println!("{}", size_of::<OpCode>());
    }
}

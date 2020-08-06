use crate::value::Value;
use std::fmt::Display;

#[derive(Debug, Copy, Clone)]
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
    OpJumpIfFalse(usize),
    OpJump(usize),
    OpLoop(usize),
    OpReturn,
}

/// A Chunk of code that will be sent to the VM
#[derive(Debug)]
pub struct Chunk {
    name: String,
    code: Vec<OpCode>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "==== {} ====", self.name)?;
        for (op, line) in self.code.iter().zip(&self.lines) {
            match op {
                OpCode::OpConstant(const_idx) => {
                    // constant values known at compile time, safe to unwrap.
                    writeln!(
                        f,
                        "{:<4} line:{:<4} {:<10} val:{}",
                        const_idx,
                        line,
                        "OP_CONSTANT",
                        self.constants.get(*const_idx as usize).unwrap()
                    )?
                }
                OpCode::OpBool(b) => {
                    writeln!(f, "{:<4} line:{:<4} {:<10} {:^10}", b, line, "OP_BOOL", "")?
                }
                OpCode::OpNil => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    "nil", line, "OP_NIL", ""
                )?,
                OpCode::OpAdd => writeln!(f, "{:<4} {:<4} {:<10} {:^10}", "", line, "OP_ADD", "")?,
                OpCode::OpSubtract => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    "", line, "OP_SUBTRACT", ""
                )?,
                OpCode::OpMultiply => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    "", line, "OP_MULTIPLY", ""
                )?,
                OpCode::OpDivide => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    "", line, "OP_DIVIDE", ""
                )?,
                OpCode::OpNegate => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    "", line, "OP_NEGATE", ""
                )?,
                OpCode::OpLess => writeln!(f, "{:<4} {:<4} {:<10} {:^10}", "", line, "OP_LT", "")?,
                OpCode::OpLessEqual => {
                    writeln!(f, "{:<4} line:{:<4} {:<10} {:^10}", "", line, "OP_LTEQ", "")?
                }
                OpCode::OpGreater => {
                    writeln!(f, "{:<4} line:{:<4} {:<10} {:^10}", "", line, "OP_GT", "")?
                }
                OpCode::OpGreatEqual => {
                    writeln!(f, "{:<4} line:{:<4} {:<10} {:^10}", "", line, "OP_GTEQ", "")?
                }
                OpCode::OpEqual => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    "", line, "OP_EQUAL", ""
                )?,
                OpCode::OpNotEqual => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    "", line, "OP_NOTEQ", ""
                )?,
                OpCode::OpReturn => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    "", line, "OP_RETURN", ""
                )?,
                OpCode::OpNot => {
                    writeln!(f, "{:<4} line:{:<4} {:<10} {:^10}", "", line, "OP_NOT", "")?
                }
                OpCode::OpPrint => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    "", line, "OP_PRINT", ""
                )?,
                OpCode::OpPop => {
                    writeln!(f, "{:<4} line:{:<4} {:<10} {:^10}", "", line, "OP_POP", "")?
                }
                OpCode::OpDefineGlobal(var_ident_ptr) => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    var_ident_ptr, line, "OP_DEFINE_GLOBAL", ""
                )?,
                OpCode::OpGetGlobal(var_ident_ptr) => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    var_ident_ptr, line, "OP_GET_GLOBAL", ""
                )?,
                OpCode::OpSetGlobal(var_ident_ptr) => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    var_ident_ptr, line, "OP_SET_GLOBAL", ""
                )?,
                OpCode::OpGetLocal(stack_ptr) => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    stack_ptr, line, "OP_GET_LOCAL", ""
                )?,
                OpCode::OpSetLocal(stack_ptr) => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    stack_ptr, line, "OP_SET_LOCAL", ""
                )?,
                OpCode::OpJumpIfFalse(offset) => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    offset, line, "OP_JMP_IF_FALSE", ""
                )?,
                OpCode::OpJump(offset) => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    offset, line, "OP_JMP", ""
                )?,
                OpCode::OpLoop(offset) => writeln!(
                    f,
                    "{:<4} line:{:<4} {:<10} {:^10}",
                    offset, line, "OP_LOOP", ""
                )?,
            }
        }
        Ok(())
    }
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
        println!("{}", c.to_string());
    }

    #[test]
    fn test_size_of_ops() {
        println!("{}", size_of::<OpCode>());
    }
}

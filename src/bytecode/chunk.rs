use super::value::Value;
use std::fmt::Display;

#[derive(Debug)]
pub enum OpCode {
    OpConstantLong(u32),
    OpConstant(u8),
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNegate,
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
                OpCode::OpConstantLong(const_idx) => writeln!(
                    f,
                    "{:<4} {:<4} {:^10} {}",
                    const_idx,
                    line,
                    "OP_CONSTANT_LONG",
                    self.constants.get(*const_idx as usize).unwrap()
                )?,

                OpCode::OpConstant(const_idx) => {
                    // constant values known at compile time, safe to unwrap.
                    writeln!(
                        f,
                        "{:<4} {:<4} {:^10} {}",
                        const_idx,
                        line,
                        "OP_CONSTANT",
                        self.constants.get(*const_idx as usize).unwrap()
                    )?
                }
                OpCode::OpAdd => writeln!(f, "{:<4} {:<4} {:^10} {:^10}", "", line, "OP_ADD", "")?,
                OpCode::OpSubtract => {
                    writeln!(f, "{:<4} {:<4} {:^10} {:^10}", "", line, "OP_SUBTRACT", "")?
                }
                OpCode::OpMultiply => {
                    writeln!(f, "{:<4} {:<4} {:^10} {:^10}", "", line, "OP_MULTIPLY", "")?
                }
                OpCode::OpDivide => {
                    writeln!(f, "{:<4} {:<4} {:^10} {:^10}", "", line, "OP_DIVIDE", "")?
                }
                OpCode::OpNegate => {
                    writeln!(f, "{:<4} {:<4} {:^10} {:^10}", "", line, "OP_NEGATE", "")?
                }
                OpCode::OpReturn => {
                    writeln!(f, "{:<4} {:<4} {:^10} {:^10}", "", line, "OP_RETURN", "")?
                }
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

    pub fn iter(&self) -> std::slice::Iter<OpCode> {
        self.code.iter()
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

    #[test]
    fn simple_opcode_print() {
        let mut c = Chunk::new("test chunk".to_string());
        c.write(OpCode::OpReturn, 123);
        let idx = c.add_constant(1.2);
        c.write(OpCode::OpConstant(idx as u8), 123);
        let idx = c.add_constant(55.5);
        c.write(OpCode::OpConstant(idx as u8), 123);
        println!("{}", c.to_string());
    }
}

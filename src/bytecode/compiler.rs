use crate::{
    chunk::{Chunk, OpCode},
    lexer::{Lexer, Token, TokenType},
    value::Value,
};
use std::{iter::Peekable, str::CharIndices};
use thiserror::Error;

#[derive(Debug, Error)]
enum CompilerError {
    #[error("Max constants in chunk.")]
    ChunkMaxConstants,
    #[error("Expected {expect:?}, found {found:?} on line {line:?}.")]
    ParserExpectedTokenError {
        expect: String,
        found: String,
        line: usize,
    },
    #[error("Expected literal value, found {found:?} on line {line:?}.")]
    ParserUnexpectedValue { found: String, line: usize },
    #[error("Unexpected operator, found {found:?} on line {line:?}.")]
    ParserUnexpectedOperator { found: String, line: usize },
    #[error("Unable to parse number, found {found:?} on line {line:?}.")]
    ParserFloatParseError { found: String, line: usize },
}
pub struct Compiler<'ch, 'input> {
    lexer: Lexer<'input>,
    peeked: Option<Token<'input>>,
    panic_mode: bool,
    current_chunk: &'ch mut Chunk,
}

impl<'ch, 'input> Compiler<'ch, 'input> {
    pub fn new(chunk: &'ch mut Chunk, source: &'input str) -> Compiler<'ch, 'input> {
        Compiler {
            lexer: Lexer::new(source),
            peeked: None,
            panic_mode: false,
            current_chunk: chunk,
        }
    }

    pub fn compile(&self) {}

    fn emit_opcode(&mut self, op: OpCode, line: usize) {
        self.current_chunk.write(op, line);
    }

    fn emit_constant(&mut self, value: Value, line: usize) -> Result<(), CompilerError> {
        let idx = self.current_chunk.add_constant(value);
        if idx > std::u16::MAX as usize {
            return Err(CompilerError::ChunkMaxConstants);
        }
        self.emit_opcode(OpCode::OpConstant(idx as u16), line);
        Ok(())
    }

    fn emit_return(&mut self, line: usize) {
        self.emit_opcode(OpCode::OpReturn, line)
    }

    fn advance(&mut self) -> Token<'input> {
        match self.lexer.scan_token() {
            Ok(token) => token,
            Err(err) => {
                eprintln!("{}", err);
                self.panic_mode = true;
                loop {
                    match self.lexer.scan_token() {
                        Ok(token) => return token,
                        Err(err) => eprintln!("{}", err),
                    }
                }
            }
        }
    }

    fn next(&mut self) -> Token {
        match self.peeked.take() {
            Some(token) => token,
            None => self.advance(),
        }
    }

    fn peek(&mut self) -> &Token<'input> {
        if self.peeked.is_none() {
            self.peeked = Some(self.advance());
            self.peeked.as_ref().unwrap()
        } else {
            self.peeked.as_ref().unwrap()
        }
    }

    fn expect(&mut self, type_: TokenType) -> Result<Token, CompilerError> {
        let next = self.next();
        if type_ != next.ttype() {
            return Err(CompilerError::ParserExpectedTokenError {
                found: next.lexeme().to_string(),
                expect: type_.to_string(),
                line: next.line(),
            });
        }
        Ok(next)
    }

    fn expr(&mut self, min_bp: u8) -> Result<(), CompilerError> {
        self.unary()?;

        loop {
            let token = self.peek();
            let current_line = token.line();
            let operator = match token.ttype() {
                TokenType::EOF => break,
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Star
                | TokenType::Slash
                | TokenType::EqualEqual
                | TokenType::BangEqual
                | TokenType::Less
                | TokenType::LessEqual
                | TokenType::Greater
                | TokenType::GreaterEqual => token.ttype(),
                _ => {
                    eprintln!(
                        "{}",
                        CompilerError::ParserUnexpectedOperator {
                            found: token.lexeme().to_string(),
                            line: token.line(),
                        }
                    );
                    break;
                }
            };

            if let Some((left_bp, right_bp)) = self.infix_binding_power(&operator) {
                if left_bp < min_bp {
                    // stop parsing this expression
                    break;
                }
                // consume operator and get next operand
                self.next();
                // calculate right hand side using the right bp as the new leftmost bp
                self.expr(right_bp)?;
                // push binary op onto chunk stack
                self.binary(&operator, current_line);

                continue;
            }
        }
        Ok(())
    }

    fn prefix_binding_power(&mut self) -> Option<((), u8)> {
        match self.peek().ttype() {
            TokenType::Minus | TokenType::Bang => Some(((), 5)),
            _ => None,
        }
    }

    fn infix_binding_power(&self, operator: &TokenType) -> Option<(u8, u8)> {
        match operator {
            TokenType::EqualEqual
            | TokenType::BangEqual
            | TokenType::Less
            | TokenType::LessEqual
            | TokenType::Greater
            | TokenType::GreaterEqual => Some((2, 3)),
            TokenType::Plus | TokenType::Minus => Some((4, 5)),
            TokenType::Star | TokenType::Slash => Some((6, 7)),
            _ => None,
        }
    }

    fn binary(&mut self, operator: &TokenType, line: usize) {
        match operator {
            TokenType::Plus => self.emit_opcode(OpCode::OpAdd, line),
            TokenType::Minus => self.emit_opcode(OpCode::OpSubtract, line),
            TokenType::Star => self.emit_opcode(OpCode::OpMultiply, line),
            TokenType::Slash => self.emit_opcode(OpCode::OpDivide, line),
            TokenType::Less => self.emit_opcode(OpCode::OpLess, line),
            TokenType::LessEqual => self.emit_opcode(OpCode::OpLessEqual, line),
            TokenType::Greater => self.emit_opcode(OpCode::OpGreatEqual, line),
            TokenType::EqualEqual => self.emit_opcode(OpCode::OpEqual, line),
            TokenType::BangEqual => self.emit_opcode(OpCode::OpNotEqual, line),
            _ => todo!(),
        }
    }

    fn unary(&mut self) -> Result<(), CompilerError> {
        // check for unary operators
        Ok(if let Some(((), r_bp)) = self.prefix_binding_power() {
            // store operator
            let operator = self.peek().ttype();
            // store line
            let line = self.peek().line();
            // consume unary operator
            self.next();
            // evaluate unary operand
            self.expr(r_bp)?;
            match operator {
                TokenType::Minus => self.emit_opcode(OpCode::OpNegate, line),
                TokenType::Bang => self.emit_opcode(OpCode::OpNot, line),
                _ => todo!(),
            }
        } else {
            self.primary()?;
        })
    }

    fn primary(&mut self) -> Result<(), CompilerError> {
        let next = self.peek();
        match next.ttype() {
            TokenType::Number => self.number(),
            TokenType::True | TokenType::False | TokenType::Nil => self.literal(),
            TokenType::LeftParen => self.grouping(),
            _ => Err(CompilerError::ParserUnexpectedValue {
                found: next.lexeme().to_string(),
                line: next.line(),
            }),
        }
    }

    fn grouping(&mut self) -> Result<(), CompilerError> {
        // eat the left paren
        self.next();
        // isolate the expression within these parens
        self.expr(0)?;
        // expect the closing parenthesis
        self.expect(TokenType::RightParen)?;
        Ok(())
    }

    fn number(&mut self) -> Result<(), CompilerError> {
        let token = self.next();
        Ok(if let Ok(f) = token.lexeme().parse::<f32>() {
            let val = Value::Number(f);
            let line = token.line();
            self.emit_constant(val, line)?;
        } else {
            return Err(CompilerError::ParserFloatParseError {
                found: token.lexeme().to_string(),
                line: token.line(),
            });
        })
    }

    fn literal(&mut self) -> Result<(), CompilerError> {
        let token = self.next();
        let line = token.line();
        Ok(match token.ttype() {
            TokenType::True => self.emit_opcode(OpCode::OpBool(true), line),
            TokenType::False => self.emit_opcode(OpCode::OpBool(false), line),
            TokenType::Nil => self.emit_opcode(OpCode::OpNil, line),
            _ => todo!(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::VM;

    fn test_chunk() -> Chunk {
        Chunk::new("test chunk".to_string())
    }

    #[test]
    fn test_basic_expression() {
        let source = "5 < 6 - 1";
        let mut chunk = test_chunk();
        let mut c = Compiler::new(&mut chunk, source);
        c.compile();
        c.expr(0).unwrap();
        chunk.write(OpCode::OpReturn, 123);
        println!("result: {}", chunk);
        let mut vm = VM::new();
        let result = vm.run(&chunk);
        println!("{:?}", result);
    }
}

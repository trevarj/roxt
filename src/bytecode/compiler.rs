use crate::{
    chunk::{Chunk, OpCode},
    lexer::{Lexer, Token, TokenType},
    memory::Memory,
    object::Object,
    value::Value,
};
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum CompilerError {
    #[error("Compilation has failed with errors.")]
    CompilationFailedWithErrors,
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
    #[error("Local variable {found:?} already defined in current scope on line {line:?}.")]
    ParserLocalVariableAlreadyDefined { found: String, line: usize },
    #[error("Cannot use variable {found:?} inside its initilizer on line {line:?}.")]
    ParserVariableInsideOwnInitializer { found: String, line: usize },
}
pub struct Compiler<'ch, 'input> {
    /// The Lexer used to tokenize the source
    lexer: Lexer<'input>,
    /// Peeked value that is next up in the lexer
    peeked: Option<Token<'input>>,
    /// Current chunk that is being compiled
    current_chunk: &'ch mut Chunk,
    /// Heap memory
    memory: &'input mut Memory,
    /// Local variable scopes, vec of variable lexemes and their depths.
    /// Is None when variable is declared but not defined (initialized)
    locals: Vec<(String, Option<usize>)>,
    /// Current scope depth
    scope_depth: usize,
    /// Vec of errors that occured during compilation
    errors: Vec<CompilerError>,
}

impl<'ch, 'input> Compiler<'ch, 'input> {
    pub fn new(
        chunk: &'ch mut Chunk,
        source: &'input str,
        memory: &'input mut Memory,
    ) -> Compiler<'ch, 'input> {
        Compiler {
            lexer: Lexer::new(source),
            peeked: None,
            current_chunk: chunk,
            memory,
            locals: Vec::new(),
            scope_depth: 0,
            errors: Vec::new(),
        }
    }

    /// Emits an opcode to the current chunk
    fn emit_opcode(&mut self, op: OpCode, line: usize) {
        self.current_chunk.write(op, line);
    }

    /// Emits a constant value into the current chunk's constant vec
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

    fn emit_ident_const(&mut self, ident: &String, line: usize) -> Result<usize, CompilerError> {
        // put ident in memory
        let ident_ptr = self.memory.add_object(Object::String(ident.clone()));
        // create constant with ident ptr
        // self.emit_constant(Value::String(ident_ptr), line)?;
        self.current_chunk.add_constant(Value::String(ident_ptr));
        Ok(ident_ptr)
    }

    /// Advances the lexer by one token
    /// If the lexer runs into an error it continues
    /// until a "good" token is found
    fn advance(&mut self) -> Result<Token<'input>, CompilerError> {
        Ok(match self.lexer.scan_token() {
            Ok(token) => token,
            Err(err) => {
                eprintln!("{}", err);
                loop {
                    match self.lexer.scan_token() {
                        Ok(token) => return Ok(token),
                        Err(err) => eprintln!("{}", err),
                    }
                }
            }
        })
    }

    /// Gets next token from lexer
    fn next(&mut self) -> Result<Token, CompilerError> {
        Ok(match self.peeked.take() {
            Some(token) => token,
            None => self.advance()?,
        })
    }

    /// Peeks at next token in lexer
    fn peek(&mut self) -> Result<&Token<'input>, CompilerError> {
        Ok(if self.peeked.is_none() {
            self.peeked = Some(self.advance()?);
            self.peeked.as_ref().unwrap()
        } else {
            self.peeked.as_ref().unwrap()
        })
    }

    /// Expects to find given TokenType
    /// Returns that token or errors
    fn expect(&mut self, type_: TokenType) -> Result<Token, CompilerError> {
        let next = self.next()?;
        if type_ != next.ttype() {
            return Err(CompilerError::ParserExpectedTokenError {
                found: next.lexeme().to_string(),
                expect: type_.to_string(),
                line: next.line(),
            });
        }
        Ok(next)
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;
        // clean out the locals from ending scope
        while let Some((_, Some(depth))) = self.locals.last() {
            if depth <= &self.scope_depth {
                break;
            }
            self.locals.pop();
            self.emit_opcode(OpCode::OpPop, 0);
        }
    }

    /// Compiles source code
    /// True if compilation was successful, false if errors occurred.
    pub fn compile(&mut self) -> Result<(), CompilerError> {
        while self.peek()?.ttype() != TokenType::EOF {
            self.decl();
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            // Log any errors
            for e in self.errors.iter() {
                eprintln!("{}", e)
            }
            Err(CompilerError::CompilationFailedWithErrors)
        }
    }

    /// Synchronize compilation to the next statement
    /// in order to continue parsing
    fn synchronize(&mut self) -> Result<(), CompilerError> {
        // synchronize to next statement
        Ok(while self.peek()?.ttype() != TokenType::EOF {
            match self.peek()?.ttype() {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => break,
                _ => {
                    self.next();
                }
            }
        })
    }

    /// A declaration
    /// If the inner statement fails, synchronize
    fn decl(&mut self) -> Result<(), CompilerError> {
        let result = match self.peek()?.ttype() {
            TokenType::Var => self.var_decl(),
            _ => self.stmt(),
        };
        Ok(if let Err(err) = result {
            self.errors.push(err);
            self.synchronize();
        })
    }

    /// A variable declaration statement
    fn var_decl(&mut self) -> Result<(), CompilerError> {
        // consume the var keyword
        self.expect(TokenType::Var)?;
        // get the variable ident
        let var_ident = self.expect(TokenType::Identifier)?.lexeme().to_string();

        let peeked = self.peek()?;
        let ttype = peeked.ttype();
        let line = peeked.line();

        // if local, do not emit constant. store in compiler locals
        let var_ident_ptr = if self.scope_depth == 0 {
            // global
            self.emit_ident_const(&var_ident, line)?
        } else {
            // check if variable is already defined locally
            for (ident, depth) in self.locals.iter().rev() {
                if depth.unwrap() < self.scope_depth {
                    break;
                }
                if *ident == var_ident {
                    return Err(CompilerError::ParserLocalVariableAlreadyDefined {
                        found: var_ident,
                        line,
                    });
                }
            }
            self.locals.push((var_ident, None));
            self.locals.len() - 1
        };

        match ttype {
            TokenType::Equal => {
                // consume equal
                self.expect(TokenType::Equal)?;
                self.expr()?;
            }
            // declare but not initialized
            _ => self.emit_opcode(OpCode::OpNil, line),
        }
        self.expect(TokenType::Semicolon)?;
        // if local don't emit OpDefineGlobal
        if self.scope_depth == 0 {
            self.emit_opcode(OpCode::OpDefineGlobal(var_ident_ptr), line);
        } else {
            // set scope to know that variable was initialized
            self.locals.last_mut().unwrap().1 = Some(self.scope_depth);
        }
        Ok(())
    }

    /// A statement
    fn stmt(&mut self) -> Result<(), CompilerError> {
        match self.peek()?.ttype() {
            TokenType::Print => self.print_statement(),
            TokenType::LeftBrace => {
                self.expect(TokenType::LeftBrace)?;
                self.begin_scope();
                self.block_stmt()?;
                self.end_scope();
                self.expect(TokenType::RightBrace)?;
                Ok(())
            }
            _ => self.expr_stmt(),
        }
    }

    /// An expression statement
    fn expr_stmt(&mut self) -> Result<(), CompilerError> {
        self.expr()?;
        let line = self.expect(TokenType::Semicolon)?.line();
        self.emit_opcode(OpCode::OpPop, line);
        Ok(())
    }

    /// A print statement
    fn print_statement(&mut self) -> Result<(), CompilerError> {
        self.expect(TokenType::Print)?;
        self.expr()?;
        let line = self.expect(TokenType::Semicolon)?.line();
        self.emit_opcode(OpCode::OpPrint, line);
        Ok(())
    }

    fn block_stmt(&mut self) -> Result<(), CompilerError> {
        Ok(
            while self.peek()?.ttype() != TokenType::RightBrace
                && self.peek()?.ttype() != TokenType::EOF
            {
                self.decl();
            },
        )
    }

    /// An expression that calls expression with binding power
    fn expr(&mut self) -> Result<(), CompilerError> {
        self.expr_bp(0)
    }

    /// An expression with binding power that will do Pratt parsing
    fn expr_bp(&mut self, min_bp: u8) -> Result<(), CompilerError> {
        self.unary()?;

        loop {
            let token = self.peek()?;
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
                TokenType::Semicolon => break,
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
                self.expr_bp(right_bp)?;
                // push binary op onto chunk stack
                self.binary(&operator, current_line);

                continue;
            }
        }
        Ok(())
    }

    /// Binding powers of prefix tokens
    fn prefix_binding_power(&mut self) -> Option<((), u8)> {
        match self.peek().ok()?.ttype() {
            TokenType::Minus | TokenType::Bang => Some(((), 5)),
            _ => None,
        }
    }

    /// Binding powers of infix tokens
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

    /// A handler for binary operations
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

    /// A handler for unary operations
    /// if there are none, goes to evaluate a primary
    fn unary(&mut self) -> Result<(), CompilerError> {
        // check for unary operators
        Ok(if let Some(((), r_bp)) = self.prefix_binding_power() {
            // store operator
            let operator = self.peek()?.ttype();
            // store line
            let line = self.peek()?.line();
            // consume unary operator
            self.next()?;
            // evaluate unary operand
            self.expr_bp(r_bp)?;
            match operator {
                TokenType::Minus => self.emit_opcode(OpCode::OpNegate, line),
                TokenType::Bang => self.emit_opcode(OpCode::OpNot, line),
                _ => todo!(),
            }
        } else {
            self.primary()?;
        })
    }

    /// A primary token
    /// or a left parenthesis to trigger a grouping expression
    fn primary(&mut self) -> Result<(), CompilerError> {
        let next = self.peek()?;
        match next.ttype() {
            TokenType::Number => self.number(),
            TokenType::String => self.string(),
            TokenType::Identifier => self.variable(),
            TokenType::True | TokenType::False | TokenType::Nil => self.literal(),
            TokenType::LeftParen => self.grouping(),
            _ => Err(CompilerError::ParserUnexpectedValue {
                found: next.lexeme().to_string(),
                line: next.line(),
            }),
        }
    }

    /// A grouping expression bound by parentheses
    fn grouping(&mut self) -> Result<(), CompilerError> {
        // eat the left paren
        self.next();
        // isolate the expression within these parens
        self.expr_bp(0)?;
        // expect the closing parenthesis
        self.expect(TokenType::RightParen)?;
        Ok(())
    }

    fn number(&mut self) -> Result<(), CompilerError> {
        let token = self.next()?;
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
        let token = self.next()?;
        let line = token.line();
        Ok(match token.ttype() {
            TokenType::True => self.emit_opcode(OpCode::OpBool(true), line),
            TokenType::False => self.emit_opcode(OpCode::OpBool(false), line),
            TokenType::Nil => self.emit_opcode(OpCode::OpNil, line),
            _ => todo!(),
        })
    }

    fn string(&mut self) -> Result<(), CompilerError> {
        let string = self.next()?;
        let string_lexeme = string.lexeme().to_string();
        let line = string.line();
        let string_ptr = self
            .memory
            .add_object(Object::String(string_lexeme.clone()));
        self.emit_constant(Value::String(string_ptr), line)
    }

    fn variable(&mut self) -> Result<(), CompilerError> {
        self.named_variable()
    }

    fn named_variable(&mut self) -> Result<(), CompilerError> {
        let token = self.next()?;
        let var_ident = token.lexeme().to_string();
        let line = token.line();

        let (get_op, set_op) = if let Some(index) = self.resolve_local(&var_ident)? {
            (OpCode::OpGetLocal(index), OpCode::OpSetLocal(index))
        } else {
            // emit global variable identifier string, so it can be found during runtime
            let index = self.emit_ident_const(&var_ident, line)?;
            (OpCode::OpGetGlobal(index), OpCode::OpSetGlobal(index))
        };

        if self.peek()?.ttype() == TokenType::Equal {
            self.expect(TokenType::Equal)?;
            self.expr()?;
            self.emit_opcode(set_op, line);
        } else {
            self.emit_opcode(get_op, line);
        }
        Ok(())
    }

    /// Resolves local variable index. Returns None if not found
    fn resolve_local(&mut self, ident: &String) -> Result<Option<usize>, CompilerError> {
        for (idx, (l_ident, depth)) in self.locals.iter().enumerate().rev() {
            if depth.is_none() {
                return Err(CompilerError::ParserVariableInsideOwnInitializer {
                    found: l_ident.clone(),
                    line: self.peek()?.line(),
                });
            }
            if l_ident == ident {
                return Ok(Some(idx));
            }
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{memory::Memory, vm::VM};

    #[test]
    fn test_basic_expression() {
        let mut mem = Memory::new();
        let mut chunk = Chunk::new("test chunk".to_string());
        let source = r#""hi" + "hi""#;
        let mut c = Compiler::new(&mut chunk, source, &mut mem);
        c.expr_bp(0).unwrap();
        chunk.write(OpCode::OpReturn, 123);
        println!("result: {}", chunk);
        let mut vm = VM::new(&mut mem);
        let result = vm.run(&chunk);
        println!("{:?}", result);
    }

    #[test]
    fn test_print_statement() {
        let mut mem = Memory::new();
        let mut chunk = Chunk::new("test chunk".to_string());

        let source = r#"print "hi";"#;
        let mut c = Compiler::new(&mut chunk, source, &mut mem);
        // compile to bytecode
        c.compile();
        // debug chunk
        println!("result: {}", chunk);
        // start up VM
        let mut vm = VM::new(&mut mem);
        // run bytecode in chunk
        let result = vm.run(&chunk);
        println!("{:?}", result);
    }

    #[test]
    fn test_global_vars() {
        let mut mem = Memory::new();
        let mut chunk = Chunk::new("test chunk".to_string());

        // let source = r#"var a = "hello"; print a + "mister"; a = "bye"; print a;"#;
        let source = r#"var a = "hello"; a = 5; print a;"#;

        let mut c = Compiler::new(&mut chunk, source, &mut mem);
        // compile to bytecode
        c.compile();
        // debug chunk
        println!("result: {}", chunk);
        // start up VM
        let mut vm = VM::new(&mut mem);
        // run bytecode in chunk
        let result = vm.run(&chunk);
        println!("{:?}", result);
        println!("{:?}", mem);
    }

    #[test]
    fn test_local_vars() {
        let mut mem = Memory::new();
        let mut chunk = Chunk::new("test chunk".to_string());

        let source = r#"
        var a = 1; 
        {
            var a = 3;
            print a;
            {
                var a = 4;
                print a;
            }
        }
        print a;
        "#;

        let mut c = Compiler::new(&mut chunk, source, &mut mem);
        // compile to bytecode
        c.compile().unwrap();
        // debug chunk
        println!("result: {}", chunk);
        // start up VM
        let mut vm = VM::new(&mut mem);
        // run bytecode in chunk
        let result = vm.run(&chunk);
        println!("{:?}", result);
        println!("{:?}", mem);
    }
}

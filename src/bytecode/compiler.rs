use crate::{
    chunk::{Chunk, OpCode},
    lexer::{Lexer, Token, TokenType},
    memory::Memory,
    object::{Function, FunctionType, Object},
    value::Value,
};
use std::convert::TryInto;
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum CompilerError {
    #[error("Compilation has failed with errors.")]
    CompilationFailedWithErrors,
    #[error("Max constants in chunk.")]
    ChunkMaxConstants,
    #[error("Expected {expect}, found {found} on line {line:?}.")]
    ParserExpectedTokenError {
        expect: String,
        found: String,
        line: usize,
    },
    #[error("Expected literal value, found {found} on line {line:?}.")]
    ParserUnexpectedValue { found: String, line: usize },
    #[error("Unexpected operator, found {found} on line {line:?}.")]
    ParserUnexpectedOperator { found: String, line: usize },
    #[error("Unable to parse number, found {found} on line {line:?}.")]
    ParserFloatParseError { found: String, line: usize },
    #[error("Local variable {found} already defined in current scope on line {line:?}.")]
    ParserLocalVariableAlreadyDefined { found: String, line: usize },
    #[error("Cannot use variable {found} inside its initilizer on line {line:?}.")]
    ParserVariableInsideOwnInitializer { found: String, line: usize },
    #[error("Cannot return from top level of script, line {line:?}.")]
    ParserReturnFromTopLevel { line: usize },
    #[error("Max upvalues for function {function}")]
    ChunkMaxUpvalues { function: String },
}
pub struct Compiler<'input> {
    /// The Lexer used to tokenize the source
    lexer: Lexer<'input>,
    /// Peeked value that is next up in the lexer
    peeked: Option<Token<'input>>,
    /// Heap memory
    memory: &'input mut Memory,
    /// Vec of errors that occured during compilation
    errors: Vec<CompilerError>,
    state: CompilerState,
}

/// (index into locals, is local)
pub type UpValue = (usize, bool);
/// (Lexeme, depth, is captured) - depth is None when variable is declared but not defined (initialized)
type Local = (String, Option<usize>, bool);
#[derive(Debug)]
struct CompilerState {
    /// Some if compiler is in enclosing function
    enclosing: Option<Box<CompilerState>>,
    /// Current function that is being compiled
    function: Function,
    /// Local variable scopes, vec of variable lexemes and their depths.
    locals: Vec<Local>,
    /// Upvalues
    upvalues: Vec<UpValue>,
    /// Current scope depth
    scope_depth: usize,
}

impl CompilerState {
    fn new(function: Function) -> CompilerState {
        CompilerState {
            enclosing: None,
            function,
            locals: Vec::new(),
            upvalues: Vec::new(),
            scope_depth: 0,
        }
    }

    fn set_enclosing(&mut self, enclosing: Option<Box<CompilerState>>) {
        self.enclosing = enclosing
    }

    fn resolve_upvalue(&mut self, ident: &String) -> Result<Option<usize>, CompilerError> {
        match &mut self.enclosing {
            Some(enclosing) => {
                if let Some(index) = local_search(&enclosing.locals, ident, 0)? {
                    // FOUND so set to captured and add upvalue
                    enclosing.locals[index].2 = true;
                    Ok(Some(self.add_upvalue(index, true)?))
                } else {
                    if let Some(upvalue) = enclosing.resolve_upvalue(ident)? {
                        Ok(Some(self.add_upvalue(upvalue, false)?))
                    } else {
                        Ok(None)
                    }
                }
            }
            None => Ok(None),
        }
    }

    fn add_upvalue(&mut self, index: usize, is_local: bool) -> Result<usize, CompilerError> {
        for (idx, (uv_idx, uv_is_local)) in self.upvalues.iter().copied().enumerate() {
            if idx == uv_idx && uv_is_local == is_local {
                return Ok(idx);
            }
        }
        self.upvalues.push((index, is_local));
        Ok(self.upvalues.len() - 1)
    }
}

impl<'input> Compiler<'input> {
    pub fn new(
        source: &'input str,
        memory: &'input mut Memory,
        function: Function,
    ) -> Compiler<'input> {
        Compiler {
            lexer: Lexer::new(source),
            peeked: None,
            memory,
            errors: Vec::new(),
            state: CompilerState::new(function),
        }
    }

    fn set_state(&mut self, state: CompilerState) {
        self.state = state;
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        self.state.function.chunk_mut()
    }

    /// Emits an opcode to the current chunk
    fn emit_opcode(&mut self, op: OpCode, line: usize) {
        self.current_chunk().write(op, line);
    }

    // Makes a constant in the current chunk
    fn make_constant(&mut self, value: Value, line: usize) -> Result<usize, CompilerError> {
        let idx = self.current_chunk().add_constant(value);
        if idx > u16::MAX as usize {
            return Err(CompilerError::ChunkMaxConstants);
        }
        Ok(idx as usize)
    }

    /// Emits a constant value into the current chunk's constant vec
    fn emit_constant(&mut self, value: Value, line: usize) -> Result<(), CompilerError> {
        let idx = self.make_constant(value, line)?;
        self.emit_opcode(OpCode::OpConstant(idx as u16), line);
        Ok(())
    }

    fn emit_jump(&mut self, op: OpCode, line: usize) -> Result<usize, CompilerError> {
        self.emit_opcode(op, line);
        // return index of above opcode
        Ok(self.current_chunk().code().len() - 1)
    }

    fn patch_jump(&mut self, offset: usize, op: OpCode) -> Result<(), CompilerError> {
        // calculate jump
        let jump = self.current_chunk().code().len() - offset;
        // can't send enum variants as first-class args so i need to do this hack
        let patched_op = match op {
            OpCode::OpJump(_) => OpCode::OpJump(jump),
            OpCode::OpJumpIfFalse(_) => OpCode::OpJumpIfFalse(jump),
            // programmer's error
            _ => unimplemented!(),
        };
        // replace placeholder jump distance
        self.current_chunk().set_code(offset, patched_op);
        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize) -> Result<(), CompilerError> {
        let offset = self.current_chunk().code().len() - loop_start;
        self.emit_opcode(OpCode::OpLoop(offset), 0);
        Ok(())
    }

    fn emit_return(&mut self, line: usize) {
        self.emit_opcode(OpCode::OpNil, line);
        self.emit_opcode(OpCode::OpReturn, line)
    }

    fn emit_ident_const(&mut self, ident: &String) -> Result<usize, CompilerError> {
        // put ident in memory
        let ident_ptr = self.memory.add_object(Object::String(ident.clone()));
        // create constant with ident ptr
        self.current_chunk().add_constant(Value::String(ident_ptr));
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
        self.state.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.state.scope_depth -= 1;
        // clean out the locals from ending scope
        while let Some((_, Some(depth), is_captured)) = self.state.locals.last() {
            if depth <= &self.state.scope_depth {
                break;
            }
            if *is_captured {
                self.emit_opcode(OpCode::OpCloseUpValue, 0)
            } else {
                self.emit_opcode(OpCode::OpPop, 0);
            }
            self.state.locals.pop();
        }
    }

    /// Compiles source code
    /// True if compilation was successful, false if errors occurred.
    pub fn compile(&mut self) -> Result<Function, CompilerError> {
        while self.peek()?.ttype() != TokenType::EOF {
            self.decl()?;
        }

        if self.errors.is_empty() {
            // yucky clone
            Ok(self.state.function.clone())
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
                    self.next()?;
                }
            }
        })
    }

    /// A declaration
    /// If the inner statement fails, synchronize
    fn decl(&mut self) -> Result<(), CompilerError> {
        let result = match self.peek()?.ttype() {
            TokenType::Var => self.var_decl(),
            TokenType::Fun => self.fun_decl(),
            TokenType::Class => self.class_decl(),
            _ => self.stmt(),
        };
        Ok(if let Err(err) = result {
            self.errors.push(err);
            self.synchronize()?;
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
        let var_ident_ptr = if self.state.scope_depth == 0 {
            // global
            self.emit_ident_const(&var_ident)?
        } else {
            // check if variable is already defined locally
            for (ident, depth, is_captured) in self.state.locals.iter().rev() {
                if depth.unwrap() < self.state.scope_depth {
                    break;
                }
                if *ident == var_ident {
                    return Err(CompilerError::ParserLocalVariableAlreadyDefined {
                        found: var_ident,
                        line,
                    });
                }
            }
            self.state.locals.push((var_ident, None, false));
            self.state.locals.len() - 1
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
        // if local, don't emit OpDefineGlobal
        if self.state.scope_depth == 0 {
            self.emit_opcode(OpCode::OpDefineGlobal(var_ident_ptr), line);
        } else {
            // set scope to know that variable was initialized
            self.state.locals.last_mut().unwrap().1 = Some(self.state.scope_depth);
        }
        Ok(())
    }

    fn fun_decl(&mut self) -> Result<(), CompilerError> {
        self.expect(TokenType::Fun)?;
        let fun_token = self.expect(TokenType::Identifier)?;
        let fun_ident = fun_token.lexeme().to_string();
        let line = fun_token.line();

        // bind function to global or local variable
        let fun_ident_ptr = if self.state.scope_depth == 0 {
            // global
            self.emit_ident_const(&fun_ident)?
        } else {
            // check if variable is already defined locally
            for (ident, depth, is_captured) in self.state.locals.iter().rev() {
                if depth.unwrap() < self.state.scope_depth {
                    break;
                }
                if *ident == fun_ident {
                    return Err(CompilerError::ParserLocalVariableAlreadyDefined {
                        found: fun_ident,
                        line,
                    });
                }
            }
            // give it a depth so that we can use it inside the function body
            self.state
                .locals
                .push((fun_ident.clone(), Some(self.state.scope_depth), false));
            self.state.locals.len() - 1
        };

        // compile function
        self.function(fun_ident.clone(), FunctionType::Function)?;

        // if local, don't emit OpDefineGlobal
        if self.state.scope_depth == 0 {
            self.emit_opcode(OpCode::OpDefineGlobal(fun_ident_ptr), line);
        } else {
            // set scope to know that variable was initialized
            self.state.locals.last_mut().unwrap().1 = Some(self.state.scope_depth);
        }

        Ok(())
    }

    fn function(
        &mut self,
        fun_ident: String,
        function_type: FunctionType,
    ) -> Result<(), CompilerError> {
        // fuckin hacky
        // create a new function (the one we're about to compile)
        let new_function = Function::new(
            fun_ident.clone(),
            function_type,
            0,
            Chunk::new(fun_ident.clone()),
        );
        // create new state
        let mut temp_state = CompilerState::new(new_function);
        // swap to the new state
        std::mem::swap(&mut self.state, &mut temp_state);
        // set the enclosing state...upvalue shit
        self.state.set_enclosing(Some(Box::new(temp_state)));
        self.begin_scope();

        // compile the function
        // parse parameters and put them in the function scope
        self.expect(TokenType::LeftParen)?;
        while self.peek()?.ttype() != TokenType::RightParen {
            self.state.function.inc_arity();
            let param = self.expect(TokenType::Identifier)?;
            let param_ident = param.lexeme().to_string();
            self.state.locals.push((param_ident, Some(0), false));
            if self.peek()?.ttype() != TokenType::Comma {
                break;
            } else {
                self.expect(TokenType::Comma)?;
            }
        }
        self.expect(TokenType::RightParen)?;

        // function body
        let line = self.expect(TokenType::LeftBrace)?.line();
        self.block_stmt()?;
        self.end_scope();
        self.expect(TokenType::RightBrace)?;

        // return from the function
        self.emit_return(line);

        let compiled_function = self.state.function.clone();
        let upvals = self.state.upvalues.clone();
        // restore state
        let old_state = std::mem::take(&mut self.state.enclosing).unwrap();
        self.set_state(*old_state);

        // store function on the heap
        let fun_ptr = self
            .memory
            .add_object(Object::Function(Rc::new(compiled_function)));
        let const_ptr = self.make_constant(Value::Object(fun_ptr), line)?;

        self.emit_opcode(OpCode::OpClosure(const_ptr, upvals), line);
        Ok(())
    }

    fn class_decl(&mut self) -> Result<(), CompilerError> {
        self.expect(TokenType::Class)?;
        let class_token = self.expect(TokenType::Identifier)?;
        let class_ident = class_token.lexeme().to_string();
        let line = class_token.line();
        // if local, do not emit constant. store in compiler locals
        let class_ident_ptr = if self.state.scope_depth == 0 {
            // global
            self.emit_ident_const(&class_ident)?
        } else {
            // check if variable is already defined locally
            for (ident, depth, _) in self.state.locals.iter().rev() {
                if depth.unwrap() < self.state.scope_depth {
                    break;
                }
                if *ident == class_ident {
                    return Err(CompilerError::ParserLocalVariableAlreadyDefined {
                        found: class_ident,
                        line,
                    });
                }
            }
            self.state.locals.push((class_ident, None, false));
            self.state.locals.len() - 1
        };

        self.emit_opcode(OpCode::OpClass(class_ident_ptr), line);

        // if local, don't emit OpDefineGlobal
        if self.state.scope_depth == 0 {
            self.emit_opcode(OpCode::OpDefineGlobal(class_ident_ptr), line);
        } else {
            // set scope to know that variable was initialized
            self.state.locals.last_mut().unwrap().1 = Some(self.state.scope_depth);
        }

        self.expect(TokenType::LeftBrace)?;
        self.expect(TokenType::RightBrace)?;
        Ok(())
    }

    /// A statement
    fn stmt(&mut self) -> Result<(), CompilerError> {
        match self.peek()?.ttype() {
            TokenType::Print => self.print_stmt(),
            TokenType::If => self.if_stmt(),
            TokenType::While => self.while_stmt(),
            TokenType::For => self.for_stmt(),
            TokenType::Return => self.return_stmt(),
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
    fn print_stmt(&mut self) -> Result<(), CompilerError> {
        self.expect(TokenType::Print)?;
        self.expr()?;
        let line = self.expect(TokenType::Semicolon)?.line();
        self.emit_opcode(OpCode::OpPrint, line);
        Ok(())
    }

    /// An if-statement
    fn if_stmt(&mut self) -> Result<(), CompilerError> {
        // consume if and (, parse condition, consume )
        self.expect(TokenType::If)?;
        self.expect(TokenType::LeftParen)?;
        self.expr()?;
        let line = self.expect(TokenType::RightParen)?.line();

        let then_jump = self.emit_jump(OpCode::OpJumpIfFalse(0xFFFF), line)?;
        // pop condition off stack before executing statement...
        self.emit_opcode(OpCode::OpPop, line);
        // evaluate then-statement
        self.stmt()?;

        let else_jump = self.emit_jump(OpCode::OpJump(0xFFFF), line)?;

        self.patch_jump(then_jump, OpCode::OpJumpIfFalse(0))?;
        // ...or pop condition off stack here before else
        self.emit_opcode(OpCode::OpPop, line);

        // handle else
        if self.peek()?.ttype() == TokenType::Else {
            self.expect(TokenType::Else)?;
            self.stmt()?;
        }

        self.patch_jump(else_jump, OpCode::OpJump(0))
    }

    fn while_stmt(&mut self) -> Result<(), CompilerError> {
        let loop_start = self.current_chunk().code().len();

        self.expect(TokenType::While)?;
        self.expect(TokenType::LeftParen)?;
        self.expr()?;
        let line = self.expect(TokenType::RightParen)?.line();

        let exit_jump = self.emit_jump(OpCode::OpJumpIfFalse(0xFFFF), line)?;
        self.emit_opcode(OpCode::OpPop, line);
        self.stmt()?;

        // loop
        self.emit_loop(loop_start)?;

        self.patch_jump(exit_jump, OpCode::OpJumpIfFalse(0))?;
        self.emit_opcode(OpCode::OpPop, line);

        Ok(())
    }

    fn for_stmt(&mut self) -> Result<(), CompilerError> {
        self.begin_scope();
        self.expect(TokenType::For)?;
        self.expect(TokenType::LeftParen)?;
        // Initializer
        if self.peek()?.ttype() == TokenType::Semicolon {
            // no initializer
            self.expect(TokenType::Semicolon)?;
        } else if self.peek()?.ttype() == TokenType::Var {
            self.var_decl()?;
        } else {
            self.expr_stmt()?;
        }

        // mark the start of the loop after initializer
        let mut loop_start = self.current_chunk().code().len();

        // Condition
        let mut exit_jump: Option<usize> = None;
        if self.peek()?.ttype() != TokenType::Semicolon {
            self.expr()?;
            let line = self.expect(TokenType::Semicolon)?.line();

            // jump out if false
            exit_jump = Some(self.emit_jump(OpCode::OpJumpIfFalse(0xFFFF), line)?);
            // pop off condition result
            self.emit_opcode(OpCode::OpPop, line);
        } else {
            // no condition...infinite loop
            self.expect(TokenType::Semicolon)?;
        }

        // Increment clause
        if self.peek()?.ttype() != TokenType::RightParen {
            // jump the increment because we will do it after the body
            let body_jump = self.emit_jump(OpCode::OpJump(0xFFFF), 0)?;
            // mark the start of the increment statement
            let increment_start = self.current_chunk().code().len();
            // increment expression
            self.expr()?;
            self.emit_opcode(OpCode::OpPop, 0);
            self.expect(TokenType::RightParen)?;

            // emit loop to return to start after incrementing
            self.emit_loop(loop_start)?;
            // set the start to increment start so we can get to it after we execute the body
            loop_start = increment_start;
            self.patch_jump(body_jump, OpCode::OpJump(0))?;
        } else {
            // no increment
            self.expect(TokenType::RightParen)?;
        }

        // for loop body
        self.stmt()?;
        // loop back to increment clause, which will then take us back to the condition check
        self.emit_loop(loop_start)?;

        // patch the jump for exiting the loop
        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump, OpCode::OpJumpIfFalse(0))?;
            // pop condition
            self.emit_opcode(OpCode::OpPop, 0);
        }
        self.end_scope();
        Ok(())
    }

    fn block_stmt(&mut self) -> Result<(), CompilerError> {
        Ok(
            while self.peek()?.ttype() != TokenType::RightBrace
                && self.peek()?.ttype() != TokenType::EOF
            {
                self.decl()?;
            },
        )
    }

    fn return_stmt(&mut self) -> Result<(), CompilerError> {
        let line = self.expect(TokenType::Return)?.line();
        // prevent return from FunctionType script
        if let FunctionType::Script = self.state.function.fun_type() {
            return Err(CompilerError::ParserReturnFromTopLevel { line });
        }
        if self.peek()?.ttype() == TokenType::Semicolon {
            self.expect(TokenType::Semicolon)?;
            self.emit_return(line);
        } else {
            self.expr()?;
            self.expect(TokenType::Semicolon)?;
            self.emit_opcode(OpCode::OpReturn, line);
        }
        Ok(())
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
                | TokenType::GreaterEqual
                | TokenType::And
                | TokenType::Or
                | TokenType::LeftParen => token.ttype(),
                // Sentinel tokens
                TokenType::Semicolon | TokenType::RightParen | TokenType::Comma => break,
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

            if let Some((l_bp, ())) = self.postfix_binding_power(&operator) {
                if l_bp < min_bp {
                    break;
                }

                self.next()?;
                self.postfix(&operator, current_line)?;
                continue;
            }

            if let Some((left_bp, right_bp)) = self.infix_binding_power(&operator) {
                if left_bp < min_bp {
                    // stop parsing this expression
                    break;
                }
                // hacky hack to short circuit logical ops
                if let TokenType::And | TokenType::Or = operator {
                    self.logical_op(&operator, right_bp, current_line)?;
                } else {
                    // consume operator and move to next operand
                    self.next()?;
                    // calculate right hand side using the right bp as the new leftmost bp
                    self.expr_bp(right_bp)?;
                    // push binary op onto chunk stack
                    self.binary(&operator, current_line);
                }

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
            | TokenType::GreaterEqual
            | TokenType::And
            | TokenType::Or => Some((2, 3)),
            TokenType::Plus | TokenType::Minus => Some((4, 5)),
            TokenType::Star | TokenType::Slash => Some((6, 7)),
            _ => None,
        }
    }

    fn postfix_binding_power(&mut self, operator: &TokenType) -> Option<(u8, ())> {
        match operator {
            TokenType::LeftParen => Some((12, ())),
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

    fn postfix(&mut self, operator: &TokenType, line: usize) -> Result<(), CompilerError> {
        match operator {
            TokenType::LeftParen => self.call(line),
            _ => todo!(),
        }
    }

    fn call(&mut self, line: usize) -> Result<(), CompilerError> {
        let arg_count = self.argument_list()?;
        self.emit_opcode(OpCode::OpCall(arg_count), line);
        Ok(())
    }

    fn argument_list(&mut self) -> Result<usize, CompilerError> {
        let mut arg_count = 0;
        while self.peek()?.ttype() != TokenType::RightParen {
            self.expr()?;
            arg_count += 1;
            if self.peek()?.ttype() != TokenType::Comma {
                break;
            } else {
                self.expect(TokenType::Comma)?;
            }
        }
        self.expect(TokenType::RightParen)?;
        Ok(arg_count)
    }

    fn logical_op(
        &mut self,
        operator: &TokenType,
        right_bp: u8,
        line: usize,
    ) -> Result<(), CompilerError> {
        Ok(match operator {
            TokenType::And => {
                let end_jump = self.emit_jump(OpCode::OpJumpIfFalse(0xFFFF), line)?;
                self.emit_opcode(OpCode::OpPop, line);
                // consume operator and move to next operand
                self.next()?;
                // calculate right hand side using the right bp as the new leftmost bp
                self.expr_bp(right_bp)?;
                self.patch_jump(end_jump, OpCode::OpJumpIfFalse(0))?;
            }
            TokenType::Or => {
                let else_jump = self.emit_jump(OpCode::OpJumpIfFalse(0xFFFF), line)?;
                let end_jump = self.emit_jump(OpCode::OpJump(0xFFFF), line)?;
                self.patch_jump(else_jump, OpCode::OpJumpIfFalse(0))?;
                self.emit_opcode(OpCode::OpPop, line);
                // consume operator and move to next operand
                self.next()?;
                // calculate right hand side using the right bp as the new leftmost bp
                self.expr_bp(right_bp)?;

                self.patch_jump(end_jump, OpCode::OpJump(end_jump))?;
            }
            _ => unreachable!(),
        })
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
    /// or a left parenthesis to trigger a grouping/call expression
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
        self.next()?;
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

        let (get_op, set_op) = {
            if let Some(index) = self.resolve_local(&var_ident)? {
                (OpCode::OpGetLocal(index), OpCode::OpSetLocal(index))
            } else {
                if let Some(index) = self.resolve_upvalue(&var_ident)? {
                    (OpCode::OpGetUpValue(index), OpCode::OpSetUpValue(index))
                } else {
                    // emit global variable identifier string, so it can be found during runtime
                    let index = self.emit_ident_const(&var_ident)?;
                    (OpCode::OpGetGlobal(index), OpCode::OpSetGlobal(index))
                }
            }
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
        let line = self.peek()?.line();
        let locals = &self.state.locals;
        local_search(locals, ident, line)
    }

    fn resolve_upvalue(&mut self, ident: &String) -> Result<Option<usize>, CompilerError> {
        let line = self.peek()?.line();
        self.state.resolve_upvalue(ident)
        // if let Some(enclosing) = &mut self.state.enclosing {
        //     Ok(enclosing.resolve_upvalue(ident)?)
        // } else {
        //     Ok(None)
        // }
    }
}

fn local_search(
    locals: &[(String, Option<usize>, bool)],
    ident: &String,
    line: usize,
) -> Result<Option<usize>, CompilerError> {
    for (idx, (l_ident, depth, is_captured)) in locals.iter().enumerate().rev() {
        if depth.is_none() {
            return Err(CompilerError::ParserVariableInsideOwnInitializer {
                found: l_ident.clone(),
                line,
            });
        }
        if l_ident == ident {
            return Ok(Some(idx));
        }
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{chunk::OpCode, memory::Memory, vm::VM};

    #[test]
    fn test_basic_expression() {
        let mut mem = Memory::new();
        let source = r#"
        "hi" + "hi";
        1 + 2;

        fun testFun(hi) {
            print i;
        }

        testFun();
        "#;
        // let v1 = vec![OpCode::OpConstant(0), OpCode::OpConstant(1), OpCode::OpAdd, OpCode::OpPop, OpCode::OpConstant(2), OpCode::OpConstant(3), OpCode::OpAdd, OpCode::OpPop];
        let mut c = Compiler::new(
            source,
            &mut mem,
            Function::new(
                "main".to_string(),
                FunctionType::Script,
                0,
                Chunk::new("main".to_string()),
            ),
        );
        assert!(c.compile().is_ok());
    }

    // #[test]
    // fn test_print_statement() {
    //     let mut mem = Memory::new();
    //     let mut chunk = Chunk::new("test chunk".to_string());

    //     let source = r#"print "hi";"#;
    //     let mut c = Compiler::new(&mut chunk, source, &mut mem);
    //     // compile to bytecode
    //     c.compile();
    //     c.emit_return(0);
    //     // debug chunk
    //     println!("{}", chunk);
    //     // start up VM
    //     let mut vm = VM::new(&mut mem);
    //     // run bytecode in chunk
    //     let result = vm.run(&chunk);
    //     println!("{:?}", result);
    // }

    // #[test]
    // fn test_global_vars() {
    //     let mut mem = Memory::new();
    //     let mut chunk = Chunk::new("test chunk".to_string());

    //     // let source = r#"var a = "hello"; print a + "mister"; a = "bye"; print a;"#;
    //     let source = r#"var a = "hello"; a = 5; print a;"#;

    //     let mut c = Compiler::new(&mut chunk, source, &mut mem);
    //     // compile to bytecode
    //     c.compile();
    //     c.emit_return(0);
    //     // debug chunk
    //     println!("{}", chunk);
    //     // start up VM
    //     let mut vm = VM::new(&mut mem);
    //     // run bytecode in chunk
    //     let result = vm.run(&chunk);
    //     println!("{:?}", result);
    //     println!("{:?}", mem);
    // }

    // #[test]
    // fn test_local_vars() {
    //     let mut mem = Memory::new();
    //     let mut chunk = Chunk::new("test chunk".to_string());

    //     let source = r#"
    //     var a = 1;
    //     var b = 10;
    //     {
    //         var a = 3;
    //         print a;
    //         {
    //             var a = 4;
    //             print a;
    //             b = 9;
    //         }
    //     }
    //     print a;
    //     print b;
    //     a = "goodbye";
    //     print a;
    //     "#;

    //     let mut c = Compiler::new(&mut chunk, source, &mut mem);
    //     // compile to bytecode
    //     c.compile().unwrap();
    //     c.emit_return(0);
    //     // debug chunk
    //     println!("{}", chunk);
    //     // start up VM
    //     let mut vm = VM::new(&mut mem);
    //     // run bytecode in chunk
    //     let result = vm.run(&chunk);
    //     println!("{:?}", result);
    //     println!("{:?}", mem);
    // }

    // #[test]
    // fn test_if_stmt() {
    //     let mut mem = Memory::new();
    //     let mut chunk = Chunk::new("test chunk".to_string());

    //     let source = r#"
    //     var a = 1;
    //     if(a == 3) {
    //         print "hello, 3!";
    //     } else {
    //         print "hi, 1";
    //     }
    //     "#;
    //     let mut c = Compiler::new(&mut chunk, source, &mut mem);
    //     // compile to bytecode
    //     c.compile().unwrap();
    //     c.emit_return(0);
    //     // debug chunk
    //     println!("{}", chunk);
    //     // start up VM
    //     let mut vm = VM::new(&mut mem);
    //     // run bytecode in chunk
    //     let result = vm.run(&chunk);
    //     println!("{:?}", result);
    //     println!("{:?}", mem);
    // }

    // #[test]
    // fn test_logical_ops() {
    //     let mut mem = Memory::new();
    //     let mut chunk = Chunk::new("test chunk".to_string());

    //     let source = r#"
    //     var a = true;
    //     var b = true;
    //     if(a and b) {
    //         print "hello, truth!";
    //     } else {
    //         print "hi, false";
    //     }
    //     a = false;
    //     if(a or b) {
    //         print "hello, or!";
    //     }
    //     "#;
    //     let mut c = Compiler::new(&mut chunk, source, &mut mem);
    //     // compile to bytecode
    //     c.compile().unwrap();
    //     c.emit_return(0);
    //     // debug chunk
    //     println!("{}", chunk);
    //     // start up VM
    //     let mut vm = VM::new(&mut mem);
    //     // run bytecode in chunk
    //     let result = vm.run(&chunk);
    //     println!("{:?}", result);
    //     println!("{:?}", mem);
    // }

    // #[test]
    // fn test_loops() {
    //     let mut mem = Memory::new();
    //     let mut chunk = Chunk::new("test chunk".to_string());

    //     let source = r#"
    //     // var i = 0;
    //     // while (i < 3) {
    //     //     print i;
    //     //     i = i + 1;
    //     // }

    //     for(var j = 0; j < 5; j = j + 1) {
    //         print j;
    //     }
    //     "#;
    //     let mut c = Compiler::new(&mut chunk, source, &mut mem);
    //     // compile to bytecode
    //     c.compile().unwrap();
    //     c.emit_return(0);
    //     // debug chunk
    //     println!("{}", chunk);
    //     // start up VM
    //     let mut vm = VM::new(&mut mem);
    //     // run bytecode in chunk
    //     let result = vm.run(&chunk);
    //     println!("{:?}", result);
    //     println!("{:?}", mem);
    // }
}

use super::ast::{Atom, Declaration, Expr, Program, Scope, Stmt};
use super::tokens::{LiteralType, Token, TokenType};
use anyhow::Result;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
enum ParseError {
    #[error("Syntax error found.")]
    SyntaxError,
    #[error("Parser in bad state.")]
    BadState,
    #[error("Unexpected token {token:?} on line {line:?}.")]
    UnexpectedToken { token: TokenType, line: usize },
    #[error("Expecting identifier after 'var', found {token:?}.")]
    ExpectedIdentiferForVar { token: TokenType },
    #[error("Expecting identifier after 'fun', found {token:?}.")]
    ExpectedIdentiferForFun { token: TokenType },
    #[error("Unexpected EOF.")]
    UnexpectedEOF,
    #[error("Error with for-loop initializer.")]
    ForLoopInitializerError,
    #[error("Exceeded maximum arg count.")]
    MaxArgCountError,
}
pub struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Parser {
        tokens.reverse();
        Parser { tokens }
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.pop()
    }

    fn peek(&mut self) -> Option<Token> {
        self.tokens.last().cloned()
    }

    fn expect(&mut self, ttype: TokenType) -> Result<TokenType> {
        if let Some(token) = self.next() {
            anyhow::ensure!(
                token.token == ttype,
                ParseError::UnexpectedToken {
                    token: token.token,
                    line: token.line
                }
            );
            Ok(token.token)
        } else {
            anyhow::bail!(ParseError::UnexpectedEOF)
        }
    }
}

pub fn parse(p: &mut Parser) -> Result<Program> {
    let mut decls: Vec<Declaration> = Vec::new();

    while let Some(token) = p.peek() {
        if let TokenType::EOF = token.token {
            break;
        } else {
            decls.push(declaration(p, token.token)?);
        }
    }
    Ok(Program {
        declarations: decls,
    })
}

fn declaration(p: &mut Parser, keyword: TokenType) -> Result<Declaration> {
    Ok(match keyword {
        TokenType::Fun => {
            p.expect(TokenType::Fun)?;
            if let Some(token) = p.peek() {
                // check for function identifier
                if let Some(Token {
                    token: TokenType::Literal(LiteralType::Identifier(func_id)),
                    ..
                }) = p.next()
                {
                    p.expect(TokenType::LeftParen)?;
                    // check for params
                    let mut params: Vec<String> = Vec::new();
                    while let Some(token) = p.peek() {
                        if let TokenType::Literal(LiteralType::Identifier(id)) = token.token {
                            // store identifier
                            params.push(id);
                            // consume current identifier
                            p.next();
                            // check for closing paren or comma
                            if let Some(Token {
                                token: TokenType::RightParen,
                                ..
                            }) = p.peek()
                            {
                                p.expect(TokenType::RightParen)?;
                                break;
                            } else {
                                // expect comma to continue parsing parameters
                                p.expect(TokenType::Comma)?;
                            }
                        } else {
                            // if no parameter identifier, expect closing paren and break
                            p.expect(TokenType::RightParen)?;
                            break;
                        }
                    }

                    // get function body
                    // expect function block
                    let block = stmt(p, TokenType::LeftBrace)?;
                    Declaration::FunDecl(func_id, params, block)
                } else {
                    anyhow::bail!(ParseError::ExpectedIdentiferForFun { token: token.token })
                }
            } else {
                anyhow::bail!(ParseError::UnexpectedEOF)
            }
        }
        TokenType::Var => {
            p.expect(TokenType::Var)?;
            if let Some(token) = p.peek() {
                if let TokenType::Literal(LiteralType::Identifier(id)) = token.token {
                    let expr = expr(p)?;
                    p.expect(TokenType::Semicolon)?;
                    Declaration::VarDecl(id, Stmt::ExprStmt(expr))
                } else {
                    anyhow::bail!(ParseError::ExpectedIdentiferForVar { token: token.token })
                }
            } else {
                anyhow::bail!(ParseError::UnexpectedEOF)
            }
        }
        _ => Declaration::Statement(stmt(p, keyword)?),
    })
}

fn stmt(p: &mut Parser, keyword: TokenType) -> Result<Stmt> {
    Ok(match keyword {
        TokenType::If => {
            p.expect(TokenType::If)?;
            p.expect(TokenType::LeftParen)?;
            let cond = expr(p)?;
            p.expect(TokenType::RightParen)?;
            if let Some(next_token) = p.peek() {
                let main_stmt = stmt(p, next_token.token)?;
                let mut else_stmt = None;
                if let Some(Token {
                    token: TokenType::Else,
                    ..
                }) = p.peek()
                {
                    p.expect(TokenType::Else)?;
                    if let Some(next_token) = p.peek() {
                        else_stmt = Some(Box::new(stmt(p, next_token.token)?));
                    } else {
                        anyhow::bail!(ParseError::UnexpectedEOF);
                    }
                }
                Stmt::IfStmt(cond, Box::new(main_stmt), else_stmt)
            } else {
                anyhow::bail!(ParseError::UnexpectedEOF);
            }
        }
        TokenType::While => {
            p.expect(TokenType::While)?;
            p.expect(TokenType::LeftParen)?;
            let cond = expr(p)?;
            p.expect(TokenType::RightParen)?;
            if let Some(next_token) = p.peek() {
                let main_stmt = stmt(p, next_token.token)?;
                Stmt::WhileStmt(cond, Box::new(main_stmt))
            } else {
                anyhow::bail!(ParseError::UnexpectedEOF);
            }
        }
        TokenType::For => {
            p.expect(TokenType::For)?;
            p.expect(TokenType::LeftParen)?;

            let initializer = if let Some(next_token) = p.peek() {
                match next_token.token {
                    TokenType::Semicolon => {
                        p.expect(TokenType::Semicolon)?;
                        None
                    }
                    TokenType::Var => Some(declaration(p, next_token.token)?),
                    _ => {
                        let expr = expr(p)?;
                        Some(Declaration::Statement(Stmt::ExprStmt(expr)))
                    }
                }
            } else {
                anyhow::bail!(ParseError::ForLoopInitializerError)
            };

            let condition = if let Some(Token {
                token: TokenType::Semicolon,
                ..
            }) = p.peek()
            {
                None
            } else {
                Some(expr(p)?)
            };
            p.expect(TokenType::Semicolon)?;

            let increment = if let Some(Token {
                token: TokenType::RightParen,
                ..
            }) = p.peek()
            {
                None
            } else {
                Some(expr(p)?)
            };
            p.expect(TokenType::RightParen)?;

            let body = if let Some(next_token) = p.peek() {
                stmt(p, next_token.token)?
            } else {
                anyhow::bail!(ParseError::UnexpectedEOF)
            };

            // desugar to while
            //
            // block
            //  initialization
            //  while (condition)
            //      block
            //         body
            //         increment
            let mut outer_block: Vec<Declaration> = Vec::new();
            if let Some(initializer) = initializer {
                outer_block.push(initializer)
            }

            let mut inner_block: Vec<Declaration> = Vec::new();
            inner_block.push(Declaration::Statement(body));
            if let Some(increment) = increment {
                inner_block.push(Declaration::Statement(Stmt::ExprStmt(increment)));
            }

            let whilestmt = Declaration::Statement(Stmt::WhileStmt(
                if let Some(condition) = condition {
                    condition
                } else {
                    Expr::Literal(Atom::Boolean(true))
                },
                Box::new(Stmt::Block(inner_block)),
            ));

            outer_block.push(whilestmt);

            Stmt::Block(outer_block)
        }
        TokenType::Return => {
            p.expect(TokenType::Return)?;
            if let Some(Token {
                token: TokenType::Semicolon,
                ..
            }) = p.peek()
            {
                p.expect(TokenType::Semicolon)?;
                Stmt::ReturnStmt(None)
            } else {
                let ret_expr = expr(p)?;
                p.expect(TokenType::Semicolon)?;
                Stmt::ReturnStmt(Some(ret_expr))
            }
        }
        TokenType::Print => {
            p.expect(TokenType::Print)?;
            let expr = expr(p)?;
            p.expect(TokenType::Semicolon)?;
            Stmt::PrintStmt(expr)
        }
        TokenType::LeftBrace => {
            p.expect(TokenType::LeftBrace)?;
            let mut decls: Vec<Declaration> = Vec::new();
            while let Some(token) = p.peek() {
                if let TokenType::RightBrace = token.token {
                    break;
                } else {
                    decls.push(declaration(p, token.token)?);
                }
            }
            p.expect(TokenType::RightBrace)?;
            Stmt::Block(decls)
        }
        _ => {
            let expr = expr(p)?;
            p.expect(TokenType::Semicolon)?;
            Stmt::ExprStmt(expr)
        }
    })
}

pub fn expr(p: &mut Parser) -> Result<Expr> {
    expr_bp(p, 0)
}

fn expr_bp(p: &mut Parser, min_bp: u8) -> Result<Expr> {
    let mut lhs = unary(p)?;
    loop {
        if let Some(token) = p.peek() {
            let op = match token.token {
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
                | TokenType::Or
                | TokenType::And
                | TokenType::Equal
                | TokenType::Dot
                | TokenType::LeftParen => token.token,
                TokenType::EOF
                | TokenType::RightParen
                | TokenType::Comma
                | TokenType::Semicolon => break,
                t => anyhow::bail!(ParseError::UnexpectedToken {
                    token: t,
                    line: token.line
                }),
            };

            if let Some((l_bp, ())) = postfix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }

                lhs = call(p, lhs)?;
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }
                p.next();
                let rhs = expr_bp(p, r_bp)?;

                lhs = Expr::Binary(Box::new(lhs), op.clone(), Box::new(rhs));
                continue;
            }
            break;
        }
    }
    Ok(lhs)
}

fn prefix_binding_power(token: &Token) -> Result<((), u8)> {
    Ok(match &token.token {
        TokenType::Bang | TokenType::Minus => ((), 8),
        t => anyhow::bail!(ParseError::UnexpectedToken {
            token: t.clone(),
            line: token.line
        }),
    })
}

fn postfix_binding_power(token: &TokenType) -> Option<(u8, ())> {
    match &token {
        TokenType::LeftParen => Some((12, ())),
        _ => None,
    }
}

fn infix_binding_power(ttype: &TokenType) -> Option<(u8, u8)> {
    match ttype {
        TokenType::Equal => Some((1, 0)),
        TokenType::Or
        | TokenType::And
        | TokenType::EqualEqual
        | TokenType::BangEqual
        | TokenType::Less
        | TokenType::LessEqual
        | TokenType::Greater
        | TokenType::GreaterEqual => Some((2, 3)),
        TokenType::Plus | TokenType::Minus => Some((4, 5)),
        TokenType::Star | TokenType::Slash => Some((6, 7)),
        TokenType::Dot => Some((10, 11)),
        _ => None,
    }
}

fn unary(p: &mut Parser) -> Result<Expr> {
    if let Some(token) = p.peek() {
        let lhs = if let TokenType::Bang | TokenType::Minus = token.token {
            p.next();
            let ((), r_bp) = prefix_binding_power(&token)?;
            let rhs = expr_bp(p, r_bp)?;
            Ok(Expr::Unary(token.token.clone(), Box::new(rhs)))
        } else {
            // call(p)
            primary(p)
        };
        lhs
    } else {
        anyhow::bail!(ParseError::BadState)
    }
}

fn call(p: &mut Parser, callee: Expr) -> Result<Expr> {
    Ok(
        if let Some(Token {
            token: TokenType::LeftParen,
            ..
        }) = p.peek()
        {
            p.expect(TokenType::LeftParen)?;
            if let Some(Token {
                token: TokenType::RightParen,
                ..
            }) = p.peek()
            {
                p.expect(TokenType::RightParen)?;
                Expr::Call(Box::new(callee), None)
            } else {
                let mut args: Vec<Expr> = Vec::new();
                while let Some(token) = p.peek() {
                    if let TokenType::RightParen = token.token {
                        p.expect(TokenType::RightParen)?;
                        break;
                    } else if args.len() < 255 {
                        let expr = expr(p)?;
                        args.push(expr);
                        if let Some(Token {
                            token: TokenType::RightParen,
                            ..
                        }) = p.peek()
                        {
                            break;
                        } else {
                            p.expect(TokenType::Comma)?;
                        }
                    } else {
                        anyhow::bail!(ParseError::MaxArgCountError)
                    }
                }
                p.expect(TokenType::RightParen)?;
                Expr::Call(Box::new(callee), Some(args))
            }
        } else {
            callee
        },
    )
}

fn primary(p: &mut Parser) -> Result<Expr> {
    Ok(if let Some(token) = p.next() {
        match token.token {
            TokenType::False => Expr::Literal(Atom::Boolean(false)),
            TokenType::True => Expr::Literal(Atom::Boolean(true)),
            TokenType::Nil => Expr::Literal(Atom::Nil),
            TokenType::Literal(LiteralType::Number(n)) => Expr::Literal(Atom::Number(n)),
            TokenType::Literal(LiteralType::String(s)) => Expr::Literal(Atom::String(s)),
            TokenType::Literal(LiteralType::Identifier(id)) => {
                Expr::Literal(Atom::Identifier(id, Scope::Global))
            }
            TokenType::LeftParen => {
                let expr = expr_bp(p, 0)?;
                p.expect(TokenType::RightParen)?;
                Expr::Grouping(Box::new(expr))
            }
            t => {
                anyhow::bail!(ParseError::UnexpectedToken {
                    token: t,
                    line: token.line
                });
            }
        }
    } else {
        unreachable!()
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Lexer;

    fn parser_setup(input: &str) -> Parser {
        let mut lexer = Lexer::new();
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        Parser::new(tokens)
    }

    #[test]
    fn test_simple_parse_expression() {
        let mut parser = parser_setup("-1 + 2 * 2");
        assert_eq!(expr(&mut parser).unwrap().to_string(), "(+ (-1) (* 2 2))");
    }

    #[test]
    fn test_parse_grouping_expression() {
        let mut parser = parser_setup("(-1 + 2) * 2");
        assert_eq!(expr(&mut parser).unwrap().to_string(), "(* ((+ (-1) 2)) 2)");
    }

    #[test]
    fn test_parse_conditional_expression() {
        let mut parser = parser_setup("1 + 2 < 2 + 2");
        assert_eq!(
            expr(&mut parser).unwrap().to_string(),
            "(< (+ 1 2) (+ 2 2))"
        );
    }

    #[test]
    fn test_syntax_error_doubleop() {
        let mut parser = parser_setup("1 * / 2");
        let expr = expr(&mut parser);
        assert!(expr.is_err());
        assert_eq!(
            expr.unwrap_err().downcast::<ParseError>().unwrap(),
            ParseError::UnexpectedToken {
                token: TokenType::Slash,
                line: 1,
            }
        );
    }

    #[test]
    fn test_syntax_error_trailingop() {
        let mut parser = parser_setup("1 * ");
        let expr = expr(&mut parser);
        assert!(expr.is_err());
        assert_eq!(
            expr.unwrap_err().downcast::<ParseError>().unwrap(),
            ParseError::UnexpectedToken {
                token: TokenType::EOF,
                line: 1,
            }
        );
    }

    #[test]
    fn test_syntax_error_postfix_plusplus() {
        let mut parser = parser_setup("1++");
        let expr = expr(&mut parser);
        assert!(expr.is_err());
    }

    #[test]
    fn test_syntax_error_lefthand_op() {
        let mut parser = parser_setup("* 1");
        let expr = expr(&mut parser);
        assert!(expr.is_err());
    }

    #[test]
    fn test_assign_identifier() {
        let mut parser = parser_setup("i = 1");
        assert_eq!(expr(&mut parser).unwrap().to_string(), "(= i 1)");
    }

    #[test]
    fn test_assign_identifier_expr() {
        let mut parser = parser_setup("i = 1 + 6 / 2");
        assert_eq!(
            expr(&mut parser).unwrap().to_string(),
            "(= i (+ 1 (/ 6 2)))"
        );
    }

    #[test]
    fn test_multiassign_expr() {
        let mut parser = parser_setup("a = b = c");
        assert_eq!(expr(&mut parser).unwrap().to_string(), "(= a (= b c))");
    }

    #[test]
    fn test_function_composition_expr() {
        let mut parser = parser_setup("a.b.c");
        assert_eq!(expr(&mut parser).unwrap().to_string(), "(. (. a b) c)");
    }

    #[test]
    fn test_function_call() {
        let mut parser = parser_setup("foo(1+1, b)");
        assert_eq!(
            expr(&mut parser).unwrap().to_string(),
            "(call((+ 1 1), b) foo)"
        );
    }

    #[test]
    fn test_function_currying() {
        let mut parser = parser_setup("foo()()()");
        assert_eq!(
            expr(&mut parser).unwrap().to_string(),
            "(call() (call() (call() foo)))"
        );
    }

    #[test]
    fn test_function_member_call_combo() {
        let mut parser = parser_setup("foo().bar(a, b, c)");
        assert_eq!(
            expr(&mut parser).unwrap().to_string(),
            "(. (call() foo) (call(a, b, c) bar))"
        );
    }

    #[test]
    fn test_function_call_within_expr() {
        let mut parser = parser_setup("foo() + 1 + bar()");
        assert_eq!(
            expr(&mut parser).unwrap().to_string(),
            "(+ (+ (call() foo) 1) (call() bar))"
        );
    }

    #[test]
    fn test_var_declaration() {
        let mut parser = parser_setup("var i = 1 + 6 / 2; var b;");
        let program = parse(&mut parser);
        println!("{:?}", program);
        assert!(program.is_ok());
    }

    #[test]
    fn test_block() {
        let mut parser = parser_setup("{ var i = 1 + 6 / 2; { print i; } }");
        let program = parse(&mut parser);
        println!("{:#?}", program);
        assert!(program.is_ok());
    }

    #[test]
    fn test_if_else() {
        let input = r#"
        if (true) {
            print "hello";
        } else {
            print "false";
        }
        "#;
        let mut parser = parser_setup(input);
        let program = parse(&mut parser);
        println!("{:#?}", program);
        assert!(program.is_ok());
    }

    #[test]
    fn test_while_loop() {
        let input = r#"
        while(true){
            print "hello";
        }
        "#;
        let mut parser = parser_setup(input);
        let program = parse(&mut parser);
        // println!("{:#?}", program);
        assert!(program.is_ok());
    }

    #[test]
    fn test_for_loop() {
        let input = r#"
        for(var i = 0; i < 3; i = i + 1) {
            print i;
        }

        for(;;)
            print i;

        var b = 10;
        for(b = 0; b = b + 1) {
            print b;
        }
        "#;
        let mut parser = parser_setup(input);
        let program = parse(&mut parser);
        // println!("{:#?}", program);
        assert!(program.is_ok());
    }

    #[test]
    fn test_function_declaration() {
        let input = r#"
        fun foo() {
            print i;
        }
        "#;
        let mut parser = parser_setup(input);
        let program = parse(&mut parser);
        // println!("{:#?}", program);
        assert!(program.is_ok());
    }
}

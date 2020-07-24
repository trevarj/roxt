use super::ast::{Atom, Declaration, Expr, Program, Stmt};
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

    fn expect(&mut self, ttype: TokenType) -> Result<()> {
        Ok(if let Some(token) = self.next() {
            anyhow::ensure!(
                token.token == ttype,
                ParseError::UnexpectedToken {
                    token: token.token,
                    line: token.line
                }
            )
        })
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
                anyhow::bail!(ParseError::BadState)
            }
        }
        _ => Declaration::Statement(stmt(p, keyword)?),
    })
}

fn stmt(p: &mut Parser, keyword: TokenType) -> Result<Stmt> {
    Ok(match keyword {
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
                | TokenType::Equal => token.token,
                TokenType::EOF | TokenType::RightParen | TokenType::Semicolon => break,
                t => anyhow::bail!(ParseError::UnexpectedToken {
                    token: t,
                    line: token.line
                }),
            };

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
        TokenType::Bang | TokenType::Minus => ((), 7),
        t => anyhow::bail!(ParseError::UnexpectedToken {
            token: t.clone(),
            line: token.line
        }),
    })
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
            primary(p)
        };
        lhs
    } else {
        anyhow::bail!(ParseError::BadState)
    }
}

fn primary(p: &mut Parser) -> Result<Expr> {
    Ok(if let Some(token) = p.next() {
        match token.token {
            TokenType::False => Expr::Literal(Atom::Boolean(false)),
            TokenType::True => Expr::Literal(Atom::Boolean(true)),
            TokenType::Nil => Expr::Literal(Atom::Nil),
            TokenType::Literal(LiteralType::Number(n)) => Expr::Literal(Atom::Number(n)),
            TokenType::Literal(LiteralType::String(s)) => Expr::Literal(Atom::String(s)),
            TokenType::Literal(LiteralType::Identifier(id)) => Expr::Literal(Atom::Identifier(id)),
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
    #[test]
    fn test_simple_parse_expression() {
        let mut lexer = Lexer::new();
        let input = "-1 + 2 * 2";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        assert_eq!(expr(&mut parser).unwrap().to_string(), "(+ (-1) (* 2 2))");
    }

    #[test]
    fn test_parse_grouping_expression() {
        let mut lexer = Lexer::new();
        let input = "(-1 + 2) * 2";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        assert_eq!(expr(&mut parser).unwrap().to_string(), "(* ((+ (-1) 2)) 2)");
    }

    #[test]
    fn test_parse_conditional_expression() {
        let mut lexer = Lexer::new();
        let input = "1 + 2 < 2 + 2";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        assert_eq!(
            expr(&mut parser).unwrap().to_string(),
            "(< (+ 1 2) (+ 2 2))"
        );
    }

    #[test]
    fn test_syntax_error_doubleop() {
        let mut lexer = Lexer::new();
        let input = "1 * / 2";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
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
        let mut lexer = Lexer::new();
        let input = "1 * ";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
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
        let mut lexer = Lexer::new();
        let input = "1++";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let expr = expr(&mut parser);
        assert!(expr.is_err());
    }

    #[test]
    fn test_syntax_error_lefthand_op() {
        let mut lexer = Lexer::new();
        let input = "* 1";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let expr = expr(&mut parser);
        assert!(expr.is_err());
    }

    #[test]
    fn test_assign_identifier() {
        let mut lexer = Lexer::new();
        let input = "i = 1";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        assert_eq!(expr(&mut parser).unwrap().to_string(), "(= i 1)");
    }

    #[test]
    fn test_assign_identifier_expr() {
        let mut lexer = Lexer::new();
        let input = "i = 1 + 6 / 2";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        assert_eq!(
            expr(&mut parser).unwrap().to_string(),
            "(= i (+ 1 (/ 6 2)))"
        );
    }

    #[test]
    fn test_multiassign_expr() {
        let mut lexer = Lexer::new();
        let input = "a = b = c";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        assert_eq!(expr(&mut parser).unwrap().to_string(), "(= a (= b c))");
    }

    #[test]
    fn test_var_declaration() {
        let mut lexer = Lexer::new();
        let input = "var i = 1 + 6 / 2;";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let program = parse(&mut parser);
        assert!(program.is_ok());
    }

    #[test]
    fn test_block() {
        let mut lexer = Lexer::new();
        let input = "{ var i = 1 + 6 / 2; { print i; } }";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let program = parse(&mut parser);
        println!("{:#?}", program);
        assert!(program.is_ok());
    }
}

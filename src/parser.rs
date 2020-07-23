use super::tokens::{LiteralType, Token, TokenType};
use anyhow::Result;
use thiserror::Error;
use std::fmt::Display;

#[derive(Error, Debug, PartialEq)]
enum ParseError {
    #[error("Syntax error found.")]
    SyntaxError,
    #[error("Parser in bad state.")]
    BadState,
    #[error("Unexpected token {token:?}.")]
    UnexpectedToken { token: TokenType },
}
pub struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Parser {
        tokens.reverse();
        Parser { tokens }
    }

    pub fn next(&mut self) -> Option<Token> {
        self.tokens.pop()
    }

    pub fn peek(&mut self) -> Option<Token> {
        self.tokens.last().cloned()
    }

    pub fn expect(&mut self, ttype: TokenType) -> Result<()> {
        Ok(if let Some(token) = self.next() {
            anyhow::ensure!(
                token.token == ttype,
                ParseError::UnexpectedToken { token: token.token }
            )
        })
    }
}

#[derive(Debug)]
enum Expr {
    Grouping(Box<Expr>),
    Binary(Box<Expr>, TokenType, Box<Expr>),
    Unary(TokenType, Box<Expr>),
    Literal(Atoms),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(match self {
            Expr::Grouping(expr) => {
                write!(f, "({})", expr)?
            }
            Expr::Binary(lhs,op, rhs) => {
                let op = match op {
                    TokenType::Plus => "+",
                    TokenType::Minus => "-",
                    TokenType::Star => "*",
                    TokenType::Slash => "/",
                    TokenType::EqualEqual => "==",
                    _ => panic!("Cannot display this operator.")
                };
                write!(f, "({} {} {})", op, lhs, rhs)?
            },
            Expr::Unary(op, expr) => {
                let op = match op {
                    TokenType::Bang => "!",
                    TokenType::Minus => "-",
                    _ => panic!("Cannot display this operator.")
                };
                write!(f, "({}{})", op, expr)?
            }
            Expr::Literal(atom) => {
                match atom {
                    Atoms::Boolean(b) => {
                        write!(f, "{}", b)?
                    }
                    Atoms::Nil => {
                        write!(f, "Nil")?
                    }
                    Atoms::Number(n) => {
                        write!(f, "{}", n)?
                    }
                    Atoms::String(s) => {
                        write!(f, "{}", s)?
                    }
                }
            }
        })
    }
    
}

#[derive(Debug)]
enum Atoms {
    Boolean(bool),
    Nil,
    Number(f32),
    String(String),
}

fn expr(p: &mut Parser) -> Result<Expr> {
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
                | TokenType::EqualEqual => token.token,
                TokenType::EOF => break,
                TokenType::RightParen => break,
                t => anyhow::bail!(ParseError::UnexpectedToken { token: t }),
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

fn prefix_binding_power(ttype: &TokenType) -> Result<((), u8)> {
    Ok(match ttype {
        TokenType::Bang | TokenType::Minus => ((), 7),
        t => anyhow::bail!(ParseError::UnexpectedToken { token: t.clone() }),
    })
}

fn infix_binding_power(ttype: &TokenType) -> Option<(u8, u8)> {
    match ttype {
        TokenType::Plus | TokenType::Minus => Some((1, 2)),
        TokenType::Star | TokenType::Slash => Some((3, 4)),
        TokenType::EqualEqual => Some((5, 6)),
        _ => None,
    }
}

fn unary(p: &mut Parser) -> Result<Expr> {
    if let Some(token) = p.peek() {
        let lhs = if let TokenType::Bang | TokenType::Minus = token.token {
            p.next();
            let ((), r_bp) = prefix_binding_power(&token.token)?;
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
            TokenType::False => Expr::Literal(Atoms::Boolean(false)),
            TokenType::True => Expr::Literal(Atoms::Boolean(true)),
            TokenType::Nil => Expr::Literal(Atoms::Nil),
            TokenType::Literal(LiteralType::Number(n)) => Expr::Literal(Atoms::Number(n)),
            TokenType::Literal(LiteralType::String(s)) => Expr::Literal(Atoms::String(s)),
            TokenType::LeftParen => {
                let expr = expr_bp(p, 0)?;
                p.expect(TokenType::RightParen)?;
                Expr::Grouping(Box::new(expr))
            }
            t => {
                anyhow::bail!(ParseError::UnexpectedToken { token: t });
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
}

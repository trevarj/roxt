use super::atom::Atom;
use crate::tokens::{LiteralType, TokenType};
use std::fmt::Display;
#[derive(Clone, Debug)]
pub enum Expr {
    Grouping(Box<Expr>),
    Binary(Box<Expr>, TokenType, Box<Expr>),
    Unary(TokenType, Box<Expr>),
    Literal(Atom),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(match self {
            Expr::Grouping(expr) => write!(f, "({})", expr)?,
            Expr::Binary(lhs, op, rhs) => {
                let op = match op {
                    TokenType::Plus => "+",
                    TokenType::Minus => "-",
                    TokenType::Star => "*",
                    TokenType::Slash => "/",
                    TokenType::EqualEqual => "==",
                    TokenType::Equal => "=",
                    TokenType::Less => "<",
                    TokenType::LessEqual => "<=",
                    TokenType::Greater => ">",
                    TokenType::GreaterEqual => ">=",
                    TokenType::And => "and",
                    TokenType::Or => "or",
                    TokenType::Literal(LiteralType::Identifier(id)) => id,
                    _ => panic!("Cannot display this operator: {:?}.", op),
                };
                write!(f, "({} {} {})", op, lhs, rhs)?
            }
            Expr::Unary(op, expr) => {
                let op = match op {
                    TokenType::Bang => "!",
                    TokenType::Minus => "-",
                    _ => panic!("Cannot display this operator."),
                };
                write!(f, "({}{})", op, expr)?
            }
            Expr::Literal(atom) => write!(f, "{}", atom)?,
        })
    }
}

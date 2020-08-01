use super::atom::Atom;
use crate::tokens::{LiteralType, TokenType};
use std::fmt::Display;
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    /// Expr bound by parentheses
    Grouping(Box<Expr>),
    /// LHS, Operator, RHS
    Binary(Box<Expr>, TokenType, Box<Expr>),
    /// Operator, Expr
    Unary(TokenType, Box<Expr>),
    /// Expr to resolve callee, optional arguments
    Call(Box<Expr>, Option<Vec<Expr>>),
    /// A literal value or identifier
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
                    TokenType::Dot => ".",
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
            Expr::Call(callee, args) => {
                if let Some(args) = args {
                    let arg_list = args
                        .iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<String>>()
                        .join(", ");
                    write!(f, "(call({}) {})", arg_list, callee)?
                } else {
                    write!(f, "(call() {})", callee)?
                }
            }
            Expr::Literal(atom) => write!(f, "{}", atom)?,
        })
    }
}

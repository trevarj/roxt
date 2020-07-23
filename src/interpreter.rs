use super::{
    parser::{Atom, Expr},
    tokens::TokenType,
};
use anyhow::Result;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
enum RuntimeError {
    #[error("Type mismatch error.")]
    TypeMismatch,
    #[error("Invalid expression operator.")]
    InvalidOperator,
    #[error("Cannot apply unary operator {op:?}.")]
    UnaryOperationInvalid { op: char },
}

fn evaluate_expr(expr: Expr) -> Result<Atom> {
    match expr {
        Expr::Grouping(expr) => evaluate_expr(*expr),
        Expr::Binary(lhs, op, rhs) => {
            let left = evaluate_expr(*lhs)?;
            let right = evaluate_expr(*rhs)?;
            evaluate_binary(op, left, right)
        }
        Expr::Unary(op, expr) => {
            let atom = evaluate_expr(*expr)?;
            evaluate_unary(op, atom)
        }
        Expr::Literal(a) => Ok(a),
    }
}

fn evaluate_binary(op: TokenType, lhs: Atom, rhs: Atom) -> Result<Atom> {
    Ok(match op {
        TokenType::Plus => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Number(a + b),
            (Atom::String(a), Atom::String(b)) => Atom::String(a + &b),
            _ => anyhow::bail!(RuntimeError::TypeMismatch),
        },
        TokenType::Minus => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Number(a - b),
            _ => anyhow::bail!(RuntimeError::TypeMismatch),
        },
        TokenType::Star => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Number(a * b),
            _ => anyhow::bail!(RuntimeError::TypeMismatch),
        },
        TokenType::Slash => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Number(a / b),
            _ => anyhow::bail!(RuntimeError::TypeMismatch),
        },
        TokenType::EqualEqual => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Boolean(a == b),
            (Atom::Boolean(a), Atom::Boolean(b)) => Atom::Boolean(a == b),
            (Atom::String(a), Atom::String(b)) => Atom::Boolean(a == b),
            _ => anyhow::bail!(RuntimeError::TypeMismatch),
        },
        TokenType::BangEqual => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Boolean(a != b),
            (Atom::Boolean(a), Atom::Boolean(b)) => Atom::Boolean(a != b),
            (Atom::String(a), Atom::String(b)) => Atom::Boolean(a != b),
            _ => anyhow::bail!(RuntimeError::TypeMismatch),
        },
        _ => anyhow::bail!(RuntimeError::InvalidOperator),
    })
}

fn evaluate_unary(op: TokenType, atom: Atom) -> Result<Atom> {
    Ok(match op {
        TokenType::Bang => match atom {
            Atom::Boolean(b) => Atom::Boolean(!b),
            _ => anyhow::bail!(RuntimeError::UnaryOperationInvalid { op: '!' }),
        },
        TokenType::Minus => match atom {
            Atom::Number(n) => Atom::Number(-n),
            _ => anyhow::bail!(RuntimeError::UnaryOperationInvalid { op: '-' }),
        },
        _ => anyhow::bail!(RuntimeError::InvalidOperator),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::*, parser::*};
    #[test]
    fn test_simple_expression_eval() {
        let mut lexer = Lexer::new();
        let input = "1 + 1";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let expr = expr(&mut parser).unwrap();
        let atom = evaluate_expr(expr).unwrap();
        assert_eq!(atom.to_string(), "2")
    }

    #[test]
    fn test_bang_bool() {
        let mut lexer = Lexer::new();
        let input = "!true";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let expr = expr(&mut parser).unwrap();
        let atom = evaluate_expr(expr).unwrap();
        assert_eq!(atom.to_string(), "false")
    }

    #[test]
    fn test_grouped_expr() {
        let mut lexer = Lexer::new();
        let input = "(2 + 2) * 2 / 2";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let expr = expr(&mut parser).unwrap();
        let atom = evaluate_expr(expr).unwrap();
        assert_eq!(atom.to_string(), "4")
    }

    #[test]
    fn test_string_concat() {
        let mut lexer = Lexer::new();
        let input = r#""hello " + "world""#;
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let expr = expr(&mut parser).unwrap();
        let atom = evaluate_expr(expr).unwrap();
        assert_eq!(atom.to_string(), "hello world")
    }
}

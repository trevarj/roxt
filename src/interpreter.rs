use super::{
    parser::{Atom, Expr},
    tokens::TokenType,
};
use anyhow::Result;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
enum RuntimeError {
    #[error("Invalid operand for expression - {lhs:?} {op:?} {rhs:?}")]
    InvalidOperand { op: String, lhs: Atom, rhs: Atom },
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
            (a, b) => anyhow::bail!(RuntimeError::InvalidOperand {
                op: "+".to_string(),
                lhs: a,
                rhs: b
            }),
        },
        TokenType::Minus => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Number(a - b),
            (a, b) => anyhow::bail!(RuntimeError::InvalidOperand {
                op: "-".to_string(),
                lhs: a,
                rhs: b
            }),
        },
        TokenType::Star => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Number(a * b),
            (a, b) => anyhow::bail!(RuntimeError::InvalidOperand {
                op: "*".to_string(),
                lhs: a,
                rhs: b
            }),
        },
        TokenType::Slash => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Number(a / b),
            (a, b) => anyhow::bail!(RuntimeError::InvalidOperand {
                op: "/".to_string(),
                lhs: a,
                rhs: b
            }),
        },
        TokenType::EqualEqual => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Boolean(a == b),
            (Atom::Boolean(a), Atom::Boolean(b)) => Atom::Boolean(a == b),
            (Atom::String(a), Atom::String(b)) => Atom::Boolean(a == b),
            (a, b) => anyhow::bail!(RuntimeError::InvalidOperand {
                op: "==".to_string(),
                lhs: a,
                rhs: b
            }),
        },
        TokenType::BangEqual => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Boolean(a != b),
            (Atom::Boolean(a), Atom::Boolean(b)) => Atom::Boolean(a != b),
            (Atom::String(a), Atom::String(b)) => Atom::Boolean(a != b),
            (a, b) => anyhow::bail!(RuntimeError::InvalidOperand {
                op: "!=".to_string(),
                lhs: a,
                rhs: b
            }),
        },
        TokenType::Less => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Boolean(a < b),
            (a, b) => anyhow::bail!(RuntimeError::InvalidOperand {
                op: "<".to_string(),
                lhs: a,
                rhs: b
            }),
        },
        TokenType::LessEqual => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Boolean(a <= b),
            (a, b) => anyhow::bail!(RuntimeError::InvalidOperand {
                op: "<=".to_string(),
                lhs: a,
                rhs: b
            }),
        },
        TokenType::Greater => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Boolean(a > b),
            (a, b) => anyhow::bail!(RuntimeError::InvalidOperand {
                op: ">".to_string(),
                lhs: a,
                rhs: b
            }),
        },
        TokenType::GreaterEqual => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Boolean(a >= b),
            (a, b) => anyhow::bail!(RuntimeError::InvalidOperand {
                op: ">=".to_string(),
                lhs: a,
                rhs: b
            }),
        },
        _ => anyhow::bail!(RuntimeError::InvalidOperator),
    })
}

fn evaluate_unary(op: TokenType, atom: Atom) -> Result<Atom> {
    Ok(match op {
        TokenType::Bang => match atom {
            Atom::Boolean(b) => Atom::Boolean(!b),
            Atom::Nil => Atom::Boolean(false),
            Atom::Number(_) => Atom::Boolean(true),
            Atom::String(_) => Atom::Boolean(true),
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
    fn test_nested_grouped_expr() {
        let mut lexer = Lexer::new();
        let input = "((((((((8))))))))";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let expr = expr(&mut parser).unwrap();
        let atom = evaluate_expr(expr).unwrap();
        assert_eq!(atom.to_string(), "8")
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

    #[test]
    fn test_less_than() {
        let mut lexer = Lexer::new();
        let input = "99 < 100";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let expr = expr(&mut parser).unwrap();
        let atom = evaluate_expr(expr).unwrap();
        assert_eq!(atom.to_string(), "true")
    }
}

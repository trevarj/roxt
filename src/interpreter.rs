use super::{
    ast::{Atom, Declaration, Expr, Program, Stmt},
    environment::{Parental, Spaghetti},
    tokens::TokenType,
};
use anyhow::Result;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
enum RuntimeError {
    #[error("Invalid operand for expression - {lhs:?} {op:?} {rhs:?}")]
    InvalidOperand { op: String, lhs: Atom, rhs: Atom },
    #[error("Invalid expression operator.")]
    InvalidOperator { op: TokenType },
    #[error("Cannot apply unary operator {op:?}.")]
    UnaryOperationInvalid { op: char },
    #[error("Variable {id:?} is undefined or out of scope.")]
    UndefinedVar { id: String },
}

type Env = Spaghetti;

pub fn interpret(program: Program) -> Result<()> {
    let env = Env::new_instance();
    for decl in program.declarations {
        evaluate_declaration(decl, env.clone())?;
    }
    Ok(())
}

fn evaluate_declaration(decl: Declaration, env: Env) -> Result<()> {
    Ok(match decl {
        Declaration::VarDecl(id, stmt) => {
            if let Stmt::ExprStmt(expr) = stmt {
                let val = evaluate_expr(expr, env.clone())?;
                env.set_var(id, val);
            }
        }
        Declaration::Statement(stmt) => evaluate_statement(stmt, env)?,
    })
}

fn evaluate_statement(stmt: Stmt, env: Env) -> Result<()> {
    match stmt {
        Stmt::ExprStmt(expr) => {
            evaluate_expr(expr, env)?;
        },
        Stmt::PrintStmt(expr) => {
            if let Expr::Literal(id) = expr {
                let val = env.get_var(id.to_string());
                if let Some(v) = val {
                    println!("{}", v);
                } else {
                    anyhow::bail!(RuntimeError::UndefinedVar { id: id.to_string() })
                }
            } else {
                let val = evaluate_expr(expr, env)?;
                println!("{}", val);
            }
        }
        Stmt::Block(decls) => {
            let block_scope = env.child();
            for decl in decls {
                evaluate_declaration(decl, block_scope.clone())?
            }
        }
    }
    Ok(())
}

fn evaluate_expr(expr: Expr, env: Env) -> Result<Atom> {
    match expr {
        Expr::Grouping(expr) => evaluate_expr(*expr, env),
        Expr::Binary(lhs, op, rhs) => {
            let left = evaluate_expr(*lhs, env.clone())?;
            let right = evaluate_expr(*rhs, env)?;
            evaluate_binary(op, left, right)
        }
        Expr::Unary(op, expr) => {
            let atom = evaluate_expr(*expr, env)?;
            evaluate_unary(op, atom)
        }
        Expr::Literal(a) => {
            if let Atom::Identifier(ref val) = a {
                if let Some(var) = env.get_var(val.to_string()) {
                    Ok(var)
                } else {
                    Ok(a)
                }
            } else {
                Ok(a)
            }
            
        }
    }
}

fn evaluate_binary(op: TokenType, lhs: Atom, rhs: Atom) -> Result<Atom> {
    Ok(match op {
        TokenType::Equal => rhs,
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
        op => anyhow::bail!(RuntimeError::InvalidOperator { op: op }),
    })
}

fn evaluate_unary(op: TokenType, atom: Atom) -> Result<Atom> {
    Ok(match op {
        TokenType::Bang => match atom {
            Atom::Boolean(b) => Atom::Boolean(!b),
            Atom::Nil => Atom::Boolean(false),
            Atom::Number(_) => Atom::Boolean(true),
            Atom::String(_) => Atom::Boolean(true),
            Atom::Identifier(_) => todo!(),
        },
        TokenType::Minus => match atom {
            Atom::Number(n) => Atom::Number(-n),
            _ => anyhow::bail!(RuntimeError::UnaryOperationInvalid { op: '-' }),
        },
        op => anyhow::bail!(RuntimeError::InvalidOperator { op: op }),
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
        let atom = evaluate_expr(expr, Env::new_instance()).unwrap();
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
        let atom = evaluate_expr(expr, Env::new_instance()).unwrap();
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
        let atom = evaluate_expr(expr, Env::new_instance()).unwrap();
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
        let atom = evaluate_expr(expr, Env::new_instance()).unwrap();
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
        let atom = evaluate_expr(expr, Env::new_instance()).unwrap();
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
        let atom = evaluate_expr(expr, Env::new_instance()).unwrap();
        assert_eq!(atom.to_string(), "true")
    }

    #[test]
    fn test_var_declaration_and_print() {
        let mut lexer = Lexer::new();
        let input = r#"
        var i = "test";
        var boolean = !true;
        var a = 5 + 5;
        var b = a + 1;
        print i;
        print boolean;
        print a;
        print b;
        "#;
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_block_scope() {
        let mut lexer = Lexer::new();
        let input = r#"
        var i = "outer";
        var x = "find me";
        {
            var i = "inner";
            {
                var i = "way inside";
                var t = "hidden";
                print i;
                print x;
            }
            //print t; <-- undefined in this scope
            print i;
        }
        print i;
        "#;
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }
}

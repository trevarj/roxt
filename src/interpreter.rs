use super::{
    ast::{Atom, Declaration, Expr, Program, Stmt},
    environment::{Parental, Spaghetti},
    tokens::TokenType,
};
use anyhow::Result;
use thiserror::Error;

#[derive(Error, Debug)]
enum RuntimeError {
    #[error("Invalid operand for expression - {lhs:?} {op:?} {rhs:?}")]
    InvalidOperand { op: String, lhs: Atom, rhs: Atom },
    #[error("Invalid expression operator.")]
    InvalidOperator { op: TokenType },
    #[error("Cannot apply unary operator {op:?}.")]
    UnaryOperationInvalid { op: char },
    #[error("Variable {id:?} is undefined or out of scope.")]
    UndefinedVar { id: String },
    #[error("Unsupported assignment: {type_a:?} = {type_b:?}.")]
    UnsupportedAssignment { type_a: String, type_b: String },
    #[error("Invalid variable declaration, {expr:?}")]
    InvalidVarDeclaration { expr: Expr },
    #[error("Invalid condition for if-else, {found:?}")]
    InvalidIfCondition { found: Atom },
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
                match expr {
                    Expr::Binary(_, _, rhs) => {
                        // declare and assign
                        let val = evaluate_expr(*rhs, env.clone())?;
                        env.set_var(id, val);
                    }
                    Expr::Literal(_) => {
                        // declaration, no assignment
                        env.set_var(id, Atom::Nil)
                    }
                    expr => anyhow::bail!(RuntimeError::InvalidVarDeclaration { expr }),
                }
            }
        }
        Declaration::Statement(stmt) => evaluate_statement(stmt, env)?,
    })
}

fn evaluate_statement(stmt: Stmt, env: Env) -> Result<()> {
    match stmt {
        Stmt::ExprStmt(expr) => {
            evaluate_expr(expr, env)?;
        }
        Stmt::IfStmt(cond, stmt, else_stmt) => {
            match evaluate_expr(cond, env.clone())? {
                Atom::Boolean(true) => {
                    // execute main stmt (or block)
                    evaluate_statement(*stmt, env.clone())?
                }
                Atom::Boolean(false) => {
                    if let Some(else_stmt) = else_stmt {
                        evaluate_statement(*else_stmt, env.clone())?
                    }
                }
                res => anyhow::bail!(RuntimeError::InvalidIfCondition { found: res }),
            }
        }
        Stmt::PrintStmt(expr) => {
            if let Expr::Literal(atom) = expr {
                let value = if let Atom::Identifier(id) = atom {
                    if let Some(var) = env.get_var(id.as_str()) {
                        var.to_string()
                    } else {
                        anyhow::bail!(RuntimeError::UndefinedVar { id })
                    }
                } else {
                    atom.to_string()
                };
                println!("{}", value);
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
            // Short circuiting for logic operations
            match (op, left) {
                (TokenType::Or, Atom::Boolean(true)) => return Ok(Atom::Boolean(true)),
                (TokenType::And, Atom::Boolean(false)) => return Ok(Atom::Boolean(false)),
                (op, left) => {
                    let right = evaluate_expr(*rhs, env.clone())?;
                    evaluate_binary(op, left, right, env)
                }
            }
        }
        Expr::Unary(op, expr) => {
            let atom = evaluate_expr(*expr, env)?;
            evaluate_unary(op, atom)
        }
        Expr::Literal(a) => {
            if let Atom::Identifier(ref id) = a {
                if let Some(var) = env.get_var(&id) {
                    if let Atom::Nil = var {
                        Ok(a)
                    } else {
                        Ok(var)
                    }
                } else {
                    anyhow::bail!(RuntimeError::UndefinedVar { id: id.to_string() })
                }
            } else {
                Ok(a)
            }
        }
    }
}

fn evaluate_binary(op: TokenType, lhs: Atom, rhs: Atom, env: Env) -> Result<Atom> {
    Ok(match op {
        TokenType::Equal => match (lhs, rhs) {
            (Atom::Identifier(a), Atom::Identifier(b)) => {
                if let Some(_) = env.get_var(&a) {
                    if let Some(right_id) = env.get_var(&b) {
                        env.set_var(a, right_id);
                        Atom::Boolean(true)
                    } else {
                        anyhow::bail!(RuntimeError::UndefinedVar { id: b })
                    }
                } else {
                    anyhow::bail!(RuntimeError::UndefinedVar { id: a })
                }
            }
            (Atom::Identifier(a), b) => {
                if let Some(_) = env.get_var(&a) {
                    env.set_var(a, b);
                    Atom::Boolean(true)
                } else {
                    anyhow::bail!(RuntimeError::UndefinedVar { id: a })
                }
            }
            (a, b) => anyhow::bail!(RuntimeError::UnsupportedAssignment {
                type_a: a.to_string(),
                type_b: b.to_string()
            }),
        },
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
        TokenType::And => match (lhs, rhs) {
            (Atom::Boolean(a), Atom::Boolean(b)) => Atom::Boolean(a && b),
            (a, b) => anyhow::bail!(RuntimeError::InvalidOperand {
                op: "and".to_string(),
                lhs: a,
                rhs: b
            }),
        },
        TokenType::Or => match (lhs, rhs) {
            (Atom::Boolean(a), Atom::Boolean(b)) => Atom::Boolean(a || b),
            (a, b) => anyhow::bail!(RuntimeError::InvalidOperand {
                op: "or".to_string(),
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
        var undefined = nil;
        var a;
        print i;
        print boolean;
        print a;
        print b;
        print undefined;
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

    #[test]
    fn test_chapter8_challenge2() {
        let mut lexer = Lexer::new();
        let input = r#"
        // No initializers.
        var a;
        var b;
        var c;

        a = "assigned";
        print a; // OK, was assigned first.

        print b; // Nil
        print c = 2; // print true
        print c; // prints 2
        "#;
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_chapter8_challenge3() {
        let mut lexer = Lexer::new();
        let input = r#"
        var a = 1;
        {
            var a = a + 2;
            print a;
        }
        "#;
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_logical_operators() {
        let mut lexer = Lexer::new();
        let input = r#"
        var a = true;
        var b = false;

        //short circuit
        print a or b;
        print b and a;
        "#;
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_if_else_statment() {
        let mut lexer = Lexer::new();
        let input = r#"
        if (true) {
            print "hello!";
        } else {
            print "bye";
        }
        "#;
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        let mut parser = Parser::new(tokens);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }
}

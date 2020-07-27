use super::{
    ast::{Atom, Declaration, Expr, Program, Stmt},
    environment::{Object, Parental, Spaghetti},
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
    #[error("Function declaration {found:?} not found")]
    FunctionDeclarationNotFound { found: String },
    #[error("Invalid callee type {found:?}")]
    InvalidCalleeType { found: Atom },
    #[error("Invalid number of arguments provided. Found {found:?}, required {req:?}.")]
    InvalidNumberOfArgs { found: usize, req: usize },
}

/// Stupid way to return from function
#[derive(Error, Debug)]
enum Return {
    // Call expr will catch this
    #[error("Unable to return from function context, value - {0}")]
    Return(Atom)
}

type Env = Spaghetti;

pub fn interpret(program: Program) -> Result<()> {
    let env = Env::new_instance();
    for decl in program.declarations {
        evaluate_declaration(&decl, &env)?;
    }
    Ok(())
}

fn evaluate_declaration(decl: &Declaration, env: &Env) -> Result<()> {
    Ok(match decl {
        Declaration::FunDecl(id, arity, body) => {
            env.declare(id.to_owned(), Object::Function(arity.to_owned(), body.to_owned()))
        },
        Declaration::VarDecl(id, stmt) => {
            if let Stmt::ExprStmt(expr) = stmt {
                match expr {
                    Expr::Binary(_, _, rhs) => {
                        // declare and assign
                        let val = evaluate_expr(&rhs, &env)?;
                        env.declare(id.to_owned(), Object::Atom(val));
                    }
                    Expr::Literal(_) => {
                        // declaration, no assignment
                        env.declare(id.to_owned(), Object::Atom(Atom::Nil))
                    }
                    expr => {
                        anyhow::bail!(RuntimeError::InvalidVarDeclaration { expr: expr.clone() })
                    }
                }
            }
        }
        Declaration::Statement(stmt) => evaluate_statement(stmt, &env)?,
    })
}

fn evaluate_statement(stmt: &Stmt, env: &Env) -> Result<()> {
    match stmt {
        Stmt::ExprStmt(expr) => {
            evaluate_expr(&expr, &env)?;
        }
        Stmt::IfStmt(cond, stmt, else_stmt) => {
            match evaluate_expr(&cond, &env)? {
                Atom::Boolean(true) => {
                    // execute main stmt (or block)
                    evaluate_statement(&stmt, env)?
                }
                Atom::Boolean(false) => {
                    if let Some(else_stmt) = else_stmt {
                        evaluate_statement(&else_stmt, env)?
                    }
                }
                res => anyhow::bail!(RuntimeError::InvalidIfCondition { found: res }),
            }
        }
        Stmt::WhileStmt(cond, stmt) => {
            while let Atom::Boolean(true) = evaluate_expr(&cond, &env)? {
                evaluate_statement(&stmt, env)?
            }
        },
        Stmt::ReturnStmt(expr) => {
            let mut val = Atom::Nil;
            if let Some(expr) = expr {
                val = evaluate_expr(expr, env)?;
            }
            anyhow::bail!(Return::Return(val))
        }
        Stmt::PrintStmt(expr) => {
            if let Expr::Literal(atom) = expr {
                let value = if let Atom::Identifier(id) = atom {
                    if let Some(var) = env.get(id.as_str()) {
                        var.to_string()
                    } else {
                        anyhow::bail!(RuntimeError::UndefinedVar { id: id.to_owned() })
                    }
                } else {
                    atom.to_string()
                };
                println!("{}", value);
            } else {
                let val = evaluate_expr(&expr, &env)?;
                println!("{}", val);
            }
        }
        Stmt::Block(decls) => {
            let block_scope = env.child();
            for decl in decls {
                evaluate_declaration(decl, &block_scope)?
            }
        }
    }
    Ok(())
}

fn evaluate_expr(expr: &Expr, env: &Env) -> Result<Atom> {
    match expr {
        Expr::Grouping(expr) => evaluate_expr(&expr, env),
        Expr::Binary(lhs, op, rhs) => {
            // Special case for assignment
            if let (Expr::Literal(Atom::Identifier(id)), TokenType::Equal) = (lhs.as_ref(), op) {
                if env.get(id).is_some() {
                    let right = evaluate_expr(rhs, env)?;
                    env.update(id.to_string(), Object::Atom(right));
                    return Ok(Atom::Boolean(true));
                } else {
                    anyhow::bail!(RuntimeError::UndefinedVar { id: id.to_string() })
                }
            }

            let left = evaluate_expr(&lhs, env)?;
            match (op, left) {
                // Short circuiting for logic operations
                (TokenType::Or, Atom::Boolean(true)) => return Ok(Atom::Boolean(true)),
                (TokenType::And, Atom::Boolean(false)) => return Ok(Atom::Boolean(false)),
                (op, left) => {
                    let right = evaluate_expr(&rhs, env)?;
                    evaluate_binary(op, left, right, env)
                }
            }
        }
        Expr::Unary(op, expr) => {
            let atom = evaluate_expr(&expr, env)?;
            evaluate_unary(op, atom)
        }
        Expr::Call(callee_expr, args) => {
            // evaluate callee expression
            let callee = evaluate_expr(callee_expr, env)?;
            // validate that the callee is an identifier
            // TODO: possibly support some built-in functions on other Atom types
            if let Atom::Identifier(id) = callee {
                // get the function from the env, verify it is a function
                if let Some(Object::Function(params, stmt)) = env.get(&id) {
                    // create new env, pass args to it
                    let func_scope = env.child();
                    if let Some(args) = args {
                        // check arity
                        anyhow::ensure!(params.len() == args.len(), RuntimeError::InvalidNumberOfArgs {found: args.len(), req: params.len()});
                        for (arg, id) in args.iter().zip(params) {
                            let val = evaluate_expr(arg, env)?;
                            func_scope.declare(id, Object::Atom(val))
                        }
                    }
                    // evaluate function block
                    let ret = evaluate_statement(&stmt,&func_scope);
                    if let Err(e) = ret {
                        // get return value
                        if let Some(Return::Return(val)) = e.downcast_ref::<Return>() {
                            Ok(val.clone())
                        } else {
                            anyhow::bail!(e)
                        }
                    } else {
                        // void return
                        Ok(Atom::Nil)
                    }
                } else {
                    // func declaration not found
                    anyhow::bail!(RuntimeError::FunctionDeclarationNotFound { found: id})
                }
            } else {
                // invalid callee type
                anyhow::bail!(RuntimeError::InvalidCalleeType {found: callee})
            }
        },
        Expr::Literal(a) => {
            if let Atom::Identifier(ref id) = a {
                if let Some(object) = env.get(&id) {
                    match object {
                        Object::Atom(val) => {
                            if let Atom::Nil = val {
                                Ok(a.to_owned())
                            } else {
                                Ok(val.to_owned())
                            }
                        }
                        Object::Function(params, stmt) => {
                            Ok(Atom::Identifier(id.to_string()))
                        },
                    }
                } else {
                    anyhow::bail!(RuntimeError::UndefinedVar { id: id.to_string() })
                }
            } else {
                Ok(a.to_owned())
            }
        }
    }
}

fn evaluate_binary(op: &TokenType, lhs: Atom, rhs: Atom, env: &Env) -> Result<Atom> {
    Ok(match op {
        // TokenType::Equal => match (lhs, rhs) {
        //     (Atom::Identifier(a), Atom::Identifier(b)) => {
        //         if let Some(_) = env.get_var(&a) {
        //             if let Some(right_id) = env.get_var(&b) {
        //                 env.set_var(a, right_id);
        //                 Atom::Boolean(true)
        //             } else {
        //                 anyhow::bail!(RuntimeError::UndefinedVar { id: b })
        //             }
        //         } else {
        //             anyhow::bail!(RuntimeError::UndefinedVar { id: a })
        //         }
        //     }
        //     (Atom::Identifier(a), b) => {
        //         if let Some(_) = env.get_var(&a) {
        //             env.set_var(a, b);
        //             Atom::Boolean(true)
        //         } else {
        //             anyhow::bail!(RuntimeError::UndefinedVar { id: a })
        //         }
        //     }
        //     (a, b) => anyhow::bail!(RuntimeError::UnsupportedAssignment {
        //         type_a: a.to_string(),
        //         type_b: b.to_string()
        //     }),
        // },
        TokenType::Plus => match (lhs, rhs) {
            (Atom::Number(a), Atom::Number(b)) => Atom::Number(a + b),
            (Atom::String(a), Atom::String(b)) => Atom::String(a + &b),
            (Atom::String(a), Atom::Number(b)) => Atom::String(a + &b.to_string()),
            (Atom::Number(a), Atom::String(b)) => Atom::String(a.to_string() + &b),
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
        TokenType::Dot => match (lhs, rhs) {
            (Atom::Identifier(a), Atom::Identifier(b)) => todo!(),
            (a, b) => anyhow::bail!(RuntimeError::InvalidOperand {
                op: ".".to_string(),
                lhs: a,
                rhs: b
            }),
        },
        op => anyhow::bail!(RuntimeError::InvalidOperator { op: op.to_owned() }),
    })
}

fn evaluate_unary(op: &TokenType, atom: Atom) -> Result<Atom> {
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
        op => anyhow::bail!(RuntimeError::InvalidOperator { op: op.to_owned() }),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::*, parser::*};

    fn parser_setup(input: &str) -> Parser {
        let mut lexer = Lexer::new();
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        Parser::new(tokens)
    }

    #[test]
    fn test_simple_expression_eval() {
        let mut parser = parser_setup("1 + 1");
        let expr = expr(&mut parser).unwrap();
        let atom = evaluate_expr(&expr, &Env::new_instance()).unwrap();
        assert_eq!(atom.to_string(), "2")
    }

    #[test]
    fn test_bang_bool() {
        let mut parser = parser_setup("!true");
        let expr = expr(&mut parser).unwrap();
        let atom = evaluate_expr(&expr, &Env::new_instance()).unwrap();
        assert_eq!(atom.to_string(), "false")
    }

    #[test]
    fn test_grouped_expr() {
        let mut parser = parser_setup("(2 + 2) * 2 / 2");
        let expr = expr(&mut parser).unwrap();
        let atom = evaluate_expr(&expr, &Env::new_instance()).unwrap();
        assert_eq!(atom.to_string(), "4")
    }

    #[test]
    fn test_nested_grouped_expr() {
        let mut parser = parser_setup("((((((((8))))))))");
        let expr = expr(&mut parser).unwrap();
        let atom = evaluate_expr(&expr, &Env::new_instance()).unwrap();
        assert_eq!(atom.to_string(), "8")
    }

    #[test]
    fn test_string_concat() {
        let mut parser = parser_setup(r#""hello " + "world""#);
        let expr = expr(&mut parser).unwrap();
        let atom = evaluate_expr(&expr, &Env::new_instance()).unwrap();
        assert_eq!(atom.to_string(), "hello world")
    }

    #[test]
    fn test_less_than() {
        let mut parser = parser_setup("99 < 100");
        let expr = expr(&mut parser).unwrap();
        let atom = evaluate_expr(&expr, &Env::new_instance()).unwrap();
        assert_eq!(atom.to_string(), "true")
    }

    #[test]
    fn test_var_declaration_and_print() {
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
        let mut parser = parser_setup(input);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_block_scope() {
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
        let mut parser = parser_setup(input);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_chapter8_challenge2() {
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
        let mut parser = parser_setup(input);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_chapter8_challenge3() {
        let input = r#"
        var a = 1;
        {
            var a = a + 2;
            print a;
        }
        print a;
        "#;
        let mut parser = parser_setup(input);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_logical_operators() {
        let input = r#"
        var a = true;
        var b = false;

        //short circuit
        print a or b;
        print b and a;
        "#;
        let mut parser = parser_setup(input);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_if_else_statment() {
        let input = r#"
        if (true) {
            print "hello!";
        } else {
            print "bye";
        }

        var a = 5;
        if (a == 2) {
            // unreachable
            print a;
        }
        "#;
        let mut parser = parser_setup(input);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_while_loop() {
        let input = r#"
        var x = 0;
        while(x < 3) {
            print x;
            x = x + 1;
        }
        "#;
        let mut parser = parser_setup(input);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_for_loop() {
        let input = r#"
        for(var i = 0; i < 3; i = i + 1){
            print i;
        }

        for(var i = 1; i < 3; i = i + 1)
            print i;
        "#;
        let mut parser = parser_setup(input);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_function_declaration_and_call() {
        let input = r#"
        var a = "hello world";
        fun foo() {
            print a;
            return 1;
        }
        var r = foo();
        print r;
        "#;
        let mut parser = parser_setup(input);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_fibonacci() {
        let input = r#"
        fun fib(n) {
            if (n <= 1) return n;
            return fib(n - 2) + fib(n - 1);
        }
          
        for (var i = 0; i < 20; i = i + 1) {
            print fib(i);
        }
        "#;
        let mut parser = parser_setup(input);
        let program = parse(&mut parser).unwrap();
        let result = interpret(program);
        assert!(result.is_ok());
    }
}

use super::ast::{Atom, Declaration, Expr, Program, Scope, Stmt};
use super::tokens::TokenType;
use std::collections::HashMap;
use thiserror::Error;

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
}

#[derive(Error, Debug)]
pub enum ResolverError {
    #[error("Cannot read local variable {id:?} in its own initializer.")]
    UsingVarInOwnInitializer { id: String },
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver { scopes: Vec::new() }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn resolve(&mut self, program: &mut Program) -> Result<(), ResolverError> {
        Ok(for decl in program.declarations.iter_mut() {
            self.resolve_declaration(decl)?;
        })
    }

    fn resolve_declaration(&mut self, decl: &mut Declaration) -> Result<(), ResolverError> {
        match decl {
            Declaration::FunDecl(id, params, body) => self.resolve_func_decl(id, params, body)?,
            Declaration::VarDecl(id, stmt) => self.resolve_var_decl(id, stmt)?,
            Declaration::Statement(stmt) => {
                self.resolve_stmt(stmt)?;
            }
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &mut Stmt) -> Result<(), ResolverError> {
        match stmt {
            Stmt::ExprStmt(expr) => self.resolve_expr(expr),
            Stmt::IfStmt(cond, then_stmt, else_stmt) => {
                self.resolve_expr(cond);
                self.resolve_stmt(then_stmt)?;
                if let Some(else_stmt) = else_stmt {
                    self.resolve_stmt(else_stmt)?;
                }
            }
            Stmt::WhileStmt(cond, stmt) => {
                self.resolve_expr(cond);
                self.resolve_stmt(stmt)?;
            }
            Stmt::ReturnStmt(expr) => {
                if let Some(expr) = expr {
                    self.resolve_expr(expr);
                }
            }
            Stmt::PrintStmt(expr) => self.resolve_expr(expr),
            Stmt::Block(decls) => {
                self.resolve_block(decls)?;
            }
        }
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Grouping(expr) => self.resolve_expr(expr),
            Expr::Binary(lhs, op, rhs) => {
                self.resolve_expr(lhs);
                self.resolve_expr(rhs);
            }
            Expr::Unary(_, expr) => self.resolve_expr(expr),
            Expr::Call(callee, args) => {
                self.resolve_expr(callee);
                if let Some(args) = args {
                    for arg in args {
                        self.resolve_expr(arg);
                    }
                }
            }
            Expr::Literal(id) => self.resolve_local(id),
        }
    }

    fn resolve_block(&mut self, decls: &mut Vec<Declaration>) -> Result<(), ResolverError> {
        self.enter_scope();
        for decl in decls {
            self.resolve_declaration(decl)?;
        }
        self.exit_scope();
        Ok(())
    }

    fn resolve_func_decl(
        &mut self,
        fun_id: &str,
        params: &Vec<String>,
        stmt: &mut Stmt,
    ) -> Result<(), ResolverError> {
        self.declare(fun_id.to_string());
        self.define(fun_id.to_string());

        self.resolve_function(&params, stmt)?;
        Ok(())
    }

    fn resolve_function(
        &mut self,
        params: &Vec<String>,
        stmt: &mut Stmt,
    ) -> Result<(), ResolverError> {
        self.enter_scope();
        for param in params {
            self.declare(param.to_string());
            self.define(param.to_string());
        }
        self.resolve_stmt(stmt)?;
        self.exit_scope();
        Ok(())
    }

    fn resolve_var_decl(&mut self, var_id: &str, stmt: &mut Stmt) -> Result<(), ResolverError> {
        // declare
        self.declare(var_id.to_string());
        // resolve initializer expr
        if let Stmt::ExprStmt(expr) = stmt {
            // can only be Binary or Literal. Literal means it's declared but not defined.
            if let Expr::Binary(lhs, op, rhs) = expr {
                self.resolve_expr(rhs);
                if let Expr::Literal(atom) = lhs.as_mut() {
                    self.resolve_local(atom);
                }
            }
        }
        // define
        self.define(var_id.to_string());
        Ok(())
    }

    fn resolve_local(&mut self, identifier: &mut Atom) {
        if let Atom::Identifier(id, scope) = identifier {
            for (depth, s) in self.scopes.iter().rev().enumerate() {
                if s.contains_key(id) {
                    if let Some(false) = s.get(id) {
                        // return Err(ResolverError::UsingVarInOwnInitializer { id: id.to_string() })
                    }
                    *scope = Scope::Distance(depth);
                    return;
                }
            }
            // is global
        }
    }

    fn declare(&mut self, id: String) {
        if let Some(last) = self.scopes.last_mut() {
            last.insert(id, false);
        }
    }

    fn define(&mut self, id: String) {
        if let Some(last) = self.scopes.last_mut() {
            last.insert(id, true);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{interpreter::Interpreter, lexer::*, parser::*};
    fn parser_setup(input: &str) -> Parser {
        let mut lexer = Lexer::new();
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let tokens = lexer.get_tokens();
        Parser::new(tokens)
    }
}

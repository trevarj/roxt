use super::Expr;
#[derive(Debug, Clone)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    /// Function identifier, Parameters, Body
    FunDecl(String, Vec<String>, Stmt),
    /// Variable identifier, Expression
    VarDecl(String, Stmt),
    /// Any other statement
    Statement(Stmt),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Expression
    ExprStmt(Expr),
    /// Condition, If body, Else body
    IfStmt(Expr, Box<Stmt>, Option<Box<Stmt>>),
    /// Condition, body
    WhileStmt(Expr, Box<Stmt>),
    /// Return expression
    ReturnStmt(Option<Expr>),
    /// Expression to print
    PrintStmt(Expr),
    /// Block body
    Block(Vec<Declaration>),
}

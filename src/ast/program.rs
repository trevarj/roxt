use super::Expr;
#[derive(Debug)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    FunDecl(String, Vec<String>, Stmt),
    VarDecl(String, Stmt),
    Statement(Stmt),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ExprStmt(Expr),
    IfStmt(Expr, Box<Stmt>, Option<Box<Stmt>>),
    WhileStmt(Expr, Box<Stmt>),
    ReturnStmt(Option<Expr>),
    PrintStmt(Expr),
    Block(Vec<Declaration>),
}

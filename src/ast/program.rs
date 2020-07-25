use super::Expr;
#[derive(Debug)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug)]
pub enum Declaration {
    VarDecl(String, Stmt),
    Statement(Stmt),
}

#[derive(Debug)]
pub enum Stmt {
    ExprStmt(Expr),
    IfStmt(Expr, Box<Stmt>, Option<Box<Stmt>>),
    WhileStmt(Expr, Box<Stmt>),
    PrintStmt(Expr),
    Block(Vec<Declaration>),
}

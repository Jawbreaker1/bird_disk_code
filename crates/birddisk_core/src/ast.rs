use crate::diagnostics::Span;

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    I64,
    Bool,
    Array(Box<Type>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Set {
        name: String,
        ty: Option<Type>,
        expr: Expr,
        span: Span,
    },
    Put {
        name: String,
        expr: Expr,
        span: Span,
    },
    PutIndex {
        name: String,
        index: Expr,
        expr: Expr,
        span: Span,
    },
    Yield {
        expr: Expr,
        span: Span,
    },
    When {
        cond: Expr,
        span: Span,
        then_body: Vec<Stmt>,
        else_body: Vec<Stmt>,
    },
    Repeat {
        cond: Expr,
        span: Span,
        body: Vec<Stmt>,
    },
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(i64),
    Bool(bool),
    Ident(String),
    Call { name: String, args: Vec<Expr> },
    ArrayLit(Vec<Expr>),
    ArrayNew { len: Box<Expr> },
    Index { base: Box<Expr>, index: Box<Expr> },
    Unary { op: UnaryOp, expr: Box<Expr> },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    I64(i64),
    Bool(bool),
    Array { elements: Vec<Value>, elem_type: Type },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    EqEq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    AndAnd,
    OrOr,
}

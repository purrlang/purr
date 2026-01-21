use crate::span::Span;

#[derive(Debug, Clone)]
pub struct Program {
    pub namespace: NamespaceDecl,
    pub uses: Vec<UseDecl>,
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub struct NamespaceDecl {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UseDecl {
    pub name: String,
    pub alias: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Struct(StructDecl),
    Enum(EnumDecl),
    Func(FuncDecl),
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub name: String,
    pub fields: Vec<Field>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub name: String,
    pub cases: Vec<EnumCase>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumCase {
    pub name: String,
    pub fields: Vec<Field>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: FunctionName,
    pub params: Vec<Param>,
    pub ret: Option<Type>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum FunctionName {
    Free(String),
    Method { type_name: String, method: String },
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Var(VarDecl),
    Assign(AssignStmt),
    Return(ReturnStmt),
    Expr(ExprStmt),
    Block(Block),
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub ty: Option<Type>,
    pub init: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AssignStmt {
    pub target: LValue,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum LValue {
    Name(String, Span),
    Field { base: Box<LValue>, name: String, span: Span },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(String, Span),
    Float(String, Span),
    String(String, Span),
    Bool(bool, Span),
    Nil(Span),
    Name(String, Span),
    StructLit { name: String, fields: Vec<StructInit>, span: Span },
    Call { callee: Box<Expr>, args: Vec<Expr>, span: Span },
    Field { base: Box<Expr>, name: String, span: Span },
    Unary { op: UnaryOp, expr: Box<Expr>, span: Span },
    Binary { op: BinaryOp, left: Box<Expr>, right: Box<Expr>, span: Span },
    Group { expr: Box<Expr>, span: Span },
}

#[derive(Debug, Clone)]
pub struct StructInit {
    pub name: String,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
}

#[derive(Debug, Clone)]
pub enum Type {
    Name(String, Span),
    Ctor { name: String, args: Vec<Type>, span: Span },
    Optional { inner: Box<Type>, span: Span },
    Func { params: Vec<Type>, ret: Box<Type>, span: Span },
}

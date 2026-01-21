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
    Actor(ActorDecl),
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ActorDecl {
    pub name: String,
    pub span: Span,
}

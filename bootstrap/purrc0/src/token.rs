use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Namespace,
    Use,
    Struct,
    Enum,
    Interface,
    Actor,
    On,
    Spawn,
    Func,
    Var,
    Return,
    If,
    Else,
    For,
    Switch,
    Case,
    Nil,
    True,
    False,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Ident(String),
    Int(String),
    Float(String),
    String(String),
    Keyword(Keyword),
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Dot,
    Colon,
    Semicolon,
    Eq,
    Arrow,
    FatArrow,
    Lt,
    Gt,
    Plus,
    Minus,
    Star,
    Slash,
    Bang,
    AmpAmp,
    PipePipe,
    EqEq,
    BangEq,
    LtEq,
    GtEq,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

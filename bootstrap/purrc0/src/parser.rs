use crate::ast::{
    AssignStmt, BinaryOp, Block, Declaration, EnumCase, EnumDecl, Expr, ExprStmt, Field,
    FunctionName, FuncDecl, LValue, NamespaceDecl, Param, Program, ReturnStmt, Stmt,
    StructDecl, StructInit, Type, UnaryOp, UseDecl, VarDecl,
};
use crate::error::Error;
use crate::span::Span;
use crate::token::{Keyword, Token, TokenKind};

pub fn parse_program(tokens: &[Token]) -> Result<Program, Error> {
    let mut p = Parser::new(tokens);
    let namespace = p.parse_namespace()?;
    let mut uses = Vec::new();
    while p.matches_keyword(Keyword::Use) {
        uses.push(p.parse_use()?);
    }

    let mut declarations = Vec::new();
    while !p.is_eof() {
        declarations.push(p.parse_declaration()?);
    }

    Ok(Program { namespace, uses, declarations })
}

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, pos: 0 }
    }

    fn parse_namespace(&mut self) -> Result<NamespaceDecl, Error> {
        self.expect_keyword(Keyword::Namespace)?;
        let start = self.prev_span().start;
        let name = self.parse_qualified_name()?;
        let end = self.expect(TokenKind::Semicolon)?.span.end;
        Ok(NamespaceDecl {
            name,
            span: Span::new(start, end),
        })
    }

    fn parse_use(&mut self) -> Result<UseDecl, Error> {
        let use_tok = self.prev_span();
        let name = self.parse_qualified_name()?;
        let alias = if self.matches(TokenKind::Eq) {
            let ident = self.expect_ident()?;
            Some(ident)
        } else {
            None
        };
        let end = self.expect(TokenKind::Semicolon)?.span.end;
        Ok(UseDecl {
            name,
            alias,
            span: Span::new(use_tok.start, end),
        })
    }

    fn parse_declaration(&mut self) -> Result<Declaration, Error> {
        match self.peek_keyword() {
            Some(Keyword::Struct) => Ok(Declaration::Struct(self.parse_struct_decl()?)),
            Some(Keyword::Enum) => Ok(Declaration::Enum(self.parse_enum_decl()?)),
            Some(Keyword::Func) => Ok(Declaration::Func(self.parse_func_decl()?)),
            Some(Keyword::Interface) => Err(self.error_here("interface not supported in purrc0")),
            Some(Keyword::Actor) => Err(self.error_here("actor not supported in purrc0")),
            _ => Err(self.error_here("expected declaration")),
        }
    }

    fn parse_struct_decl(&mut self) -> Result<StructDecl, Error> {
        self.expect_keyword(Keyword::Struct)?;
        let start = self.prev_span().start;
        let name = self.expect_ident()?;
        self.expect(TokenKind::LBrace)?;
        let mut fields = Vec::new();
        while !self.matches(TokenKind::RBrace) {
            let ty = self.parse_type()?;
            let field_name = self.expect_ident()?;
            let end = self.expect(TokenKind::Semicolon)?.span.end;
            let span = Span::new(ty.span_start(), end);
            fields.push(Field { name: field_name, ty, span });
        }
        let end = self.prev_span().end;
        Ok(StructDecl { name, fields, span: Span::new(start, end) })
    }

    fn parse_enum_decl(&mut self) -> Result<EnumDecl, Error> {
        self.expect_keyword(Keyword::Enum)?;
        let start = self.prev_span().start;
        let name = self.expect_ident()?;
        self.expect(TokenKind::LBrace)?;
        let mut cases = Vec::new();
        while !self.matches(TokenKind::RBrace) {
            self.expect_keyword(Keyword::Case)?;
            let case_start = self.prev_span().start;
            let case_name = self.expect_ident()?;
            let mut fields = Vec::new();
            if self.matches(TokenKind::LParen) {
                if !self.matches(TokenKind::RParen) {
                    loop {
                        let param_start = self.peek_span().unwrap_or(Span::new(0, 0)).start;
                        let field_name = self.expect_ident()?;
                        self.expect(TokenKind::Colon)?;
                        let ty = self.parse_type()?;
                        let span = Span::new(param_start, ty.span_end());
                        fields.push(Field { name: field_name, ty, span });
                        if self.matches(TokenKind::Comma) {
                            continue;
                        }
                        self.expect(TokenKind::RParen)?;
                        break;
                    }
                }
            }
            let end = self.expect(TokenKind::Semicolon)?.span.end;
            cases.push(EnumCase {
                name: case_name,
                fields,
                span: Span::new(case_start, end),
            });
        }
        let end = self.prev_span().end;
        Ok(EnumDecl { name, cases, span: Span::new(start, end) })
    }

    fn parse_func_decl(&mut self) -> Result<FuncDecl, Error> {
        self.expect_keyword(Keyword::Func)?;
        let start = self.prev_span().start;
        let (name, name_end) = self.parse_function_name()?;
        self.expect(TokenKind::LParen)?;
        let params = self.parse_param_list()?;
        self.expect(TokenKind::RParen)?;
        let ret = if self.peek_is_type_start() {
            Some(self.parse_type()?)
        } else {
            None
        };
        let body = self.parse_block()?;
        let end = body.span.end;
        Ok(FuncDecl {
            name,
            params,
            ret,
            body,
            span: Span::new(start, end.max(name_end)),
        })
    }

    fn parse_block(&mut self) -> Result<Block, Error> {
        let start = self.expect(TokenKind::LBrace)?.span.start;
        let mut statements = Vec::new();
        while !self.matches(TokenKind::RBrace) {
            statements.push(self.parse_statement()?);
        }
        let end = self.prev_span().end;
        Ok(Block { statements, span: Span::new(start, end) })
    }

    fn parse_statement(&mut self) -> Result<Stmt, Error> {
        if self.matches_keyword(Keyword::Var) {
            let start = self.prev_span().start;
            let name = self.expect_ident()?;
            let ty = if self.matches(TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.expect(TokenKind::Eq)?;
            let init = self.parse_expression()?;
            let end = self.expect(TokenKind::Semicolon)?.span.end;
            return Ok(Stmt::Var(VarDecl { name, ty, init, span: Span::new(start, end) }));
        }

        if self.matches_keyword(Keyword::Return) {
            let start = self.prev_span().start;
            let value = if self.peek_is_stmt_end() {
                None
            } else {
                Some(self.parse_expression()?)
            };
            let end = self.expect(TokenKind::Semicolon)?.span.end;
            return Ok(Stmt::Return(ReturnStmt { value, span: Span::new(start, end) }));
        }

        if self.matches(TokenKind::LBrace) {
            self.pos -= 1;
            return Ok(Stmt::Block(self.parse_block()?));
        }

        let expr = self.parse_expression()?;
        if self.matches(TokenKind::Eq) {
            let target = self.expr_to_lvalue(expr)?;
            let value = self.parse_expression()?;
            let end = self.expect(TokenKind::Semicolon)?.span.end;
            let span = Span::new(target.span_start(), end);
            return Ok(Stmt::Assign(AssignStmt { target, value, span }));
        }
        let end = self.expect(TokenKind::Semicolon)?.span.end;
        let span = Span::new(expr.span_start(), end);
        Ok(Stmt::Expr(ExprStmt { expr, span }))
    }

    fn parse_param_list(&mut self) -> Result<Vec<Param>, Error> {
        let mut params = Vec::new();
        if self.peek_is(TokenKind::RParen) {
            return Ok(params);
        }
        loop {
            let start = self.peek_span().unwrap_or(Span::new(0, 0)).start;
            let name = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;
            let span = Span::new(start, ty.span_end());
            params.push(Param { name, ty, span });
            if self.matches(TokenKind::Comma) {
                continue;
            }
            break;
        }
        Ok(params)
    }

    fn parse_function_name(&mut self) -> Result<(FunctionName, usize), Error> {
        let start = self.peek_span().unwrap_or(Span::new(0, 0)).start;
        let first = self.expect_ident()?;
        if self.matches(TokenKind::Dot) {
            let method = self.expect_ident()?;
            let end = self.prev_span().end;
            Ok((FunctionName::Method { type_name: first, method }, end))
        } else {
            Ok((FunctionName::Free(first), self.prev_span().end.max(start)))
        }
    }

    fn parse_type(&mut self) -> Result<Type, Error> {
        let ty = if self.matches(TokenKind::LParen) {
            let start = self.prev_span().start;
            let mut params = Vec::new();
            if !self.matches(TokenKind::RParen) {
                loop {
                    params.push(self.parse_type()?);
                    if self.matches(TokenKind::Comma) {
                        continue;
                    }
                    self.expect(TokenKind::RParen)?;
                    break;
                }
            }
            self.expect(TokenKind::Colon)?;
            let ret = self.parse_type()?;
            let span = Span::new(start, ret.span_end());
            Type::Func { params, ret: Box::new(ret), span }
        } else {
            let name_start = self.peek_span().unwrap_or(Span::new(0, 0)).start;
            let name = self.expect_ident()?;
            if self.matches(TokenKind::Lt) {
                let mut args = Vec::new();
                loop {
                    args.push(self.parse_type()?);
                    if self.matches(TokenKind::Comma) {
                        continue;
                    }
                    self.expect(TokenKind::Gt)?;
                    break;
                }
                let span = Span::new(name_start, self.prev_span().end);
                Type::Ctor { name, args, span }
            } else {
                let span = Span::new(name_start, self.prev_span().end);
                Type::Name(name, span)
            }
        };

        if self.matches(TokenKind::Question) {
            let span = Span::new(ty.span_start(), self.prev_span().end);
            Ok(Type::Optional { inner: Box::new(ty), span })
        } else {
            Ok(ty)
        }
    }

    fn parse_expression(&mut self) -> Result<Expr, Error> {
        self.parse_logical_or()
    }

    fn parse_logical_or(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_logical_and()?;
        while self.matches(TokenKind::PipePipe) {
            let op_span = self.prev_span();
            let rhs = self.parse_logical_and()?;
            let span = Span::new(expr.span_start(), rhs.span_end());
            expr = Expr::Binary { op: BinaryOp::Or, left: Box::new(expr), right: Box::new(rhs), span };
            let _ = op_span;
        }
        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_equality()?;
        while self.matches(TokenKind::AmpAmp) {
            let rhs = self.parse_equality()?;
            let span = Span::new(expr.span_start(), rhs.span_end());
            expr = Expr::Binary { op: BinaryOp::And, left: Box::new(expr), right: Box::new(rhs), span };
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_relational()?;
        loop {
            let op = if self.matches(TokenKind::EqEq) {
                Some(BinaryOp::Eq)
            } else if self.matches(TokenKind::BangEq) {
                Some(BinaryOp::Neq)
            } else {
                None
            };
            if let Some(op) = op {
                let rhs = self.parse_relational()?;
                let span = Span::new(expr.span_start(), rhs.span_end());
                expr = Expr::Binary { op, left: Box::new(expr), right: Box::new(rhs), span };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_relational(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_additive()?;
        loop {
            let op = if self.matches(TokenKind::Lt) {
                Some(BinaryOp::Lt)
            } else if self.matches(TokenKind::LtEq) {
                Some(BinaryOp::Lte)
            } else if self.matches(TokenKind::Gt) {
                Some(BinaryOp::Gt)
            } else if self.matches(TokenKind::GtEq) {
                Some(BinaryOp::Gte)
            } else {
                None
            };
            if let Some(op) = op {
                let rhs = self.parse_additive()?;
                let span = Span::new(expr.span_start(), rhs.span_end());
                expr = Expr::Binary { op, left: Box::new(expr), right: Box::new(rhs), span };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_additive(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_multiplicative()?;
        loop {
            let op = if self.matches(TokenKind::Plus) {
                Some(BinaryOp::Add)
            } else if self.matches(TokenKind::Minus) {
                Some(BinaryOp::Sub)
            } else {
                None
            };
            if let Some(op) = op {
                let rhs = self.parse_multiplicative()?;
                let span = Span::new(expr.span_start(), rhs.span_end());
                expr = Expr::Binary { op, left: Box::new(expr), right: Box::new(rhs), span };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_unary()?;
        loop {
            let op = if self.matches(TokenKind::Star) {
                Some(BinaryOp::Mul)
            } else if self.matches(TokenKind::Slash) {
                Some(BinaryOp::Div)
            } else {
                None
            };
            if let Some(op) = op {
                let rhs = self.parse_unary()?;
                let span = Span::new(expr.span_start(), rhs.span_end());
                expr = Expr::Binary { op, left: Box::new(expr), right: Box::new(rhs), span };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, Error> {
        if self.matches(TokenKind::Bang) {
            let op_span = self.prev_span();
            let expr = self.parse_unary()?;
            let span = Span::new(op_span.start, expr.span_end());
            return Ok(Expr::Unary { op: UnaryOp::Not, expr: Box::new(expr), span });
        }
        if self.matches(TokenKind::Minus) {
            let op_span = self.prev_span();
            let expr = self.parse_unary()?;
            let span = Span::new(op_span.start, expr.span_end());
            return Ok(Expr::Unary { op: UnaryOp::Neg, expr: Box::new(expr), span });
        }
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_primary()?;
        loop {
            if self.matches(TokenKind::LParen) {
                let mut args = Vec::new();
                if !self.matches(TokenKind::RParen) {
                    loop {
                        args.push(self.parse_expression()?);
                        if self.matches(TokenKind::Comma) {
                            continue;
                        }
                        self.expect(TokenKind::RParen)?;
                        break;
                    }
                }
                let span = Span::new(expr.span_start(), self.prev_span().end);
                expr = Expr::Call { callee: Box::new(expr), args, span };
                continue;
            }
            if self.matches(TokenKind::Dot) {
                let name = self.expect_ident()?;
                let span = Span::new(expr.span_start(), self.prev_span().end);
                expr = Expr::Field { base: Box::new(expr), name, span };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, Error> {
        if let Some(Token { kind: TokenKind::Int(value), span }) = self.tokens.get(self.pos) {
            let span = *span;
            let value = value.clone();
            self.pos += 1;
            return Ok(Expr::Int(value, span));
        }
        if let Some(Token { kind: TokenKind::Float(value), span }) = self.tokens.get(self.pos) {
            let span = *span;
            let value = value.clone();
            self.pos += 1;
            return Ok(Expr::Float(value, span));
        }
        if let Some(Token { kind: TokenKind::String(value), span }) = self.tokens.get(self.pos) {
            let span = *span;
            let value = value.clone();
            self.pos += 1;
            return Ok(Expr::String(value, span));
        }
        if self.matches_keyword(Keyword::True) {
            let span = self.prev_span();
            return Ok(Expr::Bool(true, span));
        }
        if self.matches_keyword(Keyword::False) {
            let span = self.prev_span();
            return Ok(Expr::Bool(false, span));
        }
        if self.matches_keyword(Keyword::Nil) {
            let span = self.prev_span();
            return Ok(Expr::Nil(span));
        }
        if self.matches(TokenKind::LParen) {
            let start = self.prev_span().start;
            let expr = self.parse_expression()?;
            let end = self.expect(TokenKind::RParen)?.span.end;
            return Ok(Expr::Group { expr: Box::new(expr), span: Span::new(start, end) });
        }
        if let Some(Token { kind: TokenKind::Ident(_), .. }) = self.tokens.get(self.pos) {
            let start = self.peek_span().unwrap_or(Span::new(0, 0)).start;
            let name = self.expect_ident()?;
            if self.matches(TokenKind::LBrace) {
                let mut fields = Vec::new();
                if !self.matches(TokenKind::RBrace) {
                    loop {
                        let field_name = self.expect_ident()?;
                        self.expect(TokenKind::Colon)?;
                        let value = self.parse_expression()?;
                        let span = Span::new(start, value.span_end());
                        fields.push(StructInit { name: field_name, value, span });
                        if self.matches(TokenKind::Comma) {
                            continue;
                        }
                        self.expect(TokenKind::RBrace)?;
                        break;
                    }
                }
                let end = self.prev_span().end;
                return Ok(Expr::StructLit { name, fields, span: Span::new(start, end) });
            }
            let span = Span::new(start, self.prev_span().end);
            return Ok(Expr::Name(name, span));
        }
        Err(self.error_here("expected expression"))
    }

    fn expr_to_lvalue(&self, expr: Expr) -> Result<LValue, Error> {
        match expr {
            Expr::Name(name, span) => Ok(LValue::Name(name, span)),
            Expr::Field { base, name, span } => {
                let base_lv = self.expr_to_lvalue(*base)?;
                Ok(LValue::Field { base: Box::new(base_lv), name, span })
            }
            _ => Err(self.error_here("invalid assignment target")),
        }
    }

    fn parse_qualified_name(&mut self) -> Result<String, Error> {
        let mut parts = Vec::new();
        parts.push(self.expect_ident()?);
        while self.matches(TokenKind::Dot) {
            parts.push(self.expect_ident()?);
        }
        Ok(parts.join("."))
    }

    fn peek_keyword(&self) -> Option<Keyword> {
        match self.tokens.get(self.pos) {
            Some(Token { kind: TokenKind::Keyword(k), .. }) => Some(*k),
            _ => None,
        }
    }

    fn peek_is_type_start(&self) -> bool {
        matches!(self.tokens.get(self.pos), Some(Token { kind: TokenKind::Ident(_), .. }) | Some(Token { kind: TokenKind::LParen, .. }))
    }

    fn peek_is(&self, kind: TokenKind) -> bool {
        matches!(self.tokens.get(self.pos), Some(Token { kind: k, .. }) if *k == kind)
    }

    fn peek_is_stmt_end(&self) -> bool {
        matches!(self.tokens.get(self.pos), Some(Token { kind: TokenKind::Semicolon, .. }))
    }

    fn matches_keyword(&mut self, kw: Keyword) -> bool {
        if let Some(Token { kind: TokenKind::Keyword(k), .. }) = self.tokens.get(self.pos) {
            if *k == kw {
                self.pos += 1;
                return true;
            }
        }
        false
    }

    fn expect_keyword(&mut self, kw: Keyword) -> Result<(), Error> {
        if self.matches_keyword(kw) {
            Ok(())
        } else {
            Err(self.error_here("expected keyword"))
        }
    }

    fn matches(&mut self, kind: TokenKind) -> bool {
        if let Some(tok) = self.tokens.get(self.pos) {
            if tok.kind == kind {
                self.pos += 1;
                return true;
            }
        }
        false
    }

    fn expect(&mut self, kind: TokenKind) -> Result<&Token, Error> {
        if let Some(tok) = self.tokens.get(self.pos) {
            if tok.kind == kind {
                self.pos += 1;
                return Ok(tok);
            }
        }
        Err(self.error_here("unexpected token"))
    }

    fn expect_ident(&mut self) -> Result<String, Error> {
        if let Some(Token { kind: TokenKind::Ident(name), .. }) = self.tokens.get(self.pos) {
            let out = name.clone();
            self.pos += 1;
            Ok(out)
        } else {
            Err(self.error_here("expected identifier"))
        }
    }

    fn prev_span(&self) -> Span {
        if self.pos == 0 {
            Span::new(0, 0)
        } else {
            self.tokens[self.pos - 1].span
        }
    }

    fn peek_span(&self) -> Option<Span> {
        self.tokens.get(self.pos).map(|t| t.span)
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn error_here(&self, msg: &str) -> Error {
        let span = self.peek_span().unwrap_or(Span::new(0, 0));
        Error::new(msg, Some(span))
    }
}

trait SpanExt {
    fn span_start(&self) -> usize;
    fn span_end(&self) -> usize;
}

impl SpanExt for Type {
    fn span_start(&self) -> usize {
        match self {
            Type::Name(_, span) => span.start,
            Type::Ctor { span, .. } => span.start,
            Type::Optional { span, .. } => span.start,
            Type::Func { span, .. } => span.start,
        }
    }

    fn span_end(&self) -> usize {
        match self {
            Type::Name(_, span) => span.end,
            Type::Ctor { span, .. } => span.end,
            Type::Optional { span, .. } => span.end,
            Type::Func { span, .. } => span.end,
        }
    }
}

impl SpanExt for Expr {
    fn span_start(&self) -> usize {
        match self {
            Expr::Int(_, span)
            | Expr::Float(_, span)
            | Expr::String(_, span)
            | Expr::Bool(_, span)
            | Expr::Nil(span)
            | Expr::Name(_, span)
            | Expr::StructLit { span, .. }
            | Expr::Call { span, .. }
            | Expr::Field { span, .. }
            | Expr::Unary { span, .. }
            | Expr::Binary { span, .. }
            | Expr::Group { span, .. } => span.start,
        }
    }

    fn span_end(&self) -> usize {
        match self {
            Expr::Int(_, span)
            | Expr::Float(_, span)
            | Expr::String(_, span)
            | Expr::Bool(_, span)
            | Expr::Nil(span)
            | Expr::Name(_, span)
            | Expr::StructLit { span, .. }
            | Expr::Call { span, .. }
            | Expr::Field { span, .. }
            | Expr::Unary { span, .. }
            | Expr::Binary { span, .. }
            | Expr::Group { span, .. } => span.end,
        }
    }
}

impl SpanExt for LValue {
    fn span_start(&self) -> usize {
        match self {
            LValue::Name(_, span) => span.start,
            LValue::Field { span, .. } => span.start,
        }
    }

    fn span_end(&self) -> usize {
        match self {
            LValue::Name(_, span) => span.end,
            LValue::Field { span, .. } => span.end,
        }
    }
}

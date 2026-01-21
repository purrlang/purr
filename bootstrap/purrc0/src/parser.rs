use crate::ast::{Declaration, NamespaceDecl, Program, UseDecl};
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

    if !p.is_eof() {
        let span = p.peek_span().unwrap_or(Span::new(0, 0));
        return Err(Error::new("declarations not implemented in purrc0", Some(span)));
    }

    Ok(Program {
        namespace,
        uses,
        declarations: Vec::new(),
    })
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

    fn parse_qualified_name(&mut self) -> Result<String, Error> {
        let mut parts = Vec::new();
        parts.push(self.expect_ident()?);
        while self.matches(TokenKind::Dot) {
            parts.push(self.expect_ident()?);
        }
        Ok(parts.join("."))
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

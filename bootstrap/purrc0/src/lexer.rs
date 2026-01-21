use crate::error::Error;
use crate::span::Span;
use crate::token::{Keyword, Token, TokenKind};

pub fn lex(source: &str) -> Result<Vec<Token>, Error> {
    let mut tokens = Vec::new();
    let bytes = source.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        let b = bytes[i];

        if is_whitespace(b) {
            i += 1;
            continue;
        }

        if b == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
            i += 2;
            while i < bytes.len() && bytes[i] != b'\n' {
                i += 1;
            }
            continue;
        }

        let start = i;

        if is_ident_start(b) {
            i += 1;
            while i < bytes.len() && is_ident_part(bytes[i]) {
                i += 1;
            }
            let text = &source[start..i];
            let kind = match_keyword(text).unwrap_or_else(|| TokenKind::Ident(text.to_string()));
            tokens.push(Token { kind, span: Span::new(start, i) });
            continue;
        }

        if is_digit(b) || (b == b'-' && i + 1 < bytes.len() && is_digit(bytes[i + 1])) {
            i += 1;
            while i < bytes.len() && is_digit(bytes[i]) {
                i += 1;
            }
            if i < bytes.len() && bytes[i] == b'.' {
                i += 1;
                let frac_start = i;
                while i < bytes.len() && is_digit(bytes[i]) {
                    i += 1;
                }
                if frac_start == i {
                    return Err(Error::new("invalid float literal", Some(Span::new(start, i))));
                }
                let text = &source[start..i];
                tokens.push(Token { kind: TokenKind::Float(text.to_string()), span: Span::new(start, i) });
            } else {
                let text = &source[start..i];
                tokens.push(Token { kind: TokenKind::Int(text.to_string()), span: Span::new(start, i) });
            }
            continue;
        }

        if b == b'"' {
            i += 1;
            let mut value = String::new();
            while i < bytes.len() && bytes[i] != b'"' {
                if bytes[i] == b'\\' {
                    i += 1;
                    if i >= bytes.len() {
                        return Err(Error::new("unterminated string escape", Some(Span::new(start, i))));
                    }
                    let esc = bytes[i];
                    match esc {
                        b'n' => value.push('\n'),
                        b't' => value.push('\t'),
                        b'\\' => value.push('\\'),
                        b'"' => value.push('"'),
                        _ => return Err(Error::new("invalid string escape", Some(Span::new(i - 1, i + 1)))),
                    }
                    i += 1;
                } else {
                    value.push(bytes[i] as char);
                    i += 1;
                }
            }
            if i >= bytes.len() {
                return Err(Error::new("unterminated string literal", Some(Span::new(start, i))));
            }
            i += 1;
            tokens.push(Token { kind: TokenKind::String(value), span: Span::new(start, i) });
            continue;
        }

        let (kind, advance) = match b {
            b'(' => (TokenKind::LParen, 1),
            b')' => (TokenKind::RParen, 1),
            b'{' => (TokenKind::LBrace, 1),
            b'}' => (TokenKind::RBrace, 1),
            b'[' => (TokenKind::LBracket, 1),
            b']' => (TokenKind::RBracket, 1),
            b',' => (TokenKind::Comma, 1),
            b'.' => (TokenKind::Dot, 1),
            b':' => (TokenKind::Colon, 1),
            b';' => (TokenKind::Semicolon, 1),
            b'+' => (TokenKind::Plus, 1),
            b'*' => (TokenKind::Star, 1),
            b'/' => (TokenKind::Slash, 1),
            b'!' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    (TokenKind::BangEq, 2)
                } else {
                    (TokenKind::Bang, 1)
                }
            }
            b'=' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    (TokenKind::EqEq, 2)
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'>' {
                    (TokenKind::FatArrow, 2)
                } else {
                    (TokenKind::Eq, 1)
                }
            }
            b'<' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    (TokenKind::LtEq, 2)
                } else {
                    (TokenKind::Lt, 1)
                }
            }
            b'>' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    (TokenKind::GtEq, 2)
                } else {
                    (TokenKind::Gt, 1)
                }
            }
            b'-' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'>' {
                    (TokenKind::Arrow, 2)
                } else {
                    (TokenKind::Minus, 1)
                }
            }
            b'&' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'&' {
                    (TokenKind::AmpAmp, 2)
                } else {
                    return Err(Error::new("unexpected token '&'", Some(Span::new(start, start + 1))));
                }
            }
            b'|' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'|' {
                    (TokenKind::PipePipe, 2)
                } else {
                    return Err(Error::new("unexpected token '|'", Some(Span::new(start, start + 1))));
                }
            }
            _ => {
                return Err(Error::new("unexpected character", Some(Span::new(start, start + 1))));
            }
        };

        tokens.push(Token { kind, span: Span::new(start, start + advance) });
        i += advance;
    }

    Ok(tokens)
}

fn match_keyword(text: &str) -> Option<TokenKind> {
    let kw = match text {
        "namespace" => Keyword::Namespace,
        "use" => Keyword::Use,
        "struct" => Keyword::Struct,
        "enum" => Keyword::Enum,
        "interface" => Keyword::Interface,
        "actor" => Keyword::Actor,
        "on" => Keyword::On,
        "spawn" => Keyword::Spawn,
        "func" => Keyword::Func,
        "var" => Keyword::Var,
        "return" => Keyword::Return,
        "if" => Keyword::If,
        "else" => Keyword::Else,
        "for" => Keyword::For,
        "switch" => Keyword::Switch,
        "case" => Keyword::Case,
        "nil" => Keyword::Nil,
        "true" => Keyword::True,
        "false" => Keyword::False,
        _ => return None,
    };
    Some(TokenKind::Keyword(kw))
}

fn is_whitespace(b: u8) -> bool {
    b == b' ' || b == b'\t' || b == b'\n' || b == b'\r'
}

fn is_ident_start(b: u8) -> bool {
    (b'A'..=b'Z').contains(&b) || (b'a'..=b'z').contains(&b) || b == b'_' 
}

fn is_ident_part(b: u8) -> bool {
    is_ident_start(b) || is_digit(b)
}

fn is_digit(b: u8) -> bool {
    (b'0'..=b'9').contains(&b)
}

use crate::span::Span;

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub span: Option<Span>,
}

impl Error {
    pub fn new(message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

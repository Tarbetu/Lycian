use crate::ScopeId;
use scanner::Span;

use std::error::Error;
use std::fmt::Display;

pub type ScopeResult<T> = Result<T, ScopeError>;

#[derive(Debug)]
pub struct ScopeError {
    kind: ScopeErrorKind,
    message: &'static str,
    scope_id: ScopeId,
    span: Span,
}

#[derive(Debug)]
pub enum ScopeErrorKind {
    UnboundSymbol,
    DanglingCapture,
    InvalidShadowing,
    Cycle,
    AmbiguousBinding,
}

impl Display for ScopeErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScopeErrorKind::UnboundSymbol => write!(f, "Unbound symbol"),
            ScopeErrorKind::DanglingCapture => write!(f, "Dangling capture"),
            ScopeErrorKind::InvalidShadowing => write!(f, "Invalid shadowing"),
            ScopeErrorKind::Cycle => write!(f, "Cycle detected"),
            ScopeErrorKind::AmbiguousBinding => write!(f, "Ambiguous binding"),
        }
    }
}

impl Display for ScopeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}] {}: {}", self.span, self.kind, self.message)
    }
}

impl Error for ScopeError {}

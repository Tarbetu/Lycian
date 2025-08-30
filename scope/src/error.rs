use crate::ScopeId;
use scanner::Span;

use std::error::Error;
use std::fmt::Display;

pub type ScopeResult<T> = Result<T, ScopeError>;

#[derive(Debug)]
pub struct ScopeError {
    pub kind: ScopeErrorKind,
    pub message: &'static str,
    pub scope_id: ScopeId,
    pub span: Span,
}

#[derive(Debug)]
pub enum ScopeErrorKind {
    UnboundSymbol,
    DanglingCapture,
    InvalidShadowing,
    Cycle,
    AmbiguousBinding,
    DuplicateBinding,
    InvalidPatternValue,
}

impl Display for ScopeErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScopeErrorKind::UnboundSymbol => write!(f, "Unbound symbol"),
            ScopeErrorKind::DanglingCapture => write!(f, "Dangling capture"),
            ScopeErrorKind::InvalidShadowing => write!(f, "Invalid shadowing"),
            ScopeErrorKind::Cycle => write!(f, "Cycle detected"),
            ScopeErrorKind::AmbiguousBinding => write!(f, "Ambiguous binding"),
            ScopeErrorKind::DuplicateBinding => write!(f, "Duplicate binding"),
            ScopeErrorKind::InvalidPatternValue => write!(f, "Invalid Pattern Value"),
        }
    }
}

impl Display for ScopeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}] {}: {}", self.span, self.kind, self.message)
    }
}

impl Error for ScopeError {}

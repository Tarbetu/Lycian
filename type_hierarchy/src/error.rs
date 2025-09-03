use crate::TypeId;
use scanner::Span;
use std::error::Error;
use std::fmt;

pub type TypeResult<T> = Result<T, TypeError>;

#[derive(Debug)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub message: &'static str,
    pub type_id: TypeId,
    pub span: Span,
}

#[derive(Debug)]
pub enum TypeErrorKind {
    UnexpectedType,
    MissingType,
    InvalidType,
    TypeMismatch,
    NotANumber,
    NotABoolean,
    DivisionByZero,
}

impl fmt::Display for TypeErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TypeErrorKind;
        match self {
            TypeErrorKind::UnexpectedType => write!(f, "Unexpected type"),
            TypeErrorKind::MissingType => write!(f, "Missing type"),
            TypeErrorKind::InvalidType => write!(f, "Invalid type"),
            TypeErrorKind::TypeMismatch => write!(f, "Type mismatch"),
            TypeErrorKind::NotANumber => write!(f, "Not a number"),
            TypeErrorKind::NotABoolean => write!(f, "Not a boolean"),
            TypeErrorKind::DivisionByZero => write!(f, "Division by zero"),
        }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {}: {}", self.span, self.kind, self.message)
    }
}

impl Error for TypeError {}

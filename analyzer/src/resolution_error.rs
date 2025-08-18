mod type_error;
pub use type_error::*;
mod inheritance_error;
pub use inheritance_error::*;
mod scope_error;
pub use scope_error::*;

pub struct ResolutionError {
    pub source: &'static str,
    pub kind: ResolutionErrorKind,
}

pub enum ResolutionErrorKind {
    InheritanceError(Vec<InheritanceError>),
    ScopeError(Vec<ScopeError>),
    TypeError(Vec<TypeError>),
}

pub type ResolutionResult<T> = Result<T, ResolutionError>;

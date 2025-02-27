pub struct ResolutionError {
    pub source: &'static str,
    pub kind: ResolutionErrorKind,
    pub line: usize,
}

pub enum ResolutionErrorKind {}

pub type ResolutionResult<T> = Result<T, ResolutionError>;

pub struct ResolutionError {
    source: &'static str,
    kind: ResolutionErrorKind,
    line: usize,
}

pub enum ResolutionErrorKind {}

pub type ResolutionResult<T> = Result<T, ResolutionError>;

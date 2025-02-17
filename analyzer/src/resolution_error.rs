pub struct ResolutionError {
    source: &'static str,
    kind: ResolutionErrorKind,
    line: usize,
}

pub enum ResolutionErrorKind {
    // If a function don't call "yield" function
    // while the block is given to function
    UnexpectedBlockError,

    // If a function call "yield" function
    // while the block is not given to function
    BlockNotGiven,
}

pub type ResolutionResult<T> = Result<T, ResolutionError>;

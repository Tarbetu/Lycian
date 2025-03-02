use crate::ResolutionResult;

pub trait Analyzer {
    type Output;

    fn analyze(self) -> ResolutionResult<Self::Output>;
}

use parser::EntityIndex;

pub struct ResolutionError {
    pub source: &'static str,
    pub kind: ResolutionErrorKind,
}

pub enum ResolutionErrorKind {
    InheritanceError(Vec<InheritanceError>),
    TypeError(Vec<TypeError>),
}

pub struct InheritanceError {
    pub kind: InheritanceErrorKind,
    pub index: EntityIndex,
    pub line: usize,
}

pub enum InheritanceErrorKind {
    Cycle(Vec<EntityIndex>),
    UndefinedClass(EntityIndex),
}

// Are we available to provide the line number of any kind of type error?
// What kind of hell is this?
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub index: Option<EntityIndex>,
    pub line: Option<usize>,
}

pub enum TypeErrorKind {
    GuardPatternNotSupported(String),
}

pub type ResolutionResult<T> = Result<T, ResolutionError>;

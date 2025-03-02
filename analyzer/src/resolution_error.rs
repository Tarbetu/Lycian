use parser::EntityIndex;

pub struct ResolutionError {
    pub source: &'static str,
    pub kind: ResolutionErrorKind,
}

pub enum ResolutionErrorKind {
    InheritanceError(Vec<InheritanceError>),
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

pub type ResolutionResult<T> = Result<T, ResolutionError>;

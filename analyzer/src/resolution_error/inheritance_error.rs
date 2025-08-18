use parser::EntityIndex;

pub struct InheritanceError {
    pub kind: InheritanceErrorKind,
    pub index: Option<EntityIndex>,
    pub line: usize,
}

pub enum InheritanceErrorKind {
    Cycle(Vec<EntityIndex>),
    UndefinedClass(EntityIndex),
}

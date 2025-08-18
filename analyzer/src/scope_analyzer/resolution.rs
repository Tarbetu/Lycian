use parser::EntityIndex;

#[derive(Debug, Copy, Clone)]
pub struct OverloadIndex(usize);

/// Result of resolving a name to its declaration
#[derive(Debug, Clone)]
pub enum Resolution {
    /// The name was resolved to a single entity
    Entity(EntityIndex),

    /// The name was resolved to a specific function implementation
    Function {
        entity: EntityIndex,
        fn_index: OverloadIndex,
    },

    /// Multiple candidates found (needs further resolution based on types)
    OverloadCandidates(Vec<OverloadIndex>),
}

#[derive(Debug, Copy, Clone)]
pub struct OverloadCandidate {
    entity: EntityIndex,
    fn_index: Option<OverloadIndex>,
}

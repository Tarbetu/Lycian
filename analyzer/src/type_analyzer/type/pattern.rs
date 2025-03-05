use parser::{EntityIndex, LiteralIndex};

pub enum PatternPrecedence {
    Literal = 0,
    Guard = 10,
    Class = 20,
    Generic = 40,
}

pub enum Pattern {
    ClassConstrained {
        entity: EntityIndex,
        class_constraint: EntityIndex,
    },
    LiteralConstrained {
        entity: EntityIndex,
        literal_value: LiteralIndex,
    },
    GuardConstrained {
        entity: EntityIndex,
        base_type: Option<EntityIndex>,
        guard: parser::Expression,
    },
    Generic {
        entity: EntityIndex,
    },
}

impl Pattern {
    pub fn matches(&self, other: &Pattern) -> bool {
        unimplemented!()
    }
}

impl From<&Pattern> for PatternPrecedence {
    fn from(value: &Pattern) -> Self {
        match value {
            Pattern::LiteralConstrained { .. } => PatternPrecedence::Literal,
            Pattern::GuardConstrained { .. } => PatternPrecedence::Guard,
            Pattern::ClassConstrained { .. } => PatternPrecedence::Class,
            Pattern::Generic { .. } => PatternPrecedence::Generic,
        }
    }
}

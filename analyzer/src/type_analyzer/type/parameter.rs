use parser::{EntityIndex, LiteralIndex};

pub enum Parameter {
    TypeConstrained {
        entity: EntityIndex,
        type_constraint: EntityIndex,
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

impl Parameter {
    pub fn matches(&self, other: &Parameter) -> bool {
        unimplemented!()
    }
}

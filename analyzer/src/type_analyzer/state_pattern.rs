use parser::NameIndex;

use super::TypeIndex;

#[derive(Debug, Clone)]
pub struct ClassState {
    name: NameIndex,
    parameters: Vec<Parameter>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    name: NameIndex,
    value: TypeIndex,
}

// State pattern matching için
#[derive(Debug, Clone)]
pub enum StatePattern {
    Specific {
        state_name: String,
        parameters: Vec<Parameter>,
    },
    Any,
}

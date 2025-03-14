use super::{Pattern, Type};
use parser::EntityIndex;

#[derive(Debug, PartialEq)]
pub struct ClassState {
    pub class: Type,
    pub state_entity: EntityIndex,
    pub parameters: Vec<Pattern>,
}

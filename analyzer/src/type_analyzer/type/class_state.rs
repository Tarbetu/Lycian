use super::Pattern;
use parser::EntityIndex;

pub struct ClassState {
    pub class: EntityIndex,
    pub state_entity: EntityIndex,
    pub parameters: Vec<Pattern>,
}

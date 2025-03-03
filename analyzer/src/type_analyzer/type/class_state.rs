use super::Parameter;
use parser::EntityIndex;

pub struct ClassState {
    pub class_id: EntityIndex,
    pub state_entity: EntityIndex,
    pub parameters: Vec<Parameter>,
}

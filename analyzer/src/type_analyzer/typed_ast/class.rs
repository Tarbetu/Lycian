use super::TypedFunction;
use crate::type_analyzer::{ClassState, Type};
use ahash::HashMap;
use parser::EntityIndex;

pub struct TypedClass {
    pub class: parser::Class,
    pub itself: Type,
    pub states: Vec<ClassState>,
    pub methods: HashMap<EntityIndex, Vec<TypedFunction>>,
}

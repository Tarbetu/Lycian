use super::super::r#type::ClassState;
use super::TypedFunction;
use ahash::HashMap;
use parser::EntityIndex;

pub struct TypedClass {
    pub class: parser::Class,
    pub states: Vec<ClassState>,
    pub methods: HashMap<EntityIndex, Vec<TypedFunction>>,
}

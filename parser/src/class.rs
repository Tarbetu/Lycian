use crate::{EntityIndex, Function, Statement};
use ahash::AHashMap;

pub struct Class {
    pub name: EntityIndex,
    pub ancestors: Vec<EntityIndex>,
    pub methods: AHashMap<EntityIndex, Vec<Function>>,
    pub states: Vec<Statement>,
    pub decorator: String,
    pub line: usize,
}

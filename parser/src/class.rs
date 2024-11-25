use crate::{Function, NameIndex, Statement};
use ahash::AHashMap;

pub struct Class {
    pub name: NameIndex,
    pub ancestors: Vec<NameIndex>,
    pub methods: AHashMap<NameIndex, Vec<Function>>,
    pub states: Vec<Statement>,
    pub decorator: String,
}

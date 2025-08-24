use crate::{Function, Pattern};
use ahash::AHashMap;
use scanner::Span;
use std::rc::Rc;

#[derive(Default)]
pub struct Class {
    pub name: Rc<String>,
    pub ancestors: Vec<Rc<String>>,
    pub methods: AHashMap<Rc<String>, Vec<Function>>,
    pub constructors: Vec<(Rc<String>, Vec<Pattern>)>,
    pub decorator: Rc<String>,
    pub span: Span,
}

use crate::{BindingId, ScopeId, SyntaxNode};

pub struct Binding<'a> {
    pub id: BindingId,
    pub node: SyntaxNode<'a>,
    pub scope_id: ScopeId,
    pub kind: BindingKind,
    pub captured_from: Vec<ScopeId>,
}

impl<'a> Binding<'a> {
    pub fn new(id: BindingId, node: SyntaxNode<'a>, scope_id: ScopeId, kind: BindingKind) -> Self {
        Self {
            id,
            node,
            scope_id,
            kind,
            captured_from: vec![],
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum BindingKind {
    Class,
    Constructor,
    Method,
    LocalFunction,
    Argument,
}

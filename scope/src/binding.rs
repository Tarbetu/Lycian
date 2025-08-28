use crate::{ScopeId, SyntaxNode};

pub struct Binding<'a> {
    pub node: SyntaxNode<'a>,
    pub scope_id: ScopeId,
    pub kind: BindingKind,
    pub captured_from: Vec<ScopeId>,
}

impl<'a> Binding<'a> {
    pub fn new(node: SyntaxNode<'a>, scope_id: ScopeId, kind: BindingKind) -> Self {
        Self {
            node,
            scope_id,
            kind,
            captured_from: vec![],
        }
    }
}

pub enum BindingKind {
    Class,
    Constructor,
    Method,
    LocalFunction,
    Argument,
}

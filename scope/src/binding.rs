use crate::{ScopeId, SyntaxNode};

pub struct Binding<'a> {
    node: SyntaxNode<'a>,
    scope_id: ScopeId,
    kind: BindingKind,
    captured_from: Vec<ScopeId>,
}

pub enum BindingKind {
    Class,
    Constructor,
    Method,
    LocalFunction,
    Argument,
}

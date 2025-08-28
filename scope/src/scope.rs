use std::collections::HashMap;
use std::rc::Rc;
use syntax::PatternName;

use crate::{Binding, ScopeId, SyntaxNode};

pub struct Scope<'a> {
    pub id: ScopeId,
    pub parent_id: ScopeId,
    pub node: SyntaxNode<'a>,
    pub children_ids: Vec<ScopeId>,
    pub bindings: HashMap<PatternName, Binding<'a>>,
    pub resolved_references: HashMap<Rc<String>, (ScopeId, ResolvedReferenceStatus)>,
}

pub enum ResolvedReferenceStatus {
    Ok,
    CheckOverload,
    MaybeInherited,
}

impl<'a> Scope<'a> {
    pub fn root() -> Self {
        Scope {
            id: ScopeId(0),
            parent_id: ScopeId(0),
            node: SyntaxNode::Root,
            children_ids: Vec::new(),
            bindings: HashMap::new(),
            resolved_references: HashMap::new(),
        }
    }
}

impl Default for Scope<'_> {
    fn default() -> Self {
        Scope::root()
    }
}

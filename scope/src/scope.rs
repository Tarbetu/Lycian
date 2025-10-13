use std::collections::HashMap;
use std::rc::Rc;
use syntax::PatternName;

use crate::{hierarchy::Hierarchy, BindingId, ScopeId, SyntaxNode};

pub struct Scope<'a> {
    pub id: ScopeId,
    pub parent_id: ScopeId,
    pub node: SyntaxNode<'a>,
    pub children_ids: Vec<ScopeId>,
    pub bindings: HashMap<PatternName, BindingId>,
    pub resolved_references: HashMap<PatternName, (BindingId, ResolvedReferenceStatus)>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ResolvedReferenceStatus {
    Ok,
    CheckOverload,
    MaybeInherited,
    CheckMethodCall,
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

    pub fn find_binding_id(
        &'a self,
        hierarchy: &'a Hierarchy<'a>,
        call_name: &'a Rc<String>,
    ) -> Option<(BindingId, ScopeId)> {
        let name = PatternName::from(call_name);

        self.do_find_binding_id(hierarchy, name)
    }

    fn do_find_binding_id(
        &'a self,
        hierarchy: &'a Hierarchy<'a>,
        name: PatternName,
    ) -> Option<(BindingId, ScopeId)> {
        if let Some(binding) = self.bindings.get(&name) {
            Some((*binding, self.id))
        } else if self.parent_id != self.id {
            hierarchy
                .scopes
                .get(&self.parent_id)
                .unwrap()
                .do_find_binding_id(hierarchy, name)
        } else {
            None
        }
    }
}

impl Default for Scope<'_> {
    fn default() -> Self {
        Scope::root()
    }
}

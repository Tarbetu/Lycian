use super::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(PartialEq, Clone)]
pub enum TypeInfo {
    Exact(TypeId),
    NeedsInfer(Rc<RefCell<TypeBounds>>),
}

impl Default for TypeInfo {
    fn default() -> Self {
        TypeInfo::NeedsInfer(Rc::default())
    }
}

impl TypeInfo {
    pub fn as_exact(&self) -> Option<TypeId> {
        if let TypeInfo::Exact(type_id) = self {
            Some(*type_id)
        } else {
            None
        }
    }
}

#[derive(PartialEq, Default, Clone)]
pub struct TypeBounds {
    // Subtyping relation
    pub upper_bounds: HashSet<TypeId>,
    pub lower_bounds: HashSet<TypeId>,

    // Trait Constraints
    pub must_be_numeric: bool,
    pub must_be_addable: bool,
    pub must_be_integer: bool,
    pub must_be_floating: bool,

    // Capability Constraints
    pub must_be_callable: bool,
    pub must_accept_block: bool,

    // Relational Constraints (If it's deferred)
    pub can_be_cast_to: Vec<TypeId>,
    pub result_of: Vec<scope::BindingId>,
    pub responds_to: HashSet<syntax::PatternName>,
    pub super_call: Option<syntax::PatternName>,
}

impl TypeBounds {
    pub fn subtype_of(mut self, supertype_id: TypeId) -> Self {
        self.upper_bounds.insert(supertype_id);
        self
    }

    pub fn supertype_of(mut self, subtype_id: TypeId) -> Self {
        self.lower_bounds.insert(subtype_id);
        self
    }

    pub fn numeric(mut self) -> Self {
        self.must_be_numeric = true;
        self
    }

    pub fn addable(mut self) -> Self {
        self.must_be_addable = true;
        self
    }

    pub fn integer(mut self) -> Self {
        self.must_be_numeric = true;
        self.must_be_integer = true;
        self
    }

    pub fn floating(mut self) -> Self {
        self.must_be_numeric = true;
        self.must_be_floating = true;
        self
    }

    pub fn callable(mut self) -> Self {
        self.must_be_callable = true;
        self
    }

    pub fn accepts_block(mut self) -> Self {
        self.must_accept_block = true;
        self
    }

    pub fn results_of(mut self, binding_id: scope::BindingId) -> Self {
        self.result_of.push(binding_id);
        self
    }

    pub fn merge(mut self, other: Self) -> Self {
        self.upper_bounds.extend(other.upper_bounds);
        self.lower_bounds.extend(other.lower_bounds);
        self.must_be_numeric |= other.must_be_numeric;
        self.must_be_addable |= other.must_be_addable;
        self.must_be_integer |= other.must_be_integer;
        self.must_be_floating |= other.must_be_floating;
        self.must_be_callable |= other.must_be_callable;
        self.must_accept_block |= other.must_accept_block;
        self.result_of.extend(other.result_of);
        self.responds_to.extend(other.responds_to);
        self.super_call = self.super_call.or(other.super_call);
        self
    }
}

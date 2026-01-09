use syntax::Literal;

use super::*;
use std::cell::RefCell;
use std::collections::HashSet;
use std::mem::take;
use std::rc::Rc;

#[derive(PartialEq, Clone)]
pub enum TypeConstraint {
    Exact(TypeId),
    ExactLiteral(Literal),
    NeedsInfer(Rc<RefCell<TypeBounds>>),
}

impl Default for TypeConstraint {
    fn default() -> Self {
        TypeConstraint::NeedsInfer(Rc::default())
    }
}

impl From<TypeId> for TypeConstraint {
    fn from(type_id: TypeId) -> Self {
        TypeConstraint::Exact(type_id)
    }
}

impl TypeConstraint {
    pub fn needs_infer(type_bounds: TypeBounds) -> Self {
        TypeConstraint::NeedsInfer(Rc::new(RefCell::new(type_bounds)))
    }

    pub fn as_exact(&self) -> Option<TypeId> {
        if let TypeConstraint::Exact(type_id) = self {
            Some(*type_id)
        } else {
            None
        }
    }
}

#[derive(PartialEq, Default, Clone)]
pub struct TypeBounds {
    // -- Subtyping relation
    pub upper_bounds: HashSet<TypeId>,
    pub lower_bounds: HashSet<TypeId>,

    // -- It will passed as a argument to the type instance
    pub type_arguments: Vec<TypeConstraint>,

    // -- Trait Constraints
    pub must_be_numeric: bool,
    pub must_be_signed: bool,
    pub must_be_addable: bool,
    pub must_be_integer: bool,
    pub must_be_floating: bool,
    pub must_be_list: bool,
    pub must_be_array: bool,

    // -- Capability Constraints
    pub must_be_callable: bool,
    pub must_accept_block: bool,

    // -- Relational Constraints (If it's deferred)
    pub result_of: Vec<scope::BindingId>,
    pub callee: Option<TypeConstraint>,
    pub responds_to: HashSet<syntax::PatternName>,
    pub super_call: Option<syntax::PatternName>,
}

impl TypeBounds {
    pub fn add_upper_bound(mut self, supertype_id: TypeId) -> Self {
        self.upper_bounds.insert(supertype_id);
        self
    }

    pub fn add_lower_bound(mut self, subtype_id: TypeId) -> Self {
        self.lower_bounds.insert(subtype_id);
        self
    }

    pub fn with_argument(mut self, info: TypeConstraint) -> Self {
        self.type_arguments.push(info);
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

    pub fn list(mut self) -> Self {
        self.must_be_list = true;
        self
    }

    pub fn array(mut self) -> Self {
        self.must_be_array = true;
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

    pub fn merge(&mut self, other: Rc<RefCell<Self>>) {
        let mut other = other.borrow_mut();
        self.upper_bounds.extend(take(&mut other.upper_bounds));
        self.lower_bounds.extend(take(&mut other.lower_bounds));
        self.must_be_numeric |= other.must_be_numeric;
        self.must_be_addable |= other.must_be_addable;
        self.must_be_integer |= other.must_be_integer;
        self.must_be_floating |= other.must_be_floating;
        self.must_be_callable |= other.must_be_callable;
        self.must_accept_block |= other.must_accept_block;
        self.result_of.extend(take(&mut other.result_of));

        if self.callee.is_none() {
            self.callee = take(&mut other.callee);
        }

        self.responds_to.extend(take(&mut other.responds_to));

        if self.super_call.is_none() {
            self.super_call = take(&mut other.super_call);
        }
    }

    pub fn can_be_casted_to_container(&self) -> bool {
        self.upper_bounds.is_empty()
            && self.lower_bounds.is_empty()
            && !self.must_be_numeric
            && !self.must_be_addable
            && !self.must_be_integer
            && !self.must_be_floating
            && !self.must_be_callable
    }

    pub fn can_be_casted_to_boolean(&self) -> bool {
        self.upper_bounds.is_empty()
            && self.lower_bounds.is_empty()
            && !self.must_be_list
            && !self.must_be_array
            && !self.must_be_numeric
            && !self.must_be_addable
            && !self.must_be_integer
            && !self.must_be_floating
            && !self.must_be_callable
    }

    pub fn can_be_casted_to_integer(&self) -> bool {
        self.upper_bounds.is_empty()
            && self.lower_bounds.is_empty()
            && !self.must_be_list
            && !self.must_be_array
            && !self.must_be_callable
            && !self.must_be_floating
    }

    // All floats can be casted as a number
    pub fn can_be_casted_to_number(&self) -> bool {
        self.upper_bounds.is_empty()
            && self.lower_bounds.is_empty()
            && !self.must_be_list
            && !self.must_be_array
            && !self.must_be_callable
    }

    pub fn can_be_casted_to_char(&self) -> bool {
        self.upper_bounds.is_empty()
            && self.lower_bounds.is_empty()
            && !self.must_be_list
            && !self.must_be_array
            && !self.must_be_numeric
            && !self.must_be_addable
            && !self.must_be_integer
            && !self.must_be_floating
            && !self.must_be_callable
    }

    pub fn can_be_casted_to_string(&self) -> bool {
        self.upper_bounds.is_empty()
            && self.lower_bounds.is_empty()
            && !self.must_be_list
            && !self.must_be_array
            && !self.must_be_numeric
            && !self.must_be_addable
            && !self.must_be_integer
            && !self.must_be_floating
            && !self.must_be_callable
    }
}

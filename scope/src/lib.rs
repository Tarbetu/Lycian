mod binding;
mod error;
mod hierarchy;
mod scope;

use synonym::Synonym;

use std::rc::Rc;

pub use binding::Binding;
pub use error::ScopeError;
pub use hierarchy::Hierarchy;
pub use scope::Scope;

#[derive(Synonym)]
pub struct ExprId(usize);

#[derive(Synonym)]
pub struct ScopeId(usize);

#[derive(Synonym)]
pub struct BindingId(usize);

pub enum SyntaxNode<'a> {
    Class(&'a syntax::Class),
    Function(&'a syntax::Function),
    Expression(&'a syntax::Expression),
    Constructor(&'a (Rc<String>, Vec<syntax::Pattern>)),
    Root,
}

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}

mod binding;
mod error;
mod hierarchy;
mod name_resolver;
mod scope;

use synonym::Synonym;

use std::rc::Rc;

pub use binding::{Binding, BindingKind};
pub use error::ScopeResult;
pub use hierarchy::Hierarchy;
pub use scope::Scope;

#[derive(Synonym)]
pub struct ExprId(pub usize);

#[derive(Synonym)]
pub struct ScopeId(usize);

#[derive(Synonym)]
pub struct BindingId(usize);

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum SyntaxNode<'a> {
    Root,
    Class(&'a syntax::Class),
    Function(&'a syntax::Function),
    // Because methods are overloadable
    // Only used for bindings
    Method(&'a [syntax::Function]),
    Expression(&'a syntax::Expression),
    Constructor(&'a syntax::Class, &'a (Rc<String>, Vec<syntax::Pattern>)),
    Pattern(&'a syntax::Pattern),
}

impl<'a> SyntaxNode<'a> {
    pub fn span(&self) -> scanner::Span {
        match self {
            SyntaxNode::Root => unreachable!(),
            SyntaxNode::Class(class) => class.span.clone(),
            SyntaxNode::Function(function) => function.span.clone(),
            SyntaxNode::Method(functions) => functions[0].span.clone(),
            SyntaxNode::Expression(expr) => expr.span.clone(),
            SyntaxNode::Constructor(_, (_, patterns)) => patterns[0].value.span.clone(),
            SyntaxNode::Pattern(pattern) => pattern.value.span.clone(),
        }
    }

    pub fn return_type_expr(&self) -> Option<&syntax::Expression> {
        match self {
            SyntaxNode::Function(function) => function.return_type.as_ref(),
            _ => panic!("Not a function node"),
        }
    }
}

pub fn build_scopes<'a>(classes: &'a [syntax::Class]) -> ScopeResult<Hierarchy<'a>> {
    name_resolver::resolve(Hierarchy::default().build(classes)?)
}

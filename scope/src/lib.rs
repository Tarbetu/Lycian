mod binding;
mod error;
mod hierarchy;
mod scope;

use synonym::Synonym;

use std::rc::Rc;

pub use binding::Binding;
pub use error::ScopeResult;
pub use hierarchy::Hierarchy;
pub use scope::Scope;

#[derive(Synonym)]
pub struct ExprId(usize);

#[derive(Synonym)]
pub struct ScopeId(usize);

#[derive(Synonym)]
pub struct BindingId(usize);

#[derive(Copy, Clone)]
pub enum SyntaxNode<'a> {
    Root,
    Class(&'a syntax::Class),
    Function(&'a syntax::Function),
    // Because methods are overloadable
    // Only used for bindings
    Method(&'a [syntax::Function]),
    Expression(&'a syntax::Expression),
    Constructor(&'a (Rc<String>, Vec<syntax::Pattern>)),
    Pattern(&'a syntax::Pattern),
}

impl<'a> SyntaxNode<'a> {
    pub fn name(&'a self) -> &'a Rc<String> {
        use SyntaxNode::*;
        match self {
            Class(class) => &class.name,
            Function(function) => &function.name,
            Method(functions) => &functions[0].name,
            Expression(syntax::Expression { kind: kind, .. }) => match kind.as_ref() {
                syntax::ExpressionKind::Function(syntax::Function { name, .. }) => name,
                syntax::ExpressionKind::Block { .. } => unimplemented!(),
                _ => unreachable!(),
            },
            Constructor((name, _)) => name,
            Root => unimplemented!(),
        }
    }

    pub fn span(&self) -> scanner::Span {
        match self {
            SyntaxNode::Class(class) => class.span.clone(),
            SyntaxNode::Function(function) => function.span.clone(),
            SyntaxNode::Method(functions) => functions[0].span.clone(),
            SyntaxNode::Expression(expr) => expr.span.clone(),
            SyntaxNode::Constructor((_, patterns)) => patterns[0].value.span.clone(),
            SyntaxNode::Root => unreachable!(),
        }
    }
}

pub fn build_scopes<'a>(classes: &'a [syntax::Class]) -> ScopeResult<Hierarchy<'a>> {
    Hierarchy::default().build(classes)
}

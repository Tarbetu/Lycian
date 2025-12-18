mod definition;
mod error;
mod hierarchy;
mod type_checker;

pub use definition::TypeId;

use error::{TypeError, TypeErrorKind, TypeResult};
use hierarchy::Hierarchy;
use type_checker::TypeChecker;

pub fn build_type_hierarchy<'a>(
    scope_hierarchy: scope::Hierarchy<'a>,
) -> TypeResult<Hierarchy<'a>> {
    let mut checker = TypeChecker::new(Hierarchy::new(scope_hierarchy)?);
    checker.traverse()?;
    let TypeChecker { hierarchy, errors } = checker;

    if errors.is_empty() {
        Ok(hierarchy)
    } else {
        Err(TypeError {
            kind: TypeErrorKind::Multiple(errors),
            message: "Multiple errors from the Type Checker",
            type_id: TypeId(0),
            span: hierarchy
                .scope_hierarchy
                .scopes
                .values()
                .next()
                .unwrap()
                .node
                .span(),
        })
    }
}

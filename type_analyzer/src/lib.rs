mod definition;
mod error;
mod hierarchy;
mod type_checker;

pub use definition::TypeId;

use error::TypeResult;
use hierarchy::Hierarchy;

pub fn build_type_hierarchy<'a>(
    scope_hierarchy: scope::Hierarchy<'a>,
) -> TypeResult<Hierarchy<'a>> {
    Hierarchy::new(scope_hierarchy)
}

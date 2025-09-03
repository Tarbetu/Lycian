mod definition;
mod error;
mod hierarchy;

pub use definition::TypeId;

use error::TypeResult;
use hierarchy::Hierarchy;

pub fn build_type_hierarchy<'a>(
    scope_hierarchy: scope::Hierarchy<'a>,
) -> TypeResult<Hierarchy<'a>> {
    Ok(Hierarchy::new(scope_hierarchy))
}

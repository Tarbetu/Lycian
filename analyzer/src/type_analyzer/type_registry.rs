use ahash::AHashMap;

use super::Type;
use super::TypeIndex;
use parser::NameIndex;

pub struct TypeRegistry {
    types: AHashMap<TypeIndex, Type>,
    type_names: AHashMap<NameIndex, TypeIndex>,
    type_application_cache: AHashMap<TypeIndex, Vec<(NameIndex, NameIndex)>>,
}

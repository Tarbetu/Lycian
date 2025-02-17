use super::{symbol::Symbol, Scope, SymbolId};
use ahash::HashMap;

pub type SymbolTable = HashMap<SymbolId, Symbol>;

#[derive(Default)]
pub struct ScopeHierarchy {
    pub symbol_table: SymbolTable,
    pub classes: HashMap<SymbolId, Scope>,
}

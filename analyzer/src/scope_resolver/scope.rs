use ahash::{HashMap, HashMapExt};

use super::symbol::SymbolId;

pub struct Scope {
    // We usually name all scopes,
    // however blocks are not named
    // We can assume their name as "yield"
    // but it's better to be explicit in code
    // so we use Option for blocks
    // Also, global scope has no name
    pub itself: SymbolId,
    pub locals: Vec<SymbolId>,
    pub scopes: HashMap<SymbolId, Scope>,
}

impl Scope {
    pub fn new(itself: SymbolId) -> Scope {
        Scope {
            itself,
            locals: Vec::new(),
            scopes: HashMap::new(),
        }
    }
}

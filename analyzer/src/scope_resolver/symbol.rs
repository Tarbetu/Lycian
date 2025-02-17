#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct SymbolId(pub usize);

impl From<usize> for SymbolId {
    fn from(id: usize) -> Self {
        SymbolId(id)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Arity(pub usize);

impl From<usize> for Arity {
    fn from(id: usize) -> Self {
        Arity(id)
    }
}

pub enum SymbolVisibility {
    Public,
    Protected,
    Private,
}

pub enum SymbolType {
    Class,
    Method,
    Function,
    Local,
}

pub struct Symbol {
    pub name: String,
    pub visibility: SymbolVisibility,
    pub kind: SymbolType,
}

impl Symbol {
    pub fn from_name(name: &parser::Name, kind: SymbolType) -> Self {
        use parser::Name::*;
        match name {
            Public(name) => Symbol {
                name: name.to_string(),
                visibility: SymbolVisibility::Public,
                kind,
            },
            Protected(name) => Symbol {
                name: name.to_string(),
                visibility: SymbolVisibility::Protected,
                kind,
            },
            Private(name) => Symbol {
                name: name.to_string(),
                visibility: SymbolVisibility::Private,
                kind,
            },
        }
    }
}

// Local is a symbol that is local to a function
// Well, it does not sound meaningful
// You could just say it's a parameter
// But we could expand the concept of local in the future
pub struct Local(pub parser::NameIndex);

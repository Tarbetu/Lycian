use ahash::AHashMap;
use scanner::TokenType;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Visibility {
    /// Public names are defined with constants.
    /// i.e: MyType
    Public,

    /// Protected names are defined with simple identifiers,
    /// i.e: my_type
    Protected,

    /// Private names are defined with leading wildcard.
    /// i.e: _my_type
    Private,
}

impl From<TokenType> for Visibility {
    fn from(token: TokenType) -> Self {
        match token {
            TokenType::Constant => Self::Public,
            TokenType::Identifier => Self::Protected,
            TokenType::Wildcard => Self::Private,
            _ => panic!("Invalid token type for visibility"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct EntityIndex(pub usize);

impl From<usize> for EntityIndex {
    fn from(number: usize) -> EntityIndex {
        Self(number)
    }
}

/// The entities are stored in an ordered way.
/// The order:
/// Global -> Class -> Declaration -> (Function | Argument | Call)
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum EntityKind {
    /// Global entities are the first entities in the entity table.
    Global,

    /// Class entities are entities that represent a class.
    Class,

    /// Declaration entities are entities that represent a class state or method.
    Declaration,

    /// Lycian does not have concept of variables
    /// Everything inside a method is a function or an argument.
    /// We represent them as locals.
    Local,

    /// We leave the resolution of calls to the scope resolution phase.
    Call,
}

#[derive(Debug)]
pub struct Entity {
    pub name: String,
    pub visibility: Visibility,
    pub kind: EntityKind,
    pub sub_entities: Vec<EntityIndex>,
}

impl Default for Entity {
    fn default() -> Self {
        Self {
            name: "Program".to_string(),
            visibility: Visibility::Public,
            kind: EntityKind::Global,
            sub_entities: vec![],
        }
    }
}

pub type EntityTable = AHashMap<EntityIndex, Entity>;

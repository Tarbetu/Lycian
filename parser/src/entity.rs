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

impl EntityIndex {
    pub fn is_same(self, other: EntityIndex, table: EntityTable) -> bool {
        table[&self].name == table[&other].name
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

#[derive(Debug, PartialEq)]
pub struct Entity {
    pub index: EntityIndex,
    pub name: String,
    pub visibility: Visibility,
    pub kind: EntityKind,
    pub sub_entities: Vec<EntityIndex>,
}

impl Entity {
    fn get_sub_entities<'a>(&'a self, table: &'a EntityTable) -> Vec<&'a Entity> {
        self.sub_entities
            .iter()
            .map(|index| &table[index])
            .collect()
    }

    pub fn find_sub_entity_by_name<'a>(
        &'a self,
        table: &'a EntityTable,
        name: &str,
    ) -> Option<&'a Entity> {
        self.get_sub_entities(table)
            .into_iter()
            .find(|entity| entity.name == name)
    }
}

impl Default for Entity {
    fn default() -> Self {
        Self {
            index: EntityIndex(0),
            name: "Program".to_string(),
            visibility: Visibility::Public,
            kind: EntityKind::Global,
            sub_entities: vec![],
        }
    }
}

pub type EntityTable = AHashMap<EntityIndex, Entity>;

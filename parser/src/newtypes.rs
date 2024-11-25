use std::fmt::Display;
use std::hash::{BuildHasher, Hasher, RandomState};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct NameIndex(pub usize);

impl NameIndex {
    pub fn new(seed: &str) -> Self {
        let mut hasher = RandomState::new().build_hasher();
        hasher.write(seed.as_bytes());

        Self(hasher.finish() as usize)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct LiteralIndex(pub usize);

#[derive(Debug, PartialEq)]
pub enum Name {
    /// Public names are defined with constants.
    /// i.e: MyType
    Public(String),

    /// Protected names are defined with simple identifiers,
    /// i.e: my_type
    Protected(String),

    /// Private names are defined with leading wildcard.
    /// i.e: _my_type
    Private(String),
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Name::Public(name) | Name::Protected(name) | Name::Private(name) => {
                write!(f, "{}", name)
            }
        }
    }
}

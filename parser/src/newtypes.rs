use std::fmt::Display;
use std::hash::{BuildHasher, Hasher, RandomState};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct NameIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct LiteralIndex(pub usize);

#[derive(Debug, PartialEq, Eq, Hash)]
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

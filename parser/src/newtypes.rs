#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ConstantIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IdentifierIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct WildcardIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FunctionIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LiteralIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Name {
    /// Public names are defined with constants.
    /// i.e: MyType
    Public(ConstantIndex),

    /// Protected names are defined with simple identifiers,
    /// i.e: my_type
    Protected(IdentifierIndex),

    /// Private names are defined with leading wildcard.
    /// i.e: _my_type
    Private(WildcardIndex),
}

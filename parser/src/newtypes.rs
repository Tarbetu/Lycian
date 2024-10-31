#[derive(Debug, Clone, Copy)]
pub struct ConstantIndex(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct IdentifierNameIndex(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct WildcardNameIndex(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct FunctionIndex(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct LiteralIndex(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct IntegerIndex(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct FloatIndex(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct StringIndex(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct ListIndex(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct NamedBlockIndex(pub usize);

#[derive(Debug, Clone, Copy)]
pub enum Name {
    /// Public names are defined with constants.
    /// i.e: MyType
    Public(ConstantIndex),

    /// Protected names are defined with simple identifiers,
    /// i.e: my_type
    Protected(IdentifierNameIndex),

    /// Private names are defined with leading wildcard.
    /// i.e: _my_type
    Private(WildcardNameIndex),
}

use parser::EntityIndex;

#[derive(Debug, PartialEq)]
pub enum PrimitiveType {
    Bool,
    Char,
    Str,
    Float32,
    Float64,
    Int8,
    Int16,
    Int32,
    Int64,
    Int128,
    Usize,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Uint128,
    BigInteger, // We haven't implemented this yet, so this is a sign of error
    BigFloat,   // Same with BigInteger
    List(EntityIndex),
    Map(EntityIndex, EntityIndex),
    // Static list is a compile time array
    // And this can not distingushed from a normal array
    // for the programmer
    StaticList(EntityIndex, usize),
}

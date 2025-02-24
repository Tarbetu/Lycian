use parser::{EntityIndex, LiteralIndex};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeIndex(pub usize);

impl From<usize> for TypeIndex {
    fn from(index: usize) -> TypeIndex {
        TypeIndex(index)
    }
}

pub enum Type {
    Class {
        entity: EntityIndex,
        ancestors: Vec<TypeIndex>,
        states: Vec<ClassState>,
    },
    Function {
        parameters: Vec<TypeIndex>,
        return_type: TypeIndex,
    },
    FunctionApplication {
        function: TypeIndex,
        arguments: Vec<TypeIndex>,
    },
    Literal(LiteralIndex),
    Primitive(PrimitiveType),
    Special(SpecialType),
}

pub struct ClassState {
    class_id: TypeIndex,
    state_entity: EntityIndex,
    arguments: Vec<TypeIndex>,
}

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
    List(TypeIndex),
    Map(TypeIndex, TypeIndex),
    // Static list is a compile time array
    // And this can not distingushed from a normal array
    // for the programmer
    StaticList(TypeIndex, usize),
}

/// This category is representing the state changes of the program or the special cases
/// None of them will be evaluated in compile time, memoized, executed in a parallel and lazy evaluated
pub enum SpecialType {
    /// Represents mutation and IO operations
    /// For example, reading a file, writing a file or changing a variable
    Mutation,

    /// For FFI, Lycian does not have a void type btw
    /// This is works same with Mutation, but it express that the function came from a foreign language
    Void,

    /// Represent the end of the program, like panic
    /// When a function returns Eschaton, the program will terminated after function finished
    Eschaton,

    /// Represent the absurd, like an infinite loop
    /// When a function returns a Absurd, the program will terminated before function started
    Absurd,
}

/// This category is representing the state changes of the program or the special cases
/// None of them will be evaluated in compile time, memoized, executed in a parallel and lazy evaluated
#[derive(Debug, PartialEq)]
pub enum SpecialType {
    /// Represents mutation and IO operations
    /// For example, reading a file, writing a file or changing a variable
    Mutation,

    /// For FFI, Lycian does not have a void type btw
    /// This is works same with Mutation, but it express that the function came from a foreign language
    Void,

    /// Represent the end of the program, like panic
    /// When a function returns Eschaton, the program will be terminated after function finished
    Eschaton,

    /// Represent the absurd, like an infinite loop
    /// This is used for the functions that never returns
    Absurd,
}

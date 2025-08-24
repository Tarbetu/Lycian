#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Operator {
    Add,
    Substract,
    Multiply,
    Divide,
    Modulo,
    And,
    Or,
    In,
    Equal,
    NotEqual,
    Smaller,
    SmallerOrEqual,
    Greater,
    GreaterOrEqual,
    Negate,
    Not,
}

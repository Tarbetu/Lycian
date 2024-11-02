#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Substract,
    Multiply,
    Divide,
    Modulo,
    Declaration,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnaryOperator {
    Negate,
    Not,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LogicalOperator {
    And,
    Or,
    In,
    Equal,
    NotEqual,
    Smaller,
    SmallerOrEqual,
    Greater,
    GreaterOrEqual,
}

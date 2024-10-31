#[derive(Copy, Clone)]
pub enum BinaryOperator {
    Add,
    Substract,
    Multiply,
    Divide,
    Modulo,
    Declaration,
}

pub enum UnaryOperator {
    Negate,
    Not,
}

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

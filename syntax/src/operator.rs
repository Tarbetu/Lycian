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

impl Operator {
    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            Operator::Add
                | Operator::Substract
                | Operator::Multiply
                | Operator::Divide
                | Operator::Modulo
                | Operator::Negate
        )
    }

    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
            Operator::Equal
                | Operator::NotEqual
                | Operator::Smaller
                | Operator::SmallerOrEqual
                | Operator::Greater
                | Operator::GreaterOrEqual
        )
    }

    pub fn is_logical(&self) -> bool {
        matches!(self, Operator::And | Operator::Or | Operator::Not)
    }
}

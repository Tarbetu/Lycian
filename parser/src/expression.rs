use crate::literal::*;
use crate::operator::*;
use crate::{FunctionIndex, NameIndex};

#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Grouping(Box<Expression>),
    Type(NameIndex),

    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Logical(Box<Expression>, LogicalOperator, Box<Expression>),

    Function(FunctionIndex),

    Match {
        matchable: Box<Expression>,
        patterns: Vec<Expression>,
    },

    Call {
        callee: Box<Expression>,
        function_id: FunctionIndex,
        args: Vec<Expression>,
    },

    Get {
        callee: Box<Expression>,
        function_id: FunctionIndex,
    },

    IndexOperator(Box<Expression>, Box<Expression>),

    ClassSelf,
    Super(FunctionIndex),
    Block {
        expressions: Vec<Expression>,
        value: Box<Expression>,
    },
    NamedBlock {
        block_name: NameIndex,
        block: Box<Expression>,
    },
    Pattern {
        name: Option<NameIndex>,
        value: Box<Expression>,
    },
}

impl Expression {
    pub fn is_constexpr(&self) -> bool {
        use Expression::*;

        match self {
            Literal(_) => true,
            Grouping(expr) => expr.is_constexpr(),
            NamedBlock { block, .. } => block.is_constexpr(),
            Block { value, .. } => value.is_constexpr(),
            Binary(lhs, _, rhs) => lhs.is_constexpr() && rhs.is_constexpr(),
            Logical(lhs, _, rhs) => lhs.is_constexpr() && rhs.is_constexpr(),
            Unary(_, expr) => expr.is_constexpr(),
            IndexOperator(expr, index) => expr.is_constexpr() && index.is_constexpr(),
            _ => false,
        }
    }
}

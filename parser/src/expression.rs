use crate::operator::*;
use crate::LiteralIndex;
use crate::Pattern;
use crate::{FunctionIndex, NameIndex};

#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal(LiteralIndex),
    Grouping(Box<Expression>),
    Type(NameIndex),

    Binary(Box<Expression>, Operator, Box<Expression>),
    Unary(Operator, Box<Expression>),

    Function(FunctionIndex),

    Match {
        scrutinee: Box<Expression>,
        arms: Vec<(Pattern, Expression)>,
    },

    Call {
        callee: Box<Expression>,
        function_id: FunctionIndex,
        args: Vec<Pattern>,
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
}

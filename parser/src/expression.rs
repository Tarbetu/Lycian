use crate::operator::*;
use crate::LiteralIndex;
use crate::NameIndex;
use crate::Pattern;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal(LiteralIndex),
    Grouping(Box<Expression>),
    Type(NameIndex),

    Binary(Box<Expression>, Operator, Box<Expression>),
    Unary(Operator, Box<Expression>),

    Function(NameIndex),

    Match {
        scrutinee: Box<Expression>,
        arms: Vec<(Pattern, Expression)>,
    },

    Call {
        callee: Box<Expression>,
        function_id: NameIndex,
        args: Vec<Expression>,
    },

    MethodCall {
        inner_call: Box<Expression>,
    },

    IndexOperator(Box<Expression>, Box<Expression>),

    ClassSelf,
    Super(NameIndex),
    Block {
        expressions: Vec<Expression>,
        value: Box<Expression>,
    },
    NamedBlock {
        block_name: NameIndex,
        block: Box<Expression>,
    },
}

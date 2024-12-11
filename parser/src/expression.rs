use crate::operator::*;
use crate::LiteralIndex;
use crate::NameIndex;
use crate::Pattern;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(LiteralIndex),
    Grouping(Box<Expression>),

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
        block: Option<Box<Expression>>,
    },

    CallRoot(NameIndex),

    IndexOperator(Box<Expression>, Box<Expression>),

    ClassSelf,
    Super,
    Block {
        expressions: Vec<Expression>,
        value: Box<Expression>,
        params: Vec<Pattern>,
    },

    Lambda {
        expression: Box<Expression>,
        params: Vec<Pattern>,
    },
}

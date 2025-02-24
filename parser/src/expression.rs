use crate::operator::*;
use crate::EntityIndex;
use crate::LiteralIndex;
use crate::Pattern;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(LiteralIndex),
    Grouping(Box<Expression>),

    Binary(Box<Expression>, Operator, Box<Expression>),
    Unary(Operator, Box<Expression>),

    Function(EntityIndex),

    Match {
        scrutinee: Box<Expression>,
        arms: Vec<(Pattern, Expression)>,
    },

    Call {
        name_id: EntityIndex,
        caller: Option<Box<Expression>>,
        args: Vec<Pattern>,
        block: Option<Box<Expression>>,
    },

    IndexOperator(Box<Expression>, Box<Expression>),

    ClassSelf,
    Super,
    Block {
        expressions: Vec<Expression>,
        value: Box<Expression>,
        params: Vec<Pattern>,
    },
}

use crate::operator::*;
use crate::Function;
use crate::Literal;
use crate::Pattern;
use scanner::Span;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub id: usize,
    pub kind: Box<ExpressionKind>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Literal(Rc<Literal>),
    Grouping(Expression),

    Binary(Expression, Operator, Expression),
    Unary(Operator, Expression),

    Function(Function),

    Match {
        scrutinee: Expression,
        arms: Vec<(Pattern, Expression)>,
    },

    Call {
        caller: Rc<String>,
        callee: Option<Expression>,
        args: Vec<Expression>,
        block: Option<Expression>,
        call_type: CallType,
    },

    IndexOperator(Expression, Expression),

    ClassSelf,
    Super,
    Pass,

    Block {
        expressions: Vec<Expression>,
        value: Expression,
        params: Vec<Pattern>,
    },
}

#[derive(Copy, Clone, Debug, PartialEq, Default)]
pub enum CallType {
    #[default]
    LazyMemoized,
    Strict,
}

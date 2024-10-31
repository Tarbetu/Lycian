use crate::literal::*;
use crate::operator::*;
use crate::{ConstantIndex, FunctionIndex, NamedBlockIndex};
use scanner::Token;

pub enum Expression {
    Literal(Literal),
    Grouping(Box<Expression>),
    Constant(ConstantIndex),

    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Logical(Box<Expression>, LogicalOperator, Box<Expression>),

    Call(Box<Expression>, FunctionIndex, Vec<Expression>),
    MethodCall(Box<Expression>, FunctionIndex),

    ClassSelf(Token),
    Super(Token, FunctionIndex),
    Block(Vec<Expression>),
    NamedBlock(NamedBlockIndex, Vec<Expression>),
    Pattern(Option<Box<Expression>>, Box<Literal>),
}

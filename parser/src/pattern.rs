use crate::Expression;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub name: PatternName,
    pub value: Expression,
    pub condition: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternName {
    Name(Rc<String>),
    ClassSelf,
    Super,
    NoName,
}

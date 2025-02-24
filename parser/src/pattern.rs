use crate::{EntityIndex, Expression};

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub name: PatternName,
    pub value: Option<Expression>,
    pub condition: Option<Expression>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PatternName {
    Name(EntityIndex),
    ClassSelf,
    NoName,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PatternType {
    Argument,
    Parameter,
}

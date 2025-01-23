use crate::{Expression, NameIndex};

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub name: Option<NameIndex>,
    pub value: Option<Expression>,
    pub condition: Option<Expression>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PatternType {
    Argument,
    Parameter,
}

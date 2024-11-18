use crate::{Expression, NameIndex};
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub name: Option<NameIndex>,
    pub value: Option<Expression>,
    pub condition: Option<Expression>,
}

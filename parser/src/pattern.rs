use crate::{Expression, NameIndex};
#[derive(Debug, PartialEq)]
pub struct Pattern {
    pub name: Option<NameIndex>,
    pub value: Expression,
    pub condition: Option<Expression>,
}

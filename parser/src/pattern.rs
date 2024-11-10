use crate::{Expression, NameIndex};
#[derive(Debug, PartialEq)]
pub struct Pattern {
    pub name: Option<NameIndex>,
    pub value: Option<Expression>,
    pub condition: Option<Expression>,
}

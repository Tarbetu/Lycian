use crate::EntityIndex;
use crate::Pattern;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Implementing(EntityIndex),
    ClassState {
        name: EntityIndex,
        patterns: Vec<Pattern>,
    },
    Method(EntityIndex),
}

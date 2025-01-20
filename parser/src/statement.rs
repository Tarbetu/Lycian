use crate::NameIndex;
use crate::Pattern;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Implementing(NameIndex),
    ClassState {
        name: NameIndex,
        patterns: Vec<Pattern>,
    },
    Method(NameIndex),
}

use crate::NameIndex;
use crate::Pattern;

pub enum Statement {
    Implementing(NameIndex),
    ClassState {
        name: NameIndex,
        patterns: Vec<Pattern>,
    },
    Method(NameIndex),
}

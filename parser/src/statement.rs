use crate::FunctionIndex;
use crate::NameIndex;
use crate::Pattern;

pub enum Statement {
    Class {
        name: NameIndex,
        implementing_list: Vec<NameIndex>,
        states: Vec<Statement>,
        decorator: String,
        methods: Vec<FunctionIndex>,
    },
    Implementing(NameIndex),
    ClassState {
        name: NameIndex,
        patterns: Vec<Pattern>,
    },
    Method(FunctionIndex),
}

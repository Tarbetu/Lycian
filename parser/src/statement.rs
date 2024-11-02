use crate::Expression;
use crate::FunctionIndex;
use crate::Name;

pub enum Statement {
    Class {
        name: Name,
        implementing_list: Vec<Statement>,
        states: Vec<Statement>,
        decorator: String,
    },
    Implementing(Name),
    ClassState {
        name: Name,
        pattern: Vec<Expression>,
    },
    Method {
        class: Box<Statement>,
        function: FunctionIndex,
    },
}

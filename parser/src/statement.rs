use crate::Expression;
use crate::Name;

pub enum Statement {
    Class {
        name: Name,
        implementing_list: Vec<Statement>,
        states: Vec<Statement>,
        decorator: String,
    },
    Implementing(Name),
    State {
        name: Name,
        pattern: Vec<Expression>,
    },
    Function {
        class: Box<Statement>,
        name: Name,
        body: Vec<Expression>,
        decorator: String,
    },
}

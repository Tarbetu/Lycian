use crate::Expression;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Pattern {
    pub name: PatternName,
    pub value: Expression,
    pub condition: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PatternName {
    Name(Rc<String>),
    ClassSelf,
    Super,
    NoName,
}

impl From<&Rc<String>> for PatternName {
    fn from(name: &Rc<String>) -> Self {
        match name.as_str() {
            "self" => PatternName::ClassSelf,
            "super" => PatternName::Super,
            _ => PatternName::Name(name.clone()),
        }
    }
}

impl AsRef<str> for PatternName {
    fn as_ref(&self) -> &str {
        match self {
            PatternName::Name(name) => name.as_ref(),
            PatternName::ClassSelf => "self",
            PatternName::Super => "super",
            PatternName::NoName => "",
        }
    }
}

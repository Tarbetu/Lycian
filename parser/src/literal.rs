use crate::LiteralIndex;

#[derive(Debug, PartialEq)]
pub struct Literal {
    index: LiteralIndex,
    kind: LiteralType,
}

#[derive(Debug, PartialEq)]
pub enum LiteralType {
    Integer,
    Float,
    Boolean,
    String,
    LiteralList(Box<LiteralType>),
    LiteralMap(Box<LiteralType>, Box<LiteralType>),
}

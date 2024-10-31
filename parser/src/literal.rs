use crate::LiteralIndex;

pub struct Literal {
    index: LiteralIndex,
    kind: LiteralType,
}

pub enum LiteralType {
    Integer,
    Float,
    Boolean,
    String,
    LiteralList(Box<LiteralType>),
    LiteralMap(Box<LiteralType>, Box<LiteralType>),
}

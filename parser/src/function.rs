use ahash::AHashMap;

use crate::Expression;
use crate::NameIndex;
use crate::Pattern;

pub struct Function {
    pub name: NameIndex,
    pub params: Vec<Pattern>,
    pub return_type: Option<Expression>,
    pub environment: Option<AHashMap<NameIndex, Vec<Function>>>,
    pub body: Expression,
    pub decorator: String,
}

impl Function {
    pub fn no_tail_call(&self) -> bool {
        self.decorator == "tailcall:false"
    }

    pub fn no_memoization(&self) -> bool {
        self.decorator == "memoization:false"
    }
}

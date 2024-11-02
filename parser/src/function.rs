use crate::Expression;
use crate::Name;

pub struct Function {
    pub class: Option<Name>,
    pub name: Name,
    pub params: Vec<Expression>,
    pub return_type: Expression,
    pub body: Box<Expression>,
    pub is_recursive: bool,
    pub decorator: Option<String>,
}

impl Function {
    pub fn no_tail_call(&self) -> bool {
        if let Some(decorator) = &self.decorator {
            decorator == "tailcall:false"
        } else {
            false
        }
    }

    pub fn is_constexpr(&self) -> bool {
        (self.return_type.is_constexpr() || self.body.is_constexpr()) && !self.is_recursive
    }
}

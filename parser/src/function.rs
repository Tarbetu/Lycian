use crate::Expression;
use crate::NameIndex;

pub struct Function {
    pub class: Option<NameIndex>,
    pub name: NameIndex,
    pub params: Vec<Expression>,
    pub return_type: Expression,
    pub body: Expression,
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
        self.return_type.is_constexpr() || self.body.is_constexpr()
    }
}

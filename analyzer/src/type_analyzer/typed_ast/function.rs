use parser::Function;

use super::TypedExpression;
use crate::type_analyzer::Pattern;
use crate::type_analyzer::Type;

pub struct TypedFunction {
    pub function: Function,
    pub itself: Type,
    pub params: Vec<Pattern>,
    pub return_type: Option<Type>,
    pub body: TypedExpression,
}

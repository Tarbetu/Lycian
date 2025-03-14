use crate::{resolution_error::TypeResult, type_analyzer::Type};
use parser::EntityTable;

#[derive(Debug, PartialEq)]
pub struct TypedExpression {
    expression: parser::Expression,
    expression_type: Type,
}

impl TypedExpression {
    pub fn from_expr(expression: &parser::Expression, entities: &EntityTable) -> TypeResult<Self> {
        unimplemented!()
    }
}

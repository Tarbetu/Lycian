use crate::type_analyzer::Type;

pub struct TypedExpression {
    expression: parser::Expression,
    expression_type: Type,
}

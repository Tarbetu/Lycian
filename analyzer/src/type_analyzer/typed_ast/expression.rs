use crate::type_analyzer::PrimitiveType;
use crate::AnalysisPipeline;
use crate::{
    resolution_error::{TypeError, TypeErrorKind, TypeResult},
    type_analyzer::Type,
};
use rayon::prelude::*;

#[derive(Debug, PartialEq)]
pub struct TypedExpression {
    expression: parser::Expression,
    expression_type: Type,
}

impl TypedExpression {
    pub fn from_expr(
        expression: &parser::Expression,
        pipeline: &AnalysisPipeline,
    ) -> TypeResult<Self> {
        use parser::Expression::*;
        match expression {
            Literal(index) => {
                let literal = pipeline.literals.get(index).expect("Invalid literal index");

                Ok(TypedExpression {
                    expression: expression.clone(),
                    expression_type: Self::type_from_literal(literal, pipeline)?,
                })
            }
            Grouping(expr) => Self::from_expr(expr, pipeline),
            Binary(lhs, opr, rhs) => {
                use parser::Operator::*;
                match opr {
                    Add | Substract | Multiply | Divide | Modulo => {
                        let (lhs, rhs) = rayon::join(
                            || Self::from_expr(lhs, pipeline),
                            || Self::from_expr(rhs, pipeline),
                        );
                        let (lhs, rhs) = (lhs?, rhs?);

                        if lhs.expression_type != rhs.expression_type {
                            Err(TypeError {
                                kind: TypeErrorKind::TypeMismatchInBinaryExpression,
                                index: None,
                                line: None,
                            })
                        } else {
                            Ok(TypedExpression {
                                expression: expression.clone(),
                                expression_type: lhs.expression_type,
                            })
                        }
                    }
                    Equal | NotEqual | Greater | GreaterOrEqual | Smaller | SmallerOrEqual => {
                        let (lhs, rhs) = rayon::join(
                            || Self::from_expr(lhs, pipeline),
                            || Self::from_expr(rhs, pipeline),
                        );
                        let (lhs, rhs) = (lhs?, rhs?);

                        if lhs.expression_type != rhs.expression_type {
                            Err(TypeError {
                                kind: TypeErrorKind::TypeMismatchInBinaryExpression,
                                index: None,
                                line: None,
                            })
                        } else {
                            Ok(TypedExpression {
                                expression: expression.clone(),
                                expression_type: lhs.expression_type,
                            })
                        }
                    }
                    And | Or => {
                        let lhs = Self::from_expr(lhs, pipeline)?;
                        let rhs = Self::from_expr(rhs, pipeline)?;

                        if lhs.expression_type != Type::Primitive(PrimitiveType::Bool)
                            || rhs.expression_type != Type::Primitive(PrimitiveType::Bool)
                        {
                            Err(TypeError {
                                kind: TypeErrorKind::BooleanNeededWithLogicalOperators,
                                index: None,
                                line: None,
                            })
                        } else {
                            Ok(TypedExpression {
                                expression: expression.clone(),
                                expression_type: Type::Primitive(PrimitiveType::Bool),
                            })
                        }
                    }

                    _ => panic!("Unexpected operator in binary expression: {:?}", opr),
                }
            }
            IndexOperator(structure, index) => {
                let structure = Self::from_expr(structure, pipeline)?;
                let index = Self::from_expr(index, pipeline)?;

                unimplemented!()
            }
            _ => todo!(),
        }
    }

    fn type_from_literal(
        literal: &parser::Literal,
        pipeline: &AnalysisPipeline,
    ) -> TypeResult<Type> {
        use parser::Literal::*;
        match literal {
            Integer(big_float) => {
                if !(usize::MIN..usize::MAX).contains(big_float) {
                    Err(TypeError {
                        kind: TypeErrorKind::IntegerOutOfRange,
                        index: None,
                        line: None,
                    })
                } else {
                    Ok(Type::Primitive(PrimitiveType::LiteralInteger))
                }
            }
            Float(big_float) => {
                if !(f64::MIN..f64::MAX).contains(big_float) {
                    Err(TypeError {
                        kind: TypeErrorKind::FloatOutOfRange,
                        index: None,
                        line: None,
                    })
                } else {
                    Ok(Type::Primitive(PrimitiveType::LiteralFloat))
                }
            }
            Boolean(_) => Ok(Type::Primitive(PrimitiveType::Bool)),

            Char(_) => Ok(Type::Primitive(PrimitiveType::Char)),

            Str(_) => Ok(Type::Primitive(PrimitiveType::Str)),

            LiteralList(literals) => {
                if literals.is_empty() {
                    return Ok(Type::Primitive(PrimitiveType::EmptyList));
                }

                let mut types_of_list: Vec<_> = literals
                    .par_iter()
                    .map(|expr_or_literal| {
                        use either::Either::*;
                        match expr_or_literal {
                            Left(expr) => {
                                Self::from_expr(expr, pipeline).map(|typed| typed.expression_type)
                            }
                            Right(literal) => Self::type_from_literal(literal, pipeline),
                        }
                    })
                    .collect();

                if types_of_list.par_iter().any(|t| t.is_err()) {
                    return Err(TypeError {
                        kind: TypeErrorKind::MultipleError(
                            types_of_list
                                .into_par_iter()
                                .filter_map(|e| e.err())
                                .collect(),
                        ),
                        index: None,
                        line: None,
                    });
                }

                let first_type = types_of_list.swap_remove(0)?;

                if types_of_list
                    .par_iter()
                    .filter(|t| t.as_ref().is_ok_and(|t| t == &first_type))
                    .count()
                    > 0
                {
                    Ok(Type::Primitive(PrimitiveType::List(Box::new(first_type))))
                } else {
                    Err(TypeError {
                        kind: TypeErrorKind::MixedTypesInList,
                        index: None,
                        line: None,
                    })
                }
            }

            LiteralMap(map) => {
                if map.is_empty() {
                    return Ok(Type::Primitive(PrimitiveType::EmptyMap));
                }

                let mut types_of_keys: Vec<_> =
                    map.keys()
                        .par_bridge()
                        .map(|expr_or_literal| {
                            use either::Either::*;
                            match expr_or_literal {
                                Left(expr) => Self::from_expr(expr, pipeline)
                                    .map(|typed| typed.expression_type),
                                Right(literal) => Self::type_from_literal(literal, pipeline),
                            }
                        })
                        .collect();
                let mut types_of_values: Vec<_> =
                    map.values()
                        .par_bridge()
                        .map(|expr_or_literal| {
                            use either::Either::*;
                            match expr_or_literal {
                                Left(expr) => Self::from_expr(expr, pipeline)
                                    .map(|typed| typed.expression_type),
                                Right(literal) => Self::type_from_literal(literal, pipeline),
                            }
                        })
                        .collect();

                if types_of_keys
                    .par_iter()
                    .chain(types_of_values.par_iter())
                    .any(|t| t.is_err())
                {
                    return Err(TypeError {
                        kind: TypeErrorKind::MultipleError(
                            types_of_keys
                                .into_par_iter()
                                .chain(types_of_values)
                                .filter_map(|e| e.err())
                                .collect(),
                        ),
                        index: None,
                        line: None,
                    });
                }

                let first_key = types_of_keys.swap_remove(0)?;
                let first_value = types_of_values.swap_remove(0)?;

                let (are_keys_same, are_values_same) = rayon::join(
                    || {
                        types_of_keys
                            .par_iter()
                            .filter(|t| t.as_ref().is_ok_and(|t| t == &first_key))
                            .count()
                            > 0
                    },
                    || {
                        types_of_values
                            .par_iter()
                            .filter(|t| t.as_ref().is_ok_and(|t| t == &first_value))
                            .count()
                            > 0
                    },
                );
                if are_keys_same && are_values_same {
                    Ok(Type::Primitive(PrimitiveType::Map(
                        Box::new(first_key),
                        Box::new(first_value),
                    )))
                } else {
                    Err(TypeError {
                        kind: TypeErrorKind::MixedTypesInMap,
                        index: None,
                        line: None,
                    })
                }
            }
        }
    }
}

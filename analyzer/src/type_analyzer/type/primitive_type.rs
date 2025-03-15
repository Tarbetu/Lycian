use super::Type;
use parser::Expression;
use parser::PatternName;

use crate::resolution_error::TypeResult;
use crate::resolution_error::{TypeError, TypeErrorKind};
use crate::{Entity, EntityTable};

#[derive(Debug, PartialEq, PartialOrd)]
pub enum PrimitiveType {
    Bool,
    Char,
    Str,
    Float32,
    Float64,
    Isize,
    Int8,
    Int16,
    Int32,
    Int64,
    Int128,
    Usize,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Uint128,
    LiteralInteger, // Untyped integer literal, we need to infer it
    LiteralFloat,   // Untyped float literal, we need to infer it
    EmptyList,      // Empty list literal, we need to infer the type
    EmptyMap,       // Empty map literal, we need to infer the type
    BigInteger,     // We haven't implemented this yet, so this is a sign of error
    BigFloat,       // Same with BigInteger
    List(Box<Type>),
    Map(Box<Type>, Box<Type>),
}

impl PrimitiveType {
    pub fn from_expr(value: &Expression, entities: &EntityTable) -> TypeResult<Self> {
        let Expression::Call {
            name_id,
            args,
            caller: None,
            block: None,
        } = value
        else {
            return Err(TypeError {
                kind: TypeErrorKind::UnexpectedExpressionForPrimitive,
                index: None,
                line: Some(0),
            });
        };

        let Entity { name, .. } = entities.get(name_id).expect("Entity not found!");

        match name.as_ref() {
            "Bool" => Ok(PrimitiveType::Bool),
            "Char" => Ok(PrimitiveType::Char),
            "F32" => Ok(PrimitiveType::Float32),
            "F64" => Ok(PrimitiveType::Float64),
            "Isize" => Ok(PrimitiveType::Isize),
            "Int8" => Ok(PrimitiveType::Int8),
            "Int16" => Ok(PrimitiveType::Int16),
            "Int32" => Ok(PrimitiveType::Int32),
            "Int64" => Ok(PrimitiveType::Int64),
            "Int128" => Ok(PrimitiveType::Int128),
            "Usize" => Ok(PrimitiveType::Usize),
            "Uint8" => Ok(PrimitiveType::Uint8),
            "Uint16" => Ok(PrimitiveType::Uint16),
            "Uint32" => Ok(PrimitiveType::Uint32),
            "Uint64" => Ok(PrimitiveType::Uint64),
            "Uint128" => Ok(PrimitiveType::Uint128),
            "BigInteger" | "BigFloat" => Err(TypeError {
                kind: TypeErrorKind::NotImplementedYet,
                index: None,
                line: Some(0),
            }),
            "List" => {
                if args.len() != 1 {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidList,
                        index: None,
                        line: Some(0),
                    });
                }

                let parser::Pattern {
                    name: PatternName::NoName,
                    condition: None,
                    value: Some(ref value),
                } = args[0]
                else {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidList,
                        index: None,
                        line: Some(0),
                    });
                };

                Ok(PrimitiveType::List(Box::new(Type::from_expr(
                    value, entities,
                )?)))
            }
            "Map" => {
                if args.len() != 2 {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidMap,
                        index: None,
                        line: Some(0),
                    });
                }

                let parser::Pattern {
                    name: PatternName::NoName,
                    condition: None,
                    value: Some(ref key),
                } = args[0]
                else {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidMap,
                        index: None,
                        line: Some(0),
                    });
                };

                let parser::Pattern {
                    name: PatternName::NoName,
                    condition: None,
                    value: Some(ref value),
                } = args[1]
                else {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidMap,
                        index: None,
                        line: Some(0),
                    });
                };

                Ok(PrimitiveType::Map(
                    Box::new(Type::from_expr(key, entities)?),
                    Box::new(Type::from_expr(value, entities)?),
                ))
            }
            _ => Err(TypeError {
                kind: TypeErrorKind::InvalidPrimitiveType,
                index: None,
                line: Some(0),
            }),
        }
    }
}

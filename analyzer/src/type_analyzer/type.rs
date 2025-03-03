mod class_state;
pub use class_state::ClassState;
mod parameter;
pub use parameter::Parameter;
mod primitive_type;
pub use primitive_type::PrimitiveType;
mod special_type;
pub use special_type::SpecialType;

use parser::{EntityIndex, LiteralIndex};

pub enum Type {
    Class {
        entity: EntityIndex,
        ancestors: Vec<EntityIndex>,
        states: Vec<ClassState>,
    },
    Function {
        entity: EntityIndex,
        parameters: Vec<Parameter>,
        return_type: Box<Type>,
    },
    FunctionApplication {
        function: EntityIndex,
        arguments: Vec<Parameter>,
    },
    Literal(LiteralIndex),
    Primitive(PrimitiveType),
    Special(SpecialType),
}

impl Type {
    pub fn is_primitive(&self) -> bool {
        matches!(self, Type::Primitive(_))
    }

    pub fn is_special(&self) -> bool {
        matches!(self, Type::Special(_))
    }

    pub fn matches(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Primitive(a), Type::Primitive(b)) => a == b,
            (Type::Special(a), Type::Special(b)) => a == b,
            (Type::Class { entity: a, .. }, Type::Class { entity: b, .. }) => a == b,
            (
                Type::Function {
                    parameters: p1,
                    return_type: r1,
                    ..
                },
                Type::Function {
                    parameters: p2,
                    return_type: r2,
                    ..
                },
            ) => {
                if (!r1.matches(r2)) && p1.len() != p2.len() {
                    return false;
                }

                p1.iter()
                    .zip(p2.iter())
                    .all(|(param1, param2)| param1.matches(param2))
            }
            (
                Type::FunctionApplication { function: a, .. },
                Type::FunctionApplication { function: b, .. },
            ) => a == b,
            (Type::Literal(a), Type::Literal(b)) => a == b,
            _ => false,
        }
    }
}

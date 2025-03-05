use parser::{EntityIndex, LiteralIndex};

use crate::{
    resolution_error::{TypeError, TypeErrorKind},
    AnalysisPipeline,
};

use super::PrimitiveType;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PatternPrecedence {
    Literal = 0,
    // Guard = 10,
    Function = 20,
    NoConstrain = 255,
}

pub enum Pattern {
    /// A pattern that is only constrained by the function of the entity
    /// Every type is a function in Lycian
    /// Example: `x: MyFunc`
    FunctionConstrained {
        entity: EntityIndex,
        function_constraint: EntityIndex,
    },
    /// A pattern that is constrained by a literal value
    /// Example: `x: 5`
    LiteralConstrained {
        entity: EntityIndex,
        base_type: PrimitiveType,
        literal_value: LiteralIndex,
    },
    // A pattern that is constrained by a condition
    // A function constraint is also present
    // Example: `x: Integer when x > 5`
    // To be implemented
    // GuardConstrained {
    //     entity: EntityIndex,
    //     base_class: EntityIndex,
    //     guard: parser::Expression,
    // },
    /// A pattern that is not constrained in any way
    /// Nothing is present, so the entity can be any value
    /// The type is deduced in the context of the Match expr
    /// In function call, this is a nameless argument
    /// In function definition, this is a parameter without a type and don't confuse with dynamic typing
    /// Example: `x`
    NoConstrain { entity: EntityIndex },
}

impl Pattern {
    pub fn matches(&self, other: &Pattern, pipeline: &AnalysisPipeline) -> bool {
        use Pattern::*;
        match (self, other) {
            (
                LiteralConstrained {
                    base_type: b1,
                    literal_value: l1,
                    ..
                },
                LiteralConstrained {
                    literal_value: l2,
                    base_type: b2,
                    ..
                },
            ) => b1 == b2 && pipeline.literals.get(l1) == pipeline.literals.get(l2),
            (
                FunctionConstrained {
                    function_constraint: c1,
                    ..
                },
                FunctionConstrained {
                    function_constraint: c2,
                    ..
                },
            ) => c1 == c2,
            _ => false,
        }
    }
}

impl From<&Pattern> for PatternPrecedence {
    fn from(value: &Pattern) -> Self {
        match value {
            Pattern::LiteralConstrained { .. } => PatternPrecedence::Literal,
            // Pattern::GuardConstrained { .. } => PatternPrecedence::Guard,
            Pattern::FunctionConstrained { .. } => PatternPrecedence::Function,
            Pattern::NoConstrain { .. } => PatternPrecedence::NoConstrain,
        }
    }
}

impl TryFrom<&parser::Pattern> for Pattern {
    type Error = TypeError;

    fn try_from(value: &parser::Pattern) -> Result<Self, Self::Error> {
        use parser::PatternName::*;

        if let Some(condition) = &value.condition {
            return Err(TypeError {
                kind: TypeErrorKind::GuardPatternNotSupported(format!("{:?}", condition)),
                index: None,
                line: None,
            });
        }

        let name = match value.name {
            Name(index) => index,
            // We are in the context of method
            ClassSelf => unimplemented!(),
            // This means we are in a context of a function call
            NoName => unimplemented!(),
        };
        unimplemented!()
    }
}

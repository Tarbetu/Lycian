use parser::EntityIndex;
use std::fmt;

pub struct TypeError {
    pub kind: TypeErrorKind,
    pub index: Option<EntityIndex>,
    pub line: Option<usize>,
}

pub enum TypeErrorKind {
    UnexpectedExpressionForPrimitive,
    InvalidPrimitiveType,
    NotImplementedYet,
    InvalidList,
    InvalidMap,
    InvalidConstrainlessPattern,
    TypeMismatchInBinaryExpression,
    BooleanNeededWithLogicalOperators,
    MixedTypesInList,
    MixedTypesInMap,
    IntegerOutOfRange,
    FloatOutOfRange,
    MultipleError(Vec<TypeError>),
}

impl fmt::Display for TypeErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TypeErrorKind::*;
        match self {
            UnexpectedExpressionForPrimitive => {
                write!(f, "Unexpected expression for the primitive")
            }
            InvalidPrimitiveType => write!(
                f,
                "You have to use a primitive type, but given a non-primitive type"
            ),
            NotImplementedYet => write!(f, "BigInteger and BigFloat not implemented yet"),
            InvalidList => write!(f, "Could not read the list parameters"),
            InvalidMap => write!(f, "Could not read the map parameters"),
            InvalidConstrainlessPattern => write!(f, "Contrainless patterns needs a name"),
            TypeMismatchInBinaryExpression => write!(
                f,
                "Both sides of a binary expression should have the same type"
            ),
            BooleanNeededWithLogicalOperators => {
                write!(f, "You can only use logical operators with booleans")
            }
            MixedTypesInList => write!(f, "All elements of a list must have the same type"),
            MixedTypesInMap => write!(f, "All keys and all values must have the same type"),
            IntegerOutOfRange => write!(f, "Integer exceeds the limit even for Usize"),
            FloatOutOfRange => write!(f, "Float exceeds the limit even for F64"),
            MultipleError(errors) => {
                for error in errors {
                    write!(f, "{}", error.kind)?;
                }
                Ok(())
            }
        }
    }
}

pub type TypeResult<T> = Result<T, TypeError>;

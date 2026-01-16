use std::fmt::Display;
use std::rc::Rc;
use synonym::Synonym;

use crate::type_bounds::TypeConstraint;

#[derive(Synonym)]
pub struct TypeId(pub usize);

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum IntegerNumber {
    Int8,
    Int16,
    Int32,
    Int64,
    Int128,
    IntSize,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    UInt128,
    UIntSize,
    Literal,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum FloatingNumber {
    Float32,
    Float64,
    Literal,
}
#[derive(Debug, PartialEq, Copy, Clone)]

pub enum PrimitiveType {
    Integer(IntegerNumber),
    Floating(FloatingNumber),
    Boolean,
    Char,
    Void,
    LiteralTrue,
    LiteralFalse,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum CompoundType {
    Array,
    LinkedList,
    EmptyArray,
    EmptyList,
    String,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum EmbeddedTypeName {
    Primitive(PrimitiveType),
    Compound(CompoundType),
    Function,
    Object,
}

impl Display for EmbeddedTypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EmbeddedTypeName::Primitive(p) => write!(f, "{:?}", p),
            EmbeddedTypeName::Compound(c) => write!(f, "{:?}", c),
            EmbeddedTypeName::Function => write!(f, "Function"),
            EmbeddedTypeName::Object => write!(f, "Object"),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TypeSize {
    Exact(usize),
    UnionSize,
    PointerSized,
    Dynamic,
    ClosureSize,
    OriginSize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeDefinition<'a> {
    Object,
    Literal {
        id: TypeId,
        origin_id: TypeId,
        node: syntax::Literal,
    },
    EmbeddedType {
        id: TypeId,
        name: EmbeddedTypeName,
        size: TypeSize,
    },
    Origin {
        id: TypeId,
        binding_id: scope::BindingId,
        name: Rc<String>,
        size: TypeSize,
        node: &'a syntax::Class,
    },
    Variant {
        id: TypeId,
        binding_id: scope::BindingId,
        name: Rc<String>,
        origin_id: TypeId,
        node: &'a [syntax::Pattern],
    },
    Function {
        id: TypeId,
        params: Vec<(TypeId, syntax::PatternName)>,
        args: Vec<(TypeId, &'a syntax::Expression)>,
        return_type: TypeId,
    },
    TypeInstance {
        id: TypeId,
        origin_id: TypeId,
        args: Rc<Vec<TypeId>>,
    },
}

impl<'a> TypeDefinition<'a> {
    pub fn name(&self) -> Rc<String> {
        use TypeDefinition::*;

        match self {
            Object => Rc::new(String::from("Object")),
            EmbeddedType { name, .. } => name.to_string().into(),
            Literal { .. } => unimplemented!(),
            Origin { name, .. } => name.clone(),
            Variant { .. } => unimplemented!(),
            Function { .. } => unimplemented!(),
            TypeInstance { .. } => unimplemented!(),
        }
    }

    pub fn id(&self) -> TypeId {
        use TypeDefinition::*;

        match self {
            Object => TypeId(0),
            Literal { id, .. } => *id,
            EmbeddedType { id, .. } => *id,
            Origin { id, .. } => *id,
            Variant { id, .. } => *id,
            Function { id, .. } => *id,
            TypeInstance { id, .. } => *id,
        }
    }

    pub fn type_args(&'a self) -> &Rc<Vec<TypeId>> {
        use TypeDefinition::*;

        match self {
            TypeInstance { args, .. } => args,
            _ => panic!("Type does not have type arguments"),
        }
    }

    pub fn exact_size(&self) -> usize {
        use TypeDefinition::*;

        match self {
            Origin { size, .. } | EmbeddedType { size, .. } => match size {
                TypeSize::Exact(s) => *s,
                TypeSize::PointerSized => 666,
                _ => panic!("Type does not have an exact size"),
            },
            Function { .. } => 666,
            Literal { .. } => {
                panic!("Literal type does not have an exact size")
            }
            Variant { .. } => panic!("Variant type does not have an exact size"),
            Object => panic!("Object type does not have an exact size"),
            TypeInstance { .. } => {
                panic!("TypeInstance does not have an exact size")
            }
        }
    }

    pub fn origin_id(&self) -> TypeId {
        use TypeDefinition::*;

        match self {
            Origin { id, .. } | EmbeddedType { id, .. } => *id,
            Literal { origin_id, .. }
            | Variant { origin_id, .. }
            | TypeInstance { origin_id, .. } => *origin_id,
            Object | Function { .. } => TypeId(0),
        }
    }

    pub fn size(&self) -> TypeSize {
        use TypeDefinition::*;

        match self {
            Origin { size, .. } | EmbeddedType { size, .. } => *size,
            Literal { .. } => TypeSize::OriginSize,
            Variant { .. } => TypeSize::UnionSize,
            Object => TypeSize::Dynamic,
            TypeInstance { .. } => TypeSize::Dynamic,
            Function { .. } => TypeSize::PointerSized,
        }
    }

    pub(crate) fn is_object(&self) -> bool {
        matches!(self, TypeDefinition::Object)
    }

    pub(crate) fn is_float32(&self) -> bool {
        matches!(
            self,
            TypeDefinition::EmbeddedType {
                name: EmbeddedTypeName::Primitive(PrimitiveType::Floating(FloatingNumber::Float32)),
                ..
            }
        )
    }

    pub(crate) fn is_float64(&self) -> bool {
        matches!(
            self,
            TypeDefinition::EmbeddedType {
                name: EmbeddedTypeName::Primitive(PrimitiveType::Floating(FloatingNumber::Float64)),
                ..
            }
        )
    }

    pub(crate) fn is_floating(&self) -> bool {
        matches!(
            self,
            TypeDefinition::EmbeddedType {
                name: EmbeddedTypeName::Primitive(PrimitiveType::Floating(
                    FloatingNumber::Float32 | FloatingNumber::Float64
                )),
                ..
            }
        )
    }

    pub(crate) fn is_signed_number(&self) -> bool {
        matches!(
            self,
            TypeDefinition::EmbeddedType {
                name: EmbeddedTypeName::Primitive(PrimitiveType::Integer(
                    IntegerNumber::Int8
                        | IntegerNumber::Int16
                        | IntegerNumber::Int32
                        | IntegerNumber::Int64
                        | IntegerNumber::Int128
                        | IntegerNumber::IntSize
                )),
                ..
            }
        )
    }

    pub(crate) fn is_unsigned_number(&self) -> bool {
        matches!(
            self,
            TypeDefinition::EmbeddedType {
                name: EmbeddedTypeName::Primitive(PrimitiveType::Integer(
                    IntegerNumber::UInt8
                        | IntegerNumber::UInt16
                        | IntegerNumber::UInt32
                        | IntegerNumber::UInt64
                        | IntegerNumber::UInt128
                        | IntegerNumber::UIntSize
                )),
                ..
            }
        )
    }
}

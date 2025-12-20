use crate::Hierarchy;
use std::collections::HashSet;
use std::fmt::Display;
use std::mem::discriminant;
use std::rc::Rc;
use synonym::Synonym;

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
        node: &'a syntax::Literal,
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
        args: Vec<(TypeId, &'a syntax::Expression)>,
        origin_id: TypeId,
        size: TypeSize,
        node: &'a syntax::Function,
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
            Origin { size, .. } | Function { size, .. } | EmbeddedType { size, .. } => match size {
                TypeSize::Exact(s) => *s,
                TypeSize::PointerSized => 666,
                _ => panic!("Type does not have an exact size"),
            },
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
            | Function { origin_id, .. }
            | TypeInstance { origin_id, .. } => *origin_id,
            Object => TypeId(0),
        }
    }

    pub fn size(&self) -> TypeSize {
        use TypeDefinition::*;

        match self {
            Origin { size, .. } | Function { size, .. } | EmbeddedType { size, .. } => *size,
            Literal { .. } => TypeSize::OriginSize,
            Variant { .. } => TypeSize::UnionSize,
            Object => TypeSize::Dynamic,
            TypeInstance { .. } => TypeSize::Dynamic,
        }
    }

    // pub(crate) fn is_supertype(
    //     super_type: &'a Self,
    //     sub_type: &'a Self,
    //     hierarchy: &'a Hierarchy<'a>,
    // ) -> bool {
    //     Self::check_supertype(super_type, sub_type, hierarchy, &mut HashSet::new())
    // }

    // fn check_supertype(
    //     super_type: &Self,
    //     sub_type: &Self,
    //     hierarchy: &'a Hierarchy<'a>,
    //     visited: &mut HashSet<(TypeId, TypeId)>,
    // ) -> bool {
    //     if visited.contains(&(super_type.id(), sub_type.id())) {
    //         return false;
    //     }

    //     if super_type.id() == sub_type.id() {
    //         return true;
    //     }

    //     if sub_type.is_object() {
    //         return false;
    //     }

    //     if super_type.is_object() {
    //         return true;
    //     }

    //     if (super_type.is_signed_number() || sub_type.is_unsigned_number())
    //         && (super_type.exact_size() > sub_type.exact_size())
    //         && sub_type.size() != TypeSize::PointerSized
    //     {
    //         return true;
    //     }

    //     if sub_type.is_float32() && super_type.is_float64() {
    //         return true;
    //     }

    //     if sub_type.is_float64() && super_type.is_floating() {
    //         return false;
    //     }

    //     if let (
    //         TypeDefinition::Origin {
    //             id: super_type_id, ..
    //         },
    //         TypeDefinition::Origin { parent_ids, .. },
    //     ) = (super_type, sub_type)
    //         && parent_ids
    //             .values()
    //             .any(|value| value.contains(super_type_id))
    //     {
    //         return true;
    //     }

    //     if let (
    //         TypeDefinition::Origin {
    //             id: super_type_id, ..
    //         },
    //         TypeDefinition::Variant { origin_id, .. }
    //         | TypeDefinition::Literal { origin_id, .. }
    //         | TypeDefinition::TypeInstance { origin_id, .. },
    //     ) = (super_type, sub_type)
    //     {
    //         if origin_id == super_type_id {
    //             return true;
    //         }

    //         let origin_type = hierarchy.types.get(origin_id).unwrap();

    //         return Self::check_supertype(super_type, origin_type, hierarchy, visited);
    //     }

    //     // Parametric contravarience
    //     if let (
    //         // TypeDefinition::TypeInstance {
    //         //     origin_id: super_origin_id,
    //         //     args: super_args,
    //         //     ..
    //         // } |
    //         TypeDefinition::Function {
    //             origin_id: super_origin_id,
    //             args: super_args,
    //             ..
    //         },
    //         // TypeDefinition::TypeInstance {
    //         //     origin_id: sub_origin_id,
    //         //     args: sub_args,
    //         //     ..
    //         // } |
    //         TypeDefinition::Function {
    //             origin_id: sub_origin_id,
    //             args: sub_args,
    //             ..
    //         },
    //     ) = (super_type, sub_type)
    //         && discriminant(super_type) == discriminant(sub_type)
    //         && super_origin_id == sub_origin_id
    //         && (super_args.starts_with(sub_args)
    //             || (super_args.len() >= sub_args.len()
    //                 && super_args.iter().zip(sub_args.iter()).all(
    //                     |((super_type_id, _super_arg), (sub_type_id, _sub_arg))| {
    //                         visited.insert((*super_type_id, *sub_type_id));
    //                         let super_type = hierarchy.types.get(super_type_id).unwrap();
    //                         let sub_type = hierarchy.types.get(sub_type_id).unwrap();
    //                         let result =
    //                             Self::check_supertype(super_type, sub_type, hierarchy, visited);
    //                         visited.remove(&(*super_type_id, *sub_type_id));
    //                         result
    //                     },
    //                 )))
    //     {
    //         return true;
    //     }

    //     // TODO: Eliminate the repetition
    //     if let (
    //         TypeDefinition::TypeInstance {
    //             origin_id: super_origin_id,
    //             args: super_args,
    //             ..
    //         },
    //         TypeDefinition::TypeInstance {
    //             origin_id: sub_origin_id,
    //             args: sub_args,
    //             ..
    //         }
    //             ) = (super_type, sub_type)
    //         && discriminant(super_type) == discriminant(sub_type)
    //         && super_origin_id == sub_origin_id
    //         && (super_args.starts_with(sub_args)
    //             || (super_args.len() >= sub_args.len()
    //                 && super_args.iter().zip(sub_args.iter()).all(
    //                     |(super_type_id, sub_type_id)| {
    //                         visited.insert((*super_type_id, *sub_type_id));
    //                         let super_type = hierarchy.types.get(super_type_id).unwrap();
    //                         let sub_type = hierarchy.types.get(sub_type_id).unwrap();
    //                         let result =
    //                             Self::check_supertype(super_type, sub_type, hierarchy, visited);
    //                         visited.remove(&(*super_type_id, *sub_type_id));
    //                         result
    //                     },
    //                 )))
    //     {
    //         return true;
    //     }

    //     false
    // }

    fn is_object(&self) -> bool {
        matches!(self, TypeDefinition::Object)
    }

    fn is_float32(&self) -> bool {
        matches!(
            self,
            TypeDefinition::EmbeddedType {
                name: EmbeddedTypeName::Primitive(PrimitiveType::Floating(FloatingNumber::Float32)),
                ..
            }
        )
    }

    fn is_float64(&self) -> bool {
        matches!(
            self,
            TypeDefinition::EmbeddedType {
                name: EmbeddedTypeName::Primitive(PrimitiveType::Floating(FloatingNumber::Float64)),
                ..
            }
        )
    }

    fn is_floating(&self) -> bool {
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

    fn is_signed_number(&self) -> bool {
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

    fn is_unsigned_number(&self) -> bool {
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

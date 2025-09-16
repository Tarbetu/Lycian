mod embedded_types;

use crate::definition::*;
use crate::error::TypeError;
use embedded_types::*;
use scope::ExprId;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Hierarchy<'a> {
    pub types: HashMap<TypeId, TypeDefinition<'a>>,
    last_id: TypeId,
    pub name_to_origin_id: HashMap<Rc<String>, TypeId>,
    pub variants_of_origin: HashMap<TypeId, Vec<TypeId>>,
    pub expr_to_type: ExprToTypeTable,
    pub expr_constraints: HashMap<TypeId, Constraint>,
    pub embedded_types: &'static EmbeddedTypes,
    pub type_instances: HashMap<TypeId, Vec<TypeId>>,
    pub scope_hierarchy: scope::Hierarchy<'a>,
}

#[derive(Default)]
pub struct ExprToTypeTable {
    ok: HashMap<ExprId, TypeId>,
    err: HashMap<ExprId, TypeError>,
    deferred: Vec<ExprId>,
}

impl<'a> Hierarchy<'a> {
    pub(crate) fn new(scope_hierarchy: scope::Hierarchy<'a>) -> Self {
        Self {
            types: HashMap::new(),
            last_id: TypeId(22),
            name_to_origin_id: HashMap::new(),
            variants_of_origin: HashMap::new(),
            expr_to_type: ExprToTypeTable::default(),
            expr_constraints: HashMap::new(),
            embedded_types: &EMBEDDED_TYPES,
            type_instances: HashMap::new(),
            scope_hierarchy,
        }
        .install_embedded_types()
        .install_custom_types()
    }

    fn install_embedded_types(mut self) -> Self {
        self.types.extend([
            (EMBEDDED_TYPES.object, TypeDefinition::Object),
            (
                EMBEDDED_TYPES.int8,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.int8,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Integer(
                        IntegerNumber::Int8,
                    ))),
                    parent_ids: HashMap::new(),
                    size: TypeSize::Exact(1),
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.int16,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.int16,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Integer(
                        IntegerNumber::Int16,
                    ))),
                    parent_ids: HashMap::new(),
                    size: TypeSize::Exact(2),
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.int32,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.int32,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Integer(
                        IntegerNumber::Int32,
                    ))),
                    parent_ids: HashMap::new(),
                    size: TypeSize::Exact(4),
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.int64,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.int64,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Integer(
                        IntegerNumber::Int64,
                    ))),
                    parent_ids: HashMap::new(),
                    size: TypeSize::Exact(8),
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.int128,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.int128,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Integer(
                        IntegerNumber::Int128,
                    ))),
                    parent_ids: HashMap::new(),
                    size: TypeSize::Exact(16),
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.intSize,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.intSize,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Integer(
                        IntegerNumber::IntSize,
                    ))),
                    parent_ids: HashMap::new(),
                    size: TypeSize::PointerSized,
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.uInt8,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.uInt8,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Integer(
                        IntegerNumber::UInt8,
                    ))),
                    parent_ids: HashMap::new(),
                    size: TypeSize::Exact(1),
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.uInt16,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.uInt16,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Integer(
                        IntegerNumber::UInt16,
                    ))),
                    parent_ids: HashMap::new(),
                    size: TypeSize::Exact(2),
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.uInt32,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.uInt32,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Integer(
                        IntegerNumber::UInt32,
                    ))),
                    parent_ids: HashMap::new(),
                    size: TypeSize::Exact(4),
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.uInt64,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.uInt64,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Integer(
                        IntegerNumber::UInt64,
                    ))),
                    parent_ids: HashMap::new(),
                    size: TypeSize::Exact(8),
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.uInt128,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.uInt128,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Integer(
                        IntegerNumber::UInt128,
                    ))),
                    parent_ids: HashMap::new(),
                    size: TypeSize::Exact(16),
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.uIntSize,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.uIntSize,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Integer(
                        IntegerNumber::UIntSize,
                    ))),
                    parent_ids: HashMap::new(),
                    size: TypeSize::PointerSized,
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.float32,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.float32,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Floating(
                        FloatingNumber::Float32,
                    ))),
                    parent_ids: HashMap::new(),
                    size: TypeSize::Exact(4),
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.float64,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.float64,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Floating(
                        FloatingNumber::Float64,
                    ))),
                    parent_ids: HashMap::new(),
                    size: TypeSize::Exact(8),
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.boolean,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.boolean,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Boolean)),
                    parent_ids: HashMap::new(),
                    size: TypeSize::Exact(1),
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.char,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.char,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Char)),
                    parent_ids: HashMap::new(),
                    size: TypeSize::Exact(4),
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.void,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.void,
                    name: TypeName::Embedded(EmbeddedType::Primitive(PrimitiveType::Void)),
                    parent_ids: HashMap::new(),
                    size: TypeSize::Exact(0),
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.array,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.array,
                    name: TypeName::Embedded(EmbeddedType::Compound(CompoundType::Array)),
                    parent_ids: HashMap::new(),
                    size: TypeSize::PointerSized,
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.linked_list,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.linked_list,
                    name: TypeName::Embedded(EmbeddedType::Compound(CompoundType::LinkedList)),
                    parent_ids: HashMap::new(),
                    size: TypeSize::PointerSized,
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.string,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.string,
                    name: TypeName::Embedded(EmbeddedType::Compound(CompoundType::String)),
                    parent_ids: HashMap::new(),
                    size: TypeSize::PointerSized,
                    node: None,
                },
            ),
            (
                EMBEDDED_TYPES.function,
                TypeDefinition::Origin {
                    id: EMBEDDED_TYPES.function,
                    name: TypeName::Embedded(EmbeddedType::Function),
                    parent_ids: HashMap::new(),
                    size: TypeSize::ClosureSize,
                    node: None,
                },
            ),
        ]);

        self
    }

    fn install_custom_types(mut self) -> Self {
        for (_class_scope_id, class_scope) in self.scope_hierarchy.scopes.iter() {
            // let class_type_id = self.next_id();
            let class_type_id = self.last_id;
            self.last_id.0 += 1;

            let scope::SyntaxNode::Class(class) = class_scope.node else {
                panic!("Class expected for root scope");
            };

            self.name_to_origin_id
                .insert(class.name.clone(), class_type_id);

            self.types.insert(
                class_type_id,
                TypeDefinition::Origin {
                    id: class_type_id,
                    name: TypeName::Custom(class.name.clone()),
                    parent_ids: HashMap::new(), // This will handled by inheritence analysis
                    size: TypeSize::UnionSize,
                    node: Some(class),
                },
            );

            for (constructor_name, patterns) in class.constructors.iter() {
                let variant_id = self.last_id;
                self.last_id.0 += 1;

                self.types.insert(
                    variant_id,
                    TypeDefinition::Variant {
                        id: variant_id,
                        variant_name: constructor_name.clone(),
                        origin_id: class_type_id,
                        node: patterns,
                    },
                );
            }
        }

        self
    }
}

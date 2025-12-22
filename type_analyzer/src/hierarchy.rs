mod embedded_types;

use crate::definition::*;
pub use embedded_types::*;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::mem::discriminant;

pub struct Hierarchy<'a> {
    pub types: HashMap<TypeId, TypeDefinition<'a>>,
    last_id: TypeId,
    pub name_to_origin_id: HashMap<Rc<String>, TypeId>,
    pub variants_of_origin: HashMap<TypeId, HashMap<Rc<String>, TypeId>>,
    pub binding_to_type: HashMap<scope::BindingId, TypeId>,
    pub literal_types: LiteralTypeTable,
    pub embedded_types: &'static EmbeddedTypes,
    pub ancestors: HashMap<TypeId, Vec<TypeId>>,
    // Type Instances might be confusing
    // It's GenericType -> (TypeArgs, TypeInstanceId)
    pub type_instances: HashMap<TypeId, HashMap<Rc<Vec<TypeId>>, TypeId>>,
    pub scope_hierarchy: scope::Hierarchy<'a>,
    pub responds_to_table: HashMap<Rc<String>, TypeId>,
    pub call_table: HashMap<scope::ExprId, (scope::BindingId, TypeId)>,

    // TODO: Clean the cache after everything is done
    supertype_cache: HashMap<(TypeId, TypeId), bool>
}

#[derive(Default)]
pub struct LiteralTypeTable {
    pub integers: HashMap<syntax::Literal, TypeId>,
    pub floats: HashMap<syntax::Literal, TypeId>,
    pub chars: HashMap<syntax::Literal, TypeId>,
    pub strings: HashMap<syntax::Literal, TypeId>,
    pub lists: HashMap<syntax::Literal, TypeId>,
    pub arrays: HashMap<syntax::Literal, TypeId>,
}

impl<'a> Hierarchy<'a> {
    pub(crate) fn new(scope_hierarchy: scope::Hierarchy<'a>) -> Self {
        Self {
            types: HashMap::new(),
            last_id: TypeId(EMBEDDED_TYPES.count),
            name_to_origin_id: HashMap::new(),
            variants_of_origin: HashMap::new(),
            literal_types: LiteralTypeTable::default(),
            embedded_types: &EMBEDDED_TYPES,
            type_instances: HashMap::new(),
            scope_hierarchy,
            responds_to_table: HashMap::new(),
            binding_to_type: HashMap::new(),
            call_table: HashMap::new(),
            ancestors: HashMap::new(),
            supertype_cache: HashMap::new()
        }
        .install_embedded_types()
        .install_custom_types()
    }

    pub(crate) fn next_id(&mut self) -> TypeId {
        let result = self.last_id;
        self.last_id = TypeId(self.last_id.0 + 1);
        result
    }

    pub(crate) fn get_type_instance(&mut self, origin_id: TypeId, args: &Vec<TypeId>) -> TypeId {
        if let Some(type_instances) = self.type_instances.get(&origin_id) {
            if let Some(type_instance_id) = type_instances.get(args) {
                return *type_instance_id;
            }
        }

        self.push_type_instance(origin_id, args.to_vec())
    }

    pub(crate) fn push_type_instance(&mut self, origin_id: TypeId, args: Vec<TypeId>) -> TypeId {
        let id = self.next_id();
        let args = Rc::new(args);
        let type_def = TypeDefinition::TypeInstance {
            id,
            origin_id,
            args,
        };

        self.types.insert(id, type_def);

        let type_def = self.types.get(&id).expect("Origin type expected!");

        let type_instances = self
            .type_instances
            .entry(origin_id)
            .or_default();


        type_instances.insert(type_def.type_args().clone(), type_def.id());

        id
    }

    pub(crate) fn is_supertype(
        &mut self,
        super_type_id: TypeId,
        sub_type_id: TypeId,
    ) -> bool {
        if let Some(value) = self.supertype_cache.get(&(super_type_id, sub_type_id)).copied() {
            value
        } else {
            let super_type = self.types.get(&super_type_id).expect("Type must exist!");
            let sub_type = self.types.get(&sub_type_id).expect("Type must exist!");
            let result = self.check_supertype(super_type, sub_type, &mut HashSet::new());
            self.supertype_cache.insert((super_type_id, sub_type_id), result);
            result
        }
    }

    fn check_supertype(
        &self,
        super_type: &TypeDefinition,
        sub_type: &TypeDefinition,
        visited: &mut HashSet<(TypeId, TypeId)>,
    ) -> bool {
        if visited.contains(&(super_type.id(), sub_type.id())) {
            return false;
        }

        if super_type.id() == sub_type.id() {
            return true;
        }

        if sub_type.is_object() {
            return false;
        }

        if super_type.is_object() {
            return true;
        }

        if sub_type.is_float64() && super_type.is_floating() {
            return false;
        }

        if let (
            TypeDefinition::Origin {
                id: super_type_id, ..
            },
            TypeDefinition::Origin { id: sub_type_id, .. },
        ) = (super_type, sub_type)
            && self.is_parent(*super_type_id, *sub_type_id)
        {
            return true;
        }

        if let (
            TypeDefinition::Origin {
                id: super_type_id, ..
            },
            TypeDefinition::Variant { origin_id, .. }
            | TypeDefinition::Literal { origin_id, .. }
            | TypeDefinition::TypeInstance { origin_id, .. },
        ) = (super_type, sub_type)
        {
            if origin_id == super_type_id {
                return true;
            }

            let origin_type = self.types.get(origin_id).unwrap();

            return self.check_supertype(super_type, origin_type, visited);
        }

        // Parametric contravarience
        if let (
            // TypeDefinition::TypeInstance {
            //     origin_id: super_origin_id,
            //     args: super_args,
            //     ..
            // } |
            TypeDefinition::Function {
                origin_id: super_origin_id,
                args: super_args,
                ..
            },
            // TypeDefinition::TypeInstance {
            //     origin_id: sub_origin_id,
            //     args: sub_args,
            //     ..
            // } |
            TypeDefinition::Function {
                origin_id: sub_origin_id,
                args: sub_args,
                ..
            },
        ) = (super_type, sub_type)
            && discriminant(super_type) == discriminant(sub_type)
            && super_origin_id == sub_origin_id
            && (super_args.starts_with(sub_args)
                || (super_args.len() >= sub_args.len()
                    && super_args.iter().zip(sub_args.iter()).all(
                        |((super_type_id, _super_arg), (sub_type_id, _sub_arg))| {
                            visited.insert((*super_type_id, *sub_type_id));
                            let super_type = self.types.get(super_type_id).unwrap();
                            let sub_type = self.types.get(sub_type_id).unwrap();
                            let result =
                                self.check_supertype(super_type, sub_type, visited);
                            visited.remove(&(*super_type_id, *sub_type_id));
                            result
                        },
                    )))
        {
            return true;
        }

        // TODO: Eliminate the repetition
        if let (
            TypeDefinition::TypeInstance {
                origin_id: super_origin_id,
                args: super_args,
                ..
            },
            TypeDefinition::TypeInstance {
                origin_id: sub_origin_id,
                args: sub_args,
                ..
            }
                ) = (super_type, sub_type)
            && discriminant(super_type) == discriminant(sub_type)
            && super_origin_id == sub_origin_id
            && (super_args.starts_with(sub_args)
                || (super_args.len() >= sub_args.len()
                    && super_args.iter().zip(sub_args.iter()).all(
                        |(super_type_id, sub_type_id)| {
                            visited.insert((*super_type_id, *sub_type_id));
                            let super_type = self.types.get(super_type_id).unwrap();
                            let sub_type = self.types.get(sub_type_id).unwrap();
                            let result =
                                self.check_supertype(super_type, sub_type, visited);
                            visited.remove(&(*super_type_id, *sub_type_id));
                            result
                        },
                    )))
        {
            return true;
        }

        false
    }

    fn is_parent(&self, super_type_id: TypeId, sub_type_id: TypeId) -> bool {
        let ancestors = self.ancestors.get(&sub_type_id).expect("Type must exist!");

        !ancestors.is_empty() && (ancestors.contains(&super_type_id) || ancestors.iter().copied().any(|ancestor_id| self.is_parent(super_type_id, ancestor_id)))
    }

    fn install_embedded_types(mut self) -> Self {
        self.types.extend([
            (EMBEDDED_TYPES.object, TypeDefinition::Object),
            (
                EMBEDDED_TYPES.int8,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.int8,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Integer(IntegerNumber::Int8)),

                    size: TypeSize::Exact(1),
                },
            ),
            (
                EMBEDDED_TYPES.int16,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.int16,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Integer(IntegerNumber::Int16)),
                    size: TypeSize::Exact(2),
                },
            ),
            (
                EMBEDDED_TYPES.int32,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.int32,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Integer(IntegerNumber::Int32)),
                    size: TypeSize::Exact(4),
                },
            ),
            (
                EMBEDDED_TYPES.int64,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.int64,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Integer(IntegerNumber::Int64)),
                    size: TypeSize::Exact(8),
                },
            ),
            (
                EMBEDDED_TYPES.int128,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.int128,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Integer(
                        IntegerNumber::Int128,
                    )),
                    size: TypeSize::Exact(16),
                },
            ),
            (
                EMBEDDED_TYPES.intSize,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.intSize,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Integer(
                        IntegerNumber::IntSize,
                    )),
                    size: TypeSize::PointerSized,
                },
            ),
            (
                EMBEDDED_TYPES.uInt8,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.uInt8,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Integer(IntegerNumber::UInt8)),
                    size: TypeSize::Exact(1),
                },
            ),
            (
                EMBEDDED_TYPES.uInt16,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.uInt16,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Integer(
                        IntegerNumber::UInt16,
                    )),
                    size: TypeSize::Exact(2),
                },
            ),
            (
                EMBEDDED_TYPES.uInt32,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.uInt32,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Integer(
                        IntegerNumber::UInt32,
                    )),
                    size: TypeSize::Exact(4),
                },
            ),
            (
                EMBEDDED_TYPES.uInt64,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.uInt64,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Integer(
                        IntegerNumber::UInt64,
                    )),
                    size: TypeSize::Exact(8),
                },
            ),
            (
                EMBEDDED_TYPES.uInt128,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.uInt128,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Integer(
                        IntegerNumber::UInt128,
                    )),
                    size: TypeSize::Exact(16),
                },
            ),
            (
                EMBEDDED_TYPES.uIntSize,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.uIntSize,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Integer(
                        IntegerNumber::UIntSize,
                    )),
                    size: TypeSize::PointerSized,
                },
            ),
            (
                EMBEDDED_TYPES.float32,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.float32,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Floating(
                        FloatingNumber::Float32,
                    )),
                    size: TypeSize::Exact(4),
                },
            ),
            (
                EMBEDDED_TYPES.float64,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.float64,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Floating(
                        FloatingNumber::Float64,
                    )),
                    size: TypeSize::Exact(8),
                },
            ),
            (
                EMBEDDED_TYPES.boolean,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.boolean,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Boolean),
                    size: TypeSize::Exact(1),
                },
            ),
            (
                EMBEDDED_TYPES.char,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.char,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Char),
                    size: TypeSize::Exact(4),
                },
            ),
            (
                EMBEDDED_TYPES.void,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.void,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::Void),
                    size: TypeSize::Exact(0),
                },
            ),
            (
                EMBEDDED_TYPES.array,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.array,
                    name: EmbeddedTypeName::Compound(CompoundType::Array),
                    size: TypeSize::PointerSized,
                },
            ),
            (
                EMBEDDED_TYPES.linked_list,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.linked_list,
                    name: EmbeddedTypeName::Compound(CompoundType::LinkedList),
                    size: TypeSize::PointerSized,
                },
            ),
            (
                EMBEDDED_TYPES.string,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.string,
                    name: EmbeddedTypeName::Compound(CompoundType::String),
                    size: TypeSize::PointerSized,
                },
            ),
            (
                EMBEDDED_TYPES.function,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.function,
                    name: EmbeddedTypeName::Function,
                    size: TypeSize::ClosureSize,
                },
            ),
            (
                EMBEDDED_TYPES.literal_true,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.literal_true,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::LiteralTrue),
                    size: TypeSize::Exact(1),
                },
            ),
            (
                EMBEDDED_TYPES.literal_false,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.literal_false,
                    name: EmbeddedTypeName::Primitive(PrimitiveType::LiteralFalse),
                    size: TypeSize::Exact(1),
                },
            ),
        ]);

        self
    }

    fn install_custom_types(mut self) -> Self {
        for (_class_scope_id, class_scope) in self.scope_hierarchy.scopes.iter() {
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
                    binding_id: self
                        .scope_hierarchy
                        .class_to_binding_id
                        .get(&class.name)
                        .copied()
                        .unwrap(),
                    name: class.name.clone(),
                    size: TypeSize::UnionSize,
                    node: class,
                },
            );

            let mut variants = HashMap::new();
            for (constructor_name, patterns) in class.constructors.iter() {
                let variant_id = self.last_id;
                self.last_id.0 += 1;

                variants.insert(constructor_name.clone(), variant_id);

                self.types.insert(
                    variant_id,
                    TypeDefinition::Variant {
                        id: variant_id,
                        binding_id: class_scope
                            .bindings
                            .get(&constructor_name.into())
                            .copied()
                            .unwrap(),
                        name: constructor_name.clone(),
                        origin_id: class_type_id,
                        node: patterns,
                    },
                );
            }

            self.variants_of_origin.insert(class_type_id, variants);
        }

        self.build_ancestors_map();

        self
    }

    fn build_ancestors_map(&mut self) {
        for (_class_name, type_id) in self.name_to_origin_id.iter().skip(EMBEDDED_TYPES.count) {
            let Some(type_def @ TypeDefinition::Origin { node: class, .. }) =
                self.types.remove(type_id)
            else {
                unreachable!("Expected a class!")
            };

            self.ancestors.insert(
                *type_id,
                class
                    .ancestors
                    .iter()
                    .map(|ancestor_name| {
                        self.name_to_origin_id
                            .get(ancestor_name)
                            .copied()
                            .expect("Name does not exist!")
                    })
                    .collect(),
            );

            self.types.insert(*type_id, type_def);
        }
    }
}

mod embedded_types;

use crate::definition::*;
use crate::error::TypeError;
pub use embedded_types::*;
use scope::ExprId;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub struct Hierarchy<'a> {
    pub types: HashMap<TypeId, TypeDefinition<'a>>,
    last_id: TypeId,
    pub name_to_origin_id: HashMap<Rc<String>, TypeId>,
    pub variants_of_origin: HashMap<TypeId, HashMap<Rc<String>, TypeId>>,
    pub expr_to_type: ExprToTypeTable,
    pub binding_to_type: HashMap<scope::BindingId, TypeId>,
    pub expr_constraints: HashMap<scope::ExprId, HashSet<Constraint>>,
    pub embedded_types: &'static EmbeddedTypes,
    pub type_instances: HashMap<TypeId, Vec<TypeId>>,
    pub scope_hierarchy: scope::Hierarchy<'a>,
    pub responds_to_table: HashMap<Rc<String>, TypeId>,
    pub call_table: HashMap<scope::ExprId, (scope::BindingId, TypeId)>,
}

#[derive(Default)]
pub struct ExprToTypeTable {
    pub ok: HashMap<ExprId, TypeId>,
    pub err: HashMap<ExprId, TypeError>,
    pub deferred: HashSet<ExprId>,
}

impl<'a> Hierarchy<'a> {
    pub(crate) fn new(scope_hierarchy: scope::Hierarchy<'a>) -> Self {
        Self {
            types: HashMap::new(),
            last_id: TypeId(24),
            name_to_origin_id: HashMap::new(),
            variants_of_origin: HashMap::new(),
            expr_to_type: ExprToTypeTable::default(),
            expr_constraints: HashMap::new(),
            embedded_types: &EMBEDDED_TYPES,
            type_instances: HashMap::new(),
            scope_hierarchy,
            responds_to_table: HashMap::new(),
            binding_to_type: HashMap::new(),
            call_table: HashMap::new(),
        }
        .install_embedded_types()
        .install_custom_types()
    }

    pub(crate) fn next_id(&mut self) -> TypeId {
        let result = self.last_id;
        self.last_id = TypeId(self.last_id.0 + 1);
        result
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
                EMBEDDED_TYPES.literal_integer,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.literal_integer,
                    name: EmbeddedTypeName::Function,
                    size: TypeSize::Exact(4),
                },
            ),
            (
                EMBEDDED_TYPES.literal_integer,
                TypeDefinition::EmbeddedType {
                    id: EMBEDDED_TYPES.literal_float,
                    name: EmbeddedTypeName::Function,
                    size: TypeSize::Exact(8),
                },
            ),
        ]);

        self
    }

    fn install_custom_types(mut self) -> Self {
        let scopes_buffer: HashMap<_, _> = self
            .scope_hierarchy
            .scopes
            .iter()
            .map(|(_class_scope_id, class_scope)| {
                let scope::SyntaxNode::Class(syntax::Class { name, .. }) = class_scope.node else {
                    panic!("Class expected for root scope");
                };

                (name.clone(), class_scope)
            })
            .collect();

        for (_class_scope_id, class_scope) in self.scope_hierarchy.scopes.iter() {
            let class_type_id = self.last_id;
            self.last_id.0 += 1;

            let scope::SyntaxNode::Class(class) = class_scope.node else {
                panic!("Class expected for root scope");
            };

            self.name_to_origin_id
                .insert(class.name.clone(), class_type_id);

            let constructors = self.constructor_bindings(class, &scopes_buffer);
            let static_methods = self.static_methods(class, &scopes_buffer);

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
                    parent_ids: HashMap::new(), // This will handled by inheritence analysis
                    size: TypeSize::UnionSize,
                    node: Some(class),
                    constructors,
                    static_methods,
                },
            );

            let mut variants = HashMap::new();
            for (constructor_name, patterns) in class.constructors.iter() {
                let variant_id = self.last_id;
                self.last_id.0 += 1;

                variants.insert(constructor_name.clone(), variant_id);

                let instance_methods =
                    self.instance_methods(constructor_name.as_str(), class_scope);

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
                        instance_methods,
                    },
                );
            }

            self.variants_of_origin.insert(class_type_id, variants);
        }

        self
    }

    fn constructor_bindings(
        &self,
        class: &syntax::Class,
        scopes_buffer: &HashMap<Rc<String>, &scope::Scope<'_>>,
    ) -> Vec<(Rc<String>, scope::BindingId)> {
        let mut constructors = self.inherited_constructors(class, scopes_buffer);

        let class_scope = *scopes_buffer.get(&class.name).unwrap();

        constructors.extend(
            class
                .constructors
                .iter()
                .map(|(constructor_name, _patterns)| {
                    let (_name, binding_id) = class_scope
                        .bindings
                        .iter()
                        .find(|(binding_name, _binding_id)| {
                            binding_name.as_ref() == constructor_name.as_ref()
                        })
                        .unwrap();

                    (class.name.clone(), *binding_id)
                }),
        );
        constructors
    }

    fn inherited_constructors(
        &self,
        class: &syntax::Class,
        scopes_buffer: &HashMap<Rc<String>, &scope::Scope<'_>>,
    ) -> Vec<(Rc<String>, scope::BindingId)> {
        class
            .ancestors
            .iter()
            .flat_map(|ancestor_name| {
                let ancestor_class_scope = scopes_buffer.get(ancestor_name).unwrap();

                let scope::SyntaxNode::Class(ancestor_class) = ancestor_class_scope.node else {
                    unreachable!()
                };

                ancestor_class
                    .constructors
                    .iter()
                    .filter_map(|(constructor_name, _parameters)| {
                        if let Some((_name, binding_id)) = ancestor_class_scope
                            .bindings
                            .iter()
                            .find(|(binding_name, _binding_id)| {
                                binding_name.as_ref() == constructor_name.as_ref()
                            })
                        {
                            Some((ancestor_class.name.clone(), *binding_id))
                        } else {
                            None
                        }
                    })
            })
            .collect()
    }

    fn static_methods(
        &self,
        class: &syntax::Class,
        scopes_buffer: &HashMap<Rc<String>, &scope::Scope<'_>>,
    ) -> Vec<(Rc<String>, scope::BindingId)> {
        let mut methods = self.inherited_static_methods(class, scopes_buffer);

        let class_scope = scopes_buffer.get(&class.name).unwrap();

        methods.extend(
            class_scope
                .bindings
                .iter()
                .filter_map(|(pattern_name, binding_id)| {
                    let syntax::PatternName::Name(method_name) = pattern_name else {
                        unreachable!()
                    };

                    class.methods.get(method_name)?.iter().find(|method| {
                        method
                            .params
                            .first()
                            .map(|param| param.name != syntax::PatternName::ClassSelf)
                            .unwrap_or(true)
                    })?;

                    Some((class.name.clone(), *binding_id))
                }),
        );

        methods
    }

    fn inherited_static_methods(
        &self,
        class: &syntax::Class,
        scopes_buffer: &HashMap<Rc<String>, &scope::Scope<'_>>,
    ) -> Vec<(Rc<String>, scope::BindingId)> {
        class
            .ancestors
            .iter()
            .flat_map(|ancestor_name| {
                let ancestor_class_scope = scopes_buffer.get(ancestor_name).unwrap();

                let scope::SyntaxNode::Class(ancestor_class) = &ancestor_class_scope.node else {
                    unreachable!()
                };

                ancestor_class_scope
                    .bindings
                    .iter()
                    .filter_map(|(pattern_name, binding_id)| {
                        let syntax::PatternName::Name(method_name) = pattern_name else {
                            unreachable!()
                        };

                        ancestor_class
                            .methods
                            .get(method_name)?
                            .iter()
                            .find(|method| {
                                method
                                    .params
                                    .first()
                                    .map(|param| param.name != syntax::PatternName::ClassSelf)
                                    .unwrap_or(true)
                            })?;

                        Some((ancestor_name.clone(), *binding_id))
                    })
            })
            .collect()
    }

    fn instance_methods(
        &self,
        constructor_name: &str,
        class_scope: &scope::Scope<'_>,
    ) -> Vec<(Rc<String>, scope::BindingId)> {
        unimplemented!()
    }

    fn get_class_scope_id(&self, class_name: &Rc<String>) -> scope::ScopeId {
        self.scope_hierarchy
            .class_to_scope_id
            .get(class_name)
            .copied()
            .unwrap()
    }
}

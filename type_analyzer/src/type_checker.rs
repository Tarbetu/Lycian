use crate::definition::TypeDefinition;
use crate::error::{TypeError, TypeErrorKind, TypeResult};
use crate::hierarchy::EMBEDDED_TYPES;
use crate::{definition::Constraint, hierarchy::Hierarchy, TypeId};
use scope::SyntaxNode;
use std::collections::HashSet;

pub(crate) struct TypeChecker<'a> {
    hierarchy: Hierarchy<'a>,
    errors: Vec<TypeError>,
}

impl<'a> TypeChecker<'a> {
    pub fn traverse(&mut self) -> TypeResult<()> {
        self.traverse_classes(0)
    }

    fn traverse_classes(&mut self, class_scope_index: usize) -> TypeResult<()> {
        let Some(class_scope_id) = self
            .hierarchy
            .scope_hierarchy
            .root()
            .children_ids
            .get(class_scope_index)
        else {
            return Ok(());
        };

        let scope::Scope {
            node: SyntaxNode::Class(class),
            ..
        } = self
            .hierarchy
            .scope_hierarchy
            .scopes
            .get(class_scope_id)
            .unwrap()
        else {
            unreachable!()
        };

        self.traverse_class(class)?;

        self.traverse_classes(class_scope_index + 1)
    }

    fn traverse_class(&mut self, class: &'a syntax::Class) -> TypeResult<()> {
        for (_method_name, method_overloads) in &class.methods {
            self.traverse_method(method_overloads)?;
        }

        Ok(())
    }

    fn traverse_method(&mut self, method_overloads: &'a [syntax::Function]) -> TypeResult<()> {
        // All overloads has to share same inheritance line, so we have to check all of them
        let mut types = HashSet::new();

        for method in method_overloads {
            let type_id = self.return_type(
                method
                    .return_type
                    .as_ref()
                    .expect("Every method definition required to include a return type"),
            )?;
            types.insert(type_id);

            if let Err(err) = self.check(&method.body, type_id) {
                self.errors.push(err);
            }
        }

        // Check the inheritance line later

        Ok(())
    }

    fn synthesize(&mut self, expr: &syntax::Expression) -> TypeResult<TypeId> {
        unimplemented!()
    }

    fn check(&mut self, expr: &syntax::Expression, expected_type: TypeId) -> TypeResult<TypeId> {
        unimplemented!()
    }

    fn return_type(&mut self, expr: &'a syntax::Expression) -> TypeResult<TypeId> {
        use syntax::ExpressionKind::*;

        match expr.kind.as_ref() {
            Call {
                caller: class_name,
                callee: None,
                ..
            } => {
                // ClassName
                let Some(origin_type_id) = self
                    .hierarchy
                    .name_to_origin_id
                    .get(class_name.as_ref())
                    .copied()
                else {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidType,
                        message: "Type not found",
                        type_id: TypeId(0),
                        span: expr.span.clone(),
                    });
                };

                Ok(origin_type_id)
            }
            Call {
                caller: variant_name,
                callee: Some(class_name),
                ..
            } => {
                // OriginName.VariantName
                let Call {
                    caller: origin_type,
                    callee: None,
                    ..
                } = class_name.kind.as_ref()
                else {
                    return Err(
                        TypeError {
                            kind: TypeErrorKind::InvalidType,
                            message: "Invalid return type expression: Expected `ClassName.VariantName` or `ClassName`",
                            type_id: TypeId(0),
                            span: expr.span.clone()
                        }
                    );
                };

                let Some(origin_type_id) =
                    self.hierarchy.name_to_origin_id.get(origin_type.as_ref())
                else {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidType,
                        message: "Class name not found",
                        type_id: TypeId(0),
                        span: expr.span.clone(),
                    });
                };

                let Some(variant_id) = self
                    .hierarchy
                    .variants_of_origin
                    .get(origin_type_id)
                    .unwrap()
                    .get(variant_name.as_ref())
                    .copied()
                else {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidType,
                        message: "Variant not found",
                        type_id: TypeId(0),
                        span: expr.span.clone(),
                    });
                };

                Ok(variant_id)
            }
            Literal(literal) => Ok(self.literal_type(literal)),
            Grouping(inner_expr) => self.return_type(&inner_expr),
            _ => {
                return Err(TypeError {
                    kind: TypeErrorKind::InvalidType,
                    message: "Invalid return type",
                    type_id: TypeId(0),
                    span: expr.span.clone(),
                })
            }
        }
    }

    fn literal_type(&mut self, literal: &'a syntax::Literal) -> TypeId {
        use syntax::Literal::*;

        match literal {
            Integer(_) => match self.hierarchy.literal_types.integers.get(literal).copied() {
                Some(type_id) => type_id,
                None => {
                    let type_id = self.hierarchy.next_id();
                    let type_def = TypeDefinition::Literal {
                        id: type_id,
                        origin_id: EMBEDDED_TYPES.literal_integer,
                        node: literal,
                    };
                    self.hierarchy.types.insert(type_id, type_def);
                    type_id
                }
            },
            Float(_) => match self.hierarchy.literal_types.floats.get(literal).copied() {
                Some(type_id) => type_id,
                None => {
                    let type_id = self.hierarchy.next_id();
                    let type_def = TypeDefinition::Literal {
                        id: type_id,
                        origin_id: EMBEDDED_TYPES.literal_float,
                        node: literal,
                    };
                    self.hierarchy.types.insert(type_id, type_def);
                    type_id
                }
            },
            Boolean(true) => EMBEDDED_TYPES.literal_true,
            Boolean(false) => EMBEDDED_TYPES.literal_false,
            Char(_) => match self.hierarchy.literal_types.chars.get(literal).copied() {
                Some(type_id) => type_id,
                None => {
                    let type_id = self.hierarchy.next_id();
                    let type_def = TypeDefinition::Literal {
                        id: type_id,
                        origin_id: EMBEDDED_TYPES.char,
                        node: literal,
                    };
                    self.hierarchy.types.insert(type_id, type_def);
                    type_id
                }
            },
            Str(_) => match self.hierarchy.literal_types.strings.get(literal).copied() {
                Some(type_id) => type_id,
                None => {
                    let type_id = self.hierarchy.next_id();
                    let type_def = TypeDefinition::Literal {
                        id: type_id,
                        origin_id: EMBEDDED_TYPES.string,
                        node: literal,
                    };
                    self.hierarchy.types.insert(type_id, type_def);
                    type_id
                }
            },
            LiteralList(_) => match self.hierarchy.literal_types.lists.get(literal).copied() {
                Some(type_id) => type_id,
                None => {
                    let type_id = self.hierarchy.next_id();
                    let type_def = TypeDefinition::Literal {
                        id: type_id,
                        origin_id: EMBEDDED_TYPES.linked_list,
                        node: literal,
                    };
                    self.hierarchy.types.insert(type_id, type_def);
                    type_id
                }
            },
            LiteralArray(_) => match self.hierarchy.literal_types.arrays.get(literal).copied() {
                Some(type_id) => type_id,
                None => {
                    let type_id = self.hierarchy.next_id();
                    let type_def = TypeDefinition::Literal {
                        id: type_id,
                        origin_id: EMBEDDED_TYPES.array,
                        node: literal,
                    };
                    self.hierarchy.types.insert(type_id, type_def);
                    type_id
                }
            },
        }
    }
}

use crate::definition::TypeDefinition;
use crate::error::{TypeError, TypeErrorKind, TypeResult};
use crate::hierarchy::EMBEDDED_TYPES;
use crate::{hierarchy::Hierarchy, TypeId};
use scope::SyntaxNode;
use std::collections::HashSet;

pub(crate) struct TypeChecker<'a> {
    pub hierarchy: Hierarchy<'a>,
    pub errors: Vec<TypeError>,
}

enum ExpectedType<'a> {
    // Requires just the type
    Exactly(TypeId),

    // One of the types (F.g: Addition operation accepts a string or number)
    OneOf(&'a [TypeId]),

    // Origin or it's another variant
    OriginWithVariants(TypeId),

    // Subtype or supertype
    KindOf(TypeId),

    // Numeric as -expr
    Numeric,

    // Leave it to the synthesis
    JustInfer,
}

impl<'a> TypeChecker<'a> {
    pub fn new(hierarchy: Hierarchy<'a>) -> Self {
        Self {
            hierarchy,
            errors: Vec::new(),
        }
    }

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
            node: SyntaxNode::Class(_),
            children_ids: children_ids,
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

        for scope_id in children_ids.clone().iter().copied() {
            self.traverse_class(scope_id)?;
        }

        self.traverse_classes(class_scope_index + 1)
    }

    fn traverse_class(&mut self, class_scope_id: scope::ScopeId) -> TypeResult<()> {
        unimplemented!()
        // for (method_name, method_overloads) in &class.methods {
        //     self.traverse_method(method_name, method_overloads)?;
        // }

        // Ok(())
    }

    fn traverse_method(
        &mut self,
        method_name: &str,
        method_overloads: &'a [syntax::Function],
    ) -> TypeResult<()> {
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

            if let Err(err) = self.check(&method.body, ExpectedType::Exactly(type_id)) {
                self.errors.push(err);
            }
        }

        // Check the inheritance line here

        Ok(())
    }

    fn synthesize(&mut self, expr: &'a syntax::Expression) -> TypeResult<TypeId> {
        use syntax::ExpressionKind::*;
        use ExpectedType::*;

        match expr.kind.as_ref() {
            Grouping(inner_expr) => self.synthesize(inner_expr),
            Literal(literal) => {
                let literal_type_id = self.literal_type(literal)?;
                self.declare_expr_type(expr.id, literal_type_id);
                Ok(literal_type_id)
            }
            Binary(lhs, op, rhs) if op.is_logical() => {
                self.check(lhs, Exactly(EMBEDDED_TYPES.boolean))?;
                self.check(rhs, Exactly(EMBEDDED_TYPES.boolean))?;
                self.declare_expr_type(expr.id, EMBEDDED_TYPES.boolean);
                Ok(EMBEDDED_TYPES.boolean)
            }
            Binary(lhs, op, rhs) if op.is_comparison() => {
                let lhs_type = self.synthesize(lhs)?;
                self.check(rhs, Exactly(lhs_type));
                self.declare_expr_type(expr.id, EMBEDDED_TYPES.boolean);
                Ok(EMBEDDED_TYPES.boolean)
            }
            Binary(lhs, op, rhs) if op.is_arithmetic() => {
                let lhs_type = self.synthesize(lhs)?;
                self.check(rhs, Exactly(lhs_type));
                self.declare_expr_type(expr.id, lhs_type);
                Ok(lhs_type)
            }
            Binary(..) => unreachable!(),
            Unary(syntax::Operator::Not, inner_expr) => {
                self.check(inner_expr, Exactly(EMBEDDED_TYPES.boolean))?;
                self.declare_expr_type(expr.id, EMBEDDED_TYPES.boolean);
                Ok(EMBEDDED_TYPES.boolean)
            }
            Unary(syntax::Operator::Negate, inner_expr) => {
                let expr_type = self.check(inner_expr, Numeric)?;
                self.declare_expr_type(expr.id, expr_type);
                Ok(expr_type)
            }
            IndexOperator(container, indexer) => {
                let container_type = self.synthesize(container)?;
                // Indexer always assumed as uIntSize!
                self.check(
                    indexer,
                    OneOf(&[EMBEDDED_TYPES.literal_integer, EMBEDDED_TYPES.uIntSize]),
                )?;

                // let element_type = match container.kind.as_ref() {};

                unimplemented!()
            }
            Call {
                caller,
                callee: None,
                ..
            } => {
                // Get binding and resolve it
                unimplemented!()
            }
            Call {
                caller,
                callee: Some(receiver),
                ..
            } => {
                // Check method call
                let receiver_ty = self.synthesize(receiver)?;
                unimplemented!()
            }
            _ => unimplemented!(),
        }
    }

    fn check(
        &mut self,
        expr: &syntax::Expression,
        expected_type: ExpectedType,
    ) -> TypeResult<TypeId> {
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
            Literal(literal) => Ok(self.literal_type(literal))?,
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

    fn literal_type(&mut self, literal: &'a syntax::Literal) -> TypeResult<TypeId> {
        use syntax::Literal::*;
        use ExpectedType::{Exactly, KindOf};

        match literal {
            Integer(_) => match self.hierarchy.literal_types.integers.get(literal).copied() {
                Some(type_id) => Ok(type_id),
                None => {
                    let type_id = self.hierarchy.next_id();
                    let type_def = TypeDefinition::Literal {
                        id: type_id,
                        origin_id: EMBEDDED_TYPES.literal_integer,
                        node: literal,
                    };
                    self.hierarchy.types.insert(type_id, type_def);
                    Ok(type_id)
                }
            },
            Float(_) => match self.hierarchy.literal_types.floats.get(literal).copied() {
                Some(type_id) => Ok(type_id),
                None => {
                    let type_id = self.hierarchy.next_id();
                    let type_def = TypeDefinition::Literal {
                        id: type_id,
                        origin_id: EMBEDDED_TYPES.literal_float,
                        node: literal,
                    };
                    self.hierarchy.types.insert(type_id, type_def);
                    Ok(type_id)
                }
            },
            Boolean(true) => Ok(EMBEDDED_TYPES.literal_true),
            Boolean(false) => Ok(EMBEDDED_TYPES.literal_false),
            Char(_) => match self.hierarchy.literal_types.chars.get(literal).copied() {
                Some(type_id) => Ok(type_id),
                None => {
                    let type_id = self.hierarchy.next_id();
                    let type_def = TypeDefinition::Literal {
                        id: type_id,
                        origin_id: EMBEDDED_TYPES.char,
                        node: literal,
                    };
                    self.hierarchy.types.insert(type_id, type_def);
                    Ok(type_id)
                }
            },
            Str(_) => match self.hierarchy.literal_types.strings.get(literal).copied() {
                Some(type_id) => Ok(type_id),
                None => {
                    let type_id = self.hierarchy.next_id();
                    let type_def = TypeDefinition::Literal {
                        id: type_id,
                        origin_id: EMBEDDED_TYPES.string,
                        node: literal,
                    };
                    self.hierarchy.types.insert(type_id, type_def);
                    Ok(type_id)
                }
            },
            LiteralList(expr_list) => {
                if let Some(expr) = expr_list.front() {
                    let type_expr_id = self.synthesize(expr)?;

                    for expr in expr_list.iter().skip(1) {
                        self.check(expr, KindOf(type_expr_id))?;
                    }

                    Ok(self
                        .hierarchy
                        .get_type_instance(EMBEDDED_TYPES.linked_list, &vec![type_expr_id]))
                } else {
                    Ok(self.hierarchy.get_type_instance(
                        EMBEDDED_TYPES.linked_list,
                        &vec![EMBEDDED_TYPES.literal_empty_list],
                    ))
                }
            }
            LiteralArray(expr_array) => {
                if let Some(expr) = expr_array.get(0) {
                    let type_expr_id = self.synthesize(expr)?;

                    for expr in expr_array.iter().skip(1) {
                        self.check(expr, Exactly(type_expr_id))?;
                    }

                    Ok(self
                        .hierarchy
                        .get_type_instance(EMBEDDED_TYPES.array, &vec![type_expr_id]))
                } else {
                    Ok(self.hierarchy.get_type_instance(
                        EMBEDDED_TYPES.array,
                        &vec![EMBEDDED_TYPES.literal_empty_array],
                    ))
                }
            }
        }
    }

    fn declare_expr_type(&mut self, expr_id: usize, type_id: TypeId) {
        let expr_id = scope::ExprId(expr_id);

        // self.hierarchy.expr_to_type.deferred.remove(&expr_id);
        // self.hierarchy.expr_to_type.ok.insert(expr_id, type_id);
        unimplemented!()
    }
}

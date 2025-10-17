use crate::definition::TypeDefinition;
use crate::error::{TypeError, TypeErrorKind, TypeResult};
use crate::hierarchy::EMBEDDED_TYPES;
use crate::{definition::Constraint, hierarchy::Hierarchy, TypeId};
use std::rc::Rc;

pub(crate) struct ConstraintDetective<'a> {
    hierarchy: Hierarchy<'a>,
    errors: Vec<TypeError>,
}

impl<'a> ConstraintDetective<'a> {
    pub(crate) fn new(hierarchy: Hierarchy<'a>) -> Self {
        Self {
            hierarchy,
            errors: vec![],
        }
    }

    pub(crate) fn investigate_constraints(&mut self) -> TypeResult<()> {
        self.do_investigate_constraints(0)
    }

    fn do_investigate_constraints(&mut self, index: usize) -> TypeResult<()> {
        let class_scope_id = self
            .hierarchy
            .scope_hierarchy
            .root()
            .children_ids
            .get(index);

        if let Some(class_scope_id) = class_scope_id {
            let scope::Scope {
                node: scope::SyntaxNode::Class(class),
                ..
            } = self
                .hierarchy
                .scope_hierarchy
                .scopes
                .get(class_scope_id)
                .unwrap()
            else {
                unreachable!("Expected class scope")
            };

            self.investigate_class_constraints(class)?;

            self.do_investigate_constraints(index + 1)
        } else {
            Ok(())
        }
    }

    fn investigate_class_constraints(&mut self, class: &'a syntax::Class) -> TypeResult<()> {
        for (_method_name, method_overloads) in &class.methods {
            self.investigate_method_constraints(method_overloads)?
        }

        Ok(())
    }

    fn investigate_method_constraints(
        &mut self,
        method_overloads: &'a [syntax::Function],
    ) -> TypeResult<()> {
        let mut types = vec![];

        for method in method_overloads {
            if let Some(type_id) =
                self.check_return_type(method.body.id, method.return_type.as_ref(), true)?
            {
                types.push(type_id)
            }

            if let Err(err) = self.investigate_expression(&method.body) {
                self.errors.push(err)
            }
        }

        Ok(())
    }

    fn investigate_expression(&mut self, expr: &'a syntax::Expression) -> TypeResult<()> {
        use syntax::ExpressionKind::*;

        let promised_type_id = self
            .hierarchy
            .expr_to_type
            .ok
            .get(&scope::ExprId(expr.id))
            .copied();

        match (expr.kind.as_ref(), promised_type_id) {
            (Grouping(expr), Some(promised_type_id)) => {
                self.declare_expr_type(expr.id, promised_type_id);
                self.investigate_expression(expr)
            }
            (Grouping(expr), None) => self.investigate_expression(expr),
            (Literal(literal), Some(promised_type_id)) => {
                let promised_type = self
                    .hierarchy
                    .types
                    .get(&promised_type_id)
                    .expect("Promised type not found");

                match promised_type {
                    TypeDefinition::Literal { id, node, .. } if *node == literal.as_ref() => {
                        self.hierarchy
                            .expr_to_type
                            .ok
                            .insert(scope::ExprId(expr.id), *id);
                        Ok(())
                    }
                    TypeDefinition::Literal { id, .. } => Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Literal type does not match with literal return type",
                        type_id: *id,
                        span: expr.span.clone(),
                    }),
                    _ => {
                        // I need a TypeId,
                        // and I hope you won't create more types than the numbers of salt grains of the world.
                        let literal_type =
                            self.literal_type_with_custom_id(literal, TypeId(usize::MAX));

                        if TypeDefinition::is_supertype(
                            promised_type,
                            &literal_type,
                            &self.hierarchy,
                        ) {
                            Ok(())
                        } else {
                            Err(TypeError {
                                kind: TypeErrorKind::TypeMismatch,
                                message: "Literal type does not match with return type",
                                type_id: promised_type_id,
                                span: expr.span.clone(),
                            })
                        }
                    }
                }
            }
            (Literal(literal), None) => {
                let literal_type = self.literal_type(literal.as_ref());

                self.add_type_definition(expr.id, literal_type);
                Ok(())
            }
            (Binary(_lhs, op, _rhs), Some(promised_type_id))
                if op.is_logical() && promised_type_id != EMBEDDED_TYPES.boolean =>
            {
                Err(TypeError {
                    kind: TypeErrorKind::TypeMismatch,
                    message: "Logical operations must return a boolean",
                    type_id: promised_type_id,
                    span: expr.span.clone(),
                })
            }
            (Binary(_lhs, syntax::Operator::Add, _rhs), Some(promised_type_id))
                if !(EMBEDDED_TYPES.is_number(promised_type_id)
                    || promised_type_id == EMBEDDED_TYPES.string) =>
            {
                Err(TypeError {
                    kind: TypeErrorKind::TypeMismatch,
                    message: "Addition operation must return a number or a string",
                    type_id: promised_type_id,
                    span: expr.span.clone(),
                })
            }
            (Binary(_lhs, op, _rhs), Some(promised_type_id))
                if !(*op == syntax::Operator::Equal || *op == syntax::Operator::NotEqual)
                    && (op.is_arithmetic() || op.is_comparison())
                    && !EMBEDDED_TYPES.is_number(promised_type_id) =>
            {
                Err(TypeError {
                    kind: TypeErrorKind::TypeMismatch,
                    message: "Arithmetic and comparison operations must return a number",
                    type_id: promised_type_id,
                    span: expr.span.clone(),
                })
            }
            (Binary(lhs, op, rhs), _) if op.is_logical() => {
                self.declare_expr_type(expr.id, EMBEDDED_TYPES.boolean);

                self.declare_expr_type(lhs.id, EMBEDDED_TYPES.boolean);
                self.declare_expr_type(rhs.id, EMBEDDED_TYPES.boolean);

                self.investigate_expression(lhs)?;
                self.investigate_expression(rhs)
            }
            (Binary(lhs, syntax::Operator::Add, rhs), _) => {
                use Constraint::*;

                self.declare_expr_constraints(lhs.id, &[Addable, SameAs(scope::ExprId(rhs.id))]);
                self.declare_expr_constraints(rhs.id, &[Addable, SameAs(scope::ExprId(lhs.id))]);

                self.investigate_expression(lhs)?;
                self.investigate_expression(rhs)
            }
            (Binary(lhs, syntax::Operator::Equal | syntax::Operator::NotEqual, rhs), _) => {
                use Constraint::SameAs;
                self.declare_expr_type(expr.id, EMBEDDED_TYPES.boolean);

                self.declare_expr_constraints(lhs.id, &[SameAs(scope::ExprId(rhs.id))]);
                self.declare_expr_constraints(rhs.id, &[SameAs(scope::ExprId(lhs.id))]);

                self.investigate_expression(lhs)?;
                self.investigate_expression(rhs)
            }
            (Binary(lhs, _, rhs), _) => {
                use Constraint::{Numeric, SameAs};

                self.declare_expr_type(expr.id, EMBEDDED_TYPES.boolean);

                // Note down that operants has same constraints
                self.declare_expr_constraints(lhs.id, &[Numeric, SameAs(scope::ExprId(rhs.id))]);
                self.declare_expr_constraints(rhs.id, &[Numeric, SameAs(scope::ExprId(lhs.id))]);

                self.investigate_expression(lhs)?;
                self.investigate_expression(rhs)
            }
            (Unary(syntax::Operator::Not, expr), Some(promised_type_id))
                if promised_type_id != EMBEDDED_TYPES.boolean =>
            {
                Err(TypeError {
                    kind: TypeErrorKind::TypeMismatch,
                    message: "Logical NOT operation must return a boolean",
                    type_id: promised_type_id,
                    span: expr.span.clone(),
                })
            }
            (Unary(syntax::Operator::Not, expr), _) => {
                self.declare_expr_type(expr.id, EMBEDDED_TYPES.boolean);

                self.investigate_expression(expr)
            }
            (Unary(syntax::Operator::Negate, expr), Some(promised_type_id))
                if !EMBEDDED_TYPES.is_number(promised_type_id) =>
            {
                Err(TypeError {
                    kind: TypeErrorKind::TypeMismatch,
                    message: "Numeric negation operation must return a number",
                    type_id: promised_type_id,
                    span: expr.span.clone(),
                })
            }
            (Unary(syntax::Operator::Negate, expr), _) => {
                self.declare_expr_constraints(expr.id, &[Constraint::Numeric]);

                self.investigate_expression(expr)
            }
            (Unary(..), _) => unreachable!(),
            (IndexOperator(container, index_expr), _) => {
                self.declare_expr_constraints(container.id, &[Constraint::Indexable]);

                self.investigate_expression(container)?;
                self.investigate_expression(index_expr)
            }
            (Function(_), Some(promised_type_id))
                if promised_type_id != EMBEDDED_TYPES.function =>
            {
                Err(TypeError {
                    kind: TypeErrorKind::TypeMismatch,
                    message: "Function expressions must return a function type",
                    type_id: promised_type_id,
                    span: expr.span.clone(),
                })
            }
            (
                Function(syntax::Function {
                    body, return_type, ..
                }),
                _,
            ) => {
                self.check_return_type(body.id, return_type.as_ref(), false)?;
                self.investigate_expression(body)
            }
            (
                Block {
                    expressions, value, ..
                },
                _,
            ) => {
                self.investigate_expression(value)?;

                for expression in expressions {
                    self.investigate_expression(expression)?;
                }

                Ok(())
            }
            (
                Call {
                    caller,
                    block,
                    callee: None,
                    ..
                },
                _,
            ) => {
                if let Some(block) = block {
                    self.investigate_expression(block)?;
                }

                let scope_id = self
                    .hierarchy
                    .scope_hierarchy
                    .expr_to_scope_id
                    .get(&scope::ExprId(expr.id))
                    .unwrap();

                let (binding_id, _status) = self
                    .hierarchy
                    .scope_hierarchy
                    .scopes
                    .get(scope_id)
                    .unwrap()
                    .resolved_references
                    .get(&caller.into())
                    .unwrap();

                let Some((binding_kind, binding_node)) = self
                    .hierarchy
                    .scope_hierarchy
                    .bindings
                    .get(binding_id)
                    .map(|binding| (binding.kind, binding.node))
                else {
                    unreachable!("Binding scope not found")
                };

                use scope::BindingKind::*;
                match binding_kind {
                    Class => {
                        let origin_id = self
                            .hierarchy
                            .name_to_origin_id
                            .get(caller)
                            .copied()
                            .unwrap();

                        self.hierarchy
                            .expr_to_type
                            .ok
                            .insert(scope::ExprId(expr.id), origin_id);

                        Ok(())
                    }
                    Constructor => {
                        let scope::SyntaxNode::Constructor(class, constructor) = binding_node
                        else {
                            unreachable!()
                        };

                        let origin_id = self
                            .hierarchy
                            .name_to_origin_id
                            .get(&class.name)
                            .copied()
                            .unwrap();

                        let variant_id = self
                            .hierarchy
                            .variants_of_origin
                            .get(&origin_id)
                            .unwrap()
                            .get(&constructor.0)
                            .copied()
                            .unwrap();

                        self.hierarchy
                            .expr_to_type
                            .ok
                            .insert(scope::ExprId(expr.id), variant_id);

                        Ok(())
                    }
                    Method | LocalFunction => {
                        self.declare_expr_constraints(
                            expr.id,
                            &[Constraint::ResultOf(*binding_id)],
                        );

                        use scope::SyntaxNode::{Function, Method};
                        match binding_node {
                            Method(methods) => {
                                for expr_id in methods
                                    .iter()
                                    .filter_map(|method| method.return_type.as_ref())
                                    .map(|expr| expr.id)
                                {
                                    self.declare_expr_constraints(expr_id, &[Constraint::TypeRef]);
                                }
                                Ok(())
                            }
                            Function(syntax::Function {
                                return_type: Some(expr),
                                ..
                            }) => {
                                self.declare_expr_constraints(expr.id, &[Constraint::TypeRef]);
                                Ok(())
                            }
                            Function(_) => Ok(()),
                            _ => unreachable!(),
                        }
                    }
                    Argument => {
                        let scope::SyntaxNode::Pattern(syntax::Pattern {
                            name: _,
                            value,
                            condition,
                        }) = binding_node
                        else {
                            unreachable!()
                        };

                        if let Some(condition) = condition {
                            self.investigate_expression(condition)?;
                        }
                        self.declare_expr_constraints(value.id, &[Constraint::TypeRef]);
                        Ok(())
                    }
                }
            }
            (
                Call {
                    caller,
                    block,
                    callee: Some(callee_expr),
                    ..
                },
                _,
            ) => {
                if let Some(block) = block {
                    self.investigate_expression(block)?
                }

                self.declare_expr_constraints(
                    callee_expr.id,
                    &[Constraint::RespondsTo(caller.into())],
                );
                self.investigate_expression(callee_expr)?;

                Ok(())
            }
            (Match { scrutinee, arms }, _) => {
                let mut arm_type_exprs = vec![];

                for (
                    syntax::Pattern {
                        value: type_value,
                        condition,
                        ..
                    },
                    value_expr,
                ) in arms
                {
                    self.declare_expr_constraints(type_value.id, &[Constraint::TypeRef]);
                    arm_type_exprs.push(scope::ExprId(type_value.id));

                    if let Some(condition) = condition {
                        self.investigate_expression(condition)?;
                    }

                    self.investigate_expression(value_expr)?
                }

                self.declare_expr_constraints(
                    scrutinee.id,
                    &[Constraint::AnyOfTypeRefs(Rc::new(arm_type_exprs))],
                );

                Ok(())
            }
            (ClassSelf, _) => {
                let scope_id = self
                    .hierarchy
                    .scope_hierarchy
                    .expr_to_scope_id
                    .get(&scope::ExprId(expr.id))
                    .copied()
                    .unwrap();

                let (binding_id, _status) = self
                    .hierarchy
                    .scope_hierarchy
                    .scopes
                    .get(&scope_id)
                    .unwrap()
                    .resolved_references
                    .get(&syntax::PatternName::ClassSelf)
                    .unwrap();

                let Some(binding_node) = self
                    .hierarchy
                    .scope_hierarchy
                    .bindings
                    .get(binding_id)
                    .map(|binding| binding.node)
                else {
                    unreachable!()
                };

                let scope::SyntaxNode::Pattern(syntax::Pattern {
                    value, condition, ..
                }) = binding_node
                else {
                    unreachable!()
                };

                if let Some(condition) = condition {
                    self.investigate_expression(condition)?;
                }

                use syntax::ExpressionKind::Call;

                match value.kind.as_ref() {
                    Call {
                        caller,
                        callee: None,
                        ..
                    } => {
                        // What if else "self" method called repeatedly?
                        // The self could be memorized for the method scope, in a way.
                        let mut current_scope_id = scope_id;

                        loop {
                            let scope = self
                                .hierarchy
                                .scope_hierarchy
                                .scopes
                                .get(&current_scope_id)
                                .unwrap();

                            use scope::SyntaxNode::*;
                            match scope.node {
                                Root => {
                                    return Err(TypeError {
                                        kind: TypeErrorKind::InvalidSelf,
                                        message: "Invalid self usage! You can't use the self at root scope!",
                                        type_id: TypeId(0),
                                        span: expr.span.clone(),
                                    });
                                }
                                Class(syntax::Class { name, .. }) => {
                                    if scope.bindings.contains_key(&caller.into()) {
                                        let variant_id = self
                                            .hierarchy
                                            .variants_of_origin
                                            .get(
                                                self.hierarchy.name_to_origin_id.get(name).unwrap(),
                                            )
                                            .unwrap()
                                            .get(caller)
                                            .copied()
                                            .unwrap();

                                        self.declare_expr_type(expr.id, variant_id);

                                        return Ok(());
                                    } else {
                                        return Err(TypeError {
                                            kind: TypeErrorKind::InvalidSelf,
                                            message: "Invalid self declaration! Variant does not exist",
                                            type_id: TypeId(0),
                                            span: expr.span.clone(),
                                        });
                                    }
                                }
                                _ => {
                                    current_scope_id = scope.parent_id;
                                }
                            }
                        }
                    }
                    _ => Err(TypeError {
                        kind: TypeErrorKind::InvalidSelf,
                        message: "Invalid self declaration! You can only use the variant name of the class",
                        type_id: TypeId(0),
                        span: expr.span.clone(),
                    }),
                }
            }
            (Super, _) => {
                let mut current_scope_id = self
                    .hierarchy
                    .scope_hierarchy
                    .expr_to_scope_id
                    .get(&scope::ExprId(expr.id))
                    .copied()
                    .unwrap();

                loop {
                    let current_scope = self
                        .hierarchy
                        .scope_hierarchy
                        .scopes
                        .get(&current_scope_id)
                        .unwrap();

                    use scope::SyntaxNode::*;

                    match current_scope.node {
                        Root | Class(..) | Constructor(..) => return Err(
                            TypeError {
                                kind: TypeErrorKind::InvalidSuper,
                                message: "Invalid super usage! You can't use the super at root or class scope!",
                                type_id: TypeId(0),
                                span: expr.span.clone(),
                            }),
                        Method(functions) => {
                            let name = syntax::PatternName::from(&functions.first().as_ref().unwrap().name);

                            self.declare_expr_constraints(expr.id, &[Constraint::SuperCall(name)]);

                            self.hierarchy.expr_to_type.deferred.insert(scope::ExprId(expr.id));

                            return Ok(());
                        }
                        _ => current_scope_id = current_scope.parent_id,
                    }
                }
            }
            (Pass, _) => Ok(()),
        }
    }

    fn check_return_type(
        &mut self,
        body_id: usize,
        return_type_expr: Option<&'a syntax::Expression>,
        is_return_expr_mandatory: bool,
    ) -> TypeResult<Option<TypeId>> {
        if let Some(expr_id) = return_type_expr.map(|expr| expr.id) {
            self.declare_expr_constraints(expr_id, &[Constraint::TypeRef])
        }

        match return_type_expr.as_ref().map(|expr| expr.kind.as_ref()) {
            Some(syntax::ExpressionKind::Call {
                callee: Some(callee_expr),
                caller: variant_name,
                ..
            }) => {
                let syntax::ExpressionKind::Call {
                    callee: None,
                    caller: origin_name,
                    ..
                } = callee_expr.kind.as_ref()
                else {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidType,
                        message: "Invalid return type expression: Expected `ClassName.VariantName`",
                        type_id: crate::TypeId(0),
                        span: return_type_expr.as_ref().unwrap().span.clone(),
                    });
                };

                let Some(origin_type_id) =
                    self.hierarchy.name_to_origin_id.get(origin_name.as_ref())
                else {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidType,
                        message: "Type not found",
                        type_id: crate::TypeId(0),
                        span: return_type_expr.as_ref().unwrap().span.clone(),
                    });
                };

                let Some(variant_id) = self
                    .hierarchy
                    .variants_of_origin
                    .get(origin_type_id)
                    .unwrap()
                    .get(variant_name)
                    .copied()
                else {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidType,
                        message: "Variant not found",
                        type_id: crate::TypeId(0),
                        span: return_type_expr.as_ref().unwrap().span.clone(),
                    });
                };

                self.declare_expr_type(body_id, variant_id);

                Ok(Some(variant_id))
            }
            Some(syntax::ExpressionKind::Call {
                callee: None,
                caller: type_name,
                ..
            }) => {
                let Some(origin_type_id) = self
                    .hierarchy
                    .name_to_origin_id
                    .get(type_name.as_ref())
                    .copied()
                else {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidType,
                        message: "Type not found",
                        type_id: crate::TypeId(0),
                        span: return_type_expr.as_ref().unwrap().span.clone(),
                    });
                };

                self.declare_expr_type(body_id, origin_type_id);

                Ok(Some(origin_type_id))
            }
            Some(syntax::ExpressionKind::Literal(literal)) => {
                let type_definition = self.literal_type(literal.as_ref());
                let literal_type_id = type_definition.id();

                self.hierarchy
                    .types
                    .insert(literal_type_id, type_definition);

                self.declare_expr_type(body_id, literal_type_id);

                Ok(Some(literal_type_id))
            }
            None if is_return_expr_mandatory => {
                panic!("Gort! Klaatu Barada Nikto! Methods must declared with an return type.")
            }
            None => Ok(None),
            _ => Err(TypeError {
                kind: TypeErrorKind::InvalidType,
                message: "Unexpected return type expression",
                type_id: crate::TypeId(0),
                span: return_type_expr.as_ref().unwrap().span.clone(),
            }),
        }
    }

    fn literal_type(&mut self, literal: &'a syntax::Literal) -> TypeDefinition<'a> {
        let type_id = self.hierarchy.next_id();

        self.literal_type_with_custom_id(literal, type_id)
    }

    fn literal_type_with_custom_id(
        &self,
        literal: &'a syntax::Literal,
        type_id: TypeId,
    ) -> TypeDefinition<'a> {
        use syntax::Literal::*;

        match literal {
            Integer(_) => TypeDefinition::Literal {
                id: type_id,
                origin_id: EMBEDDED_TYPES.literal_integer,
                node: literal,
            },
            Float(_) => TypeDefinition::Literal {
                id: type_id,
                origin_id: EMBEDDED_TYPES.literal_float,
                node: literal,
            },
            Boolean(_) => TypeDefinition::Literal {
                id: type_id,
                origin_id: EMBEDDED_TYPES.boolean,
                node: literal,
            },
            Char(_) => TypeDefinition::Literal {
                id: type_id,
                origin_id: EMBEDDED_TYPES.char,
                node: literal,
            },
            Str(_) => TypeDefinition::Literal {
                id: type_id,
                origin_id: EMBEDDED_TYPES.string,
                node: literal,
            },
            LiteralList(_) => TypeDefinition::Literal {
                id: type_id,
                origin_id: EMBEDDED_TYPES.linked_list,
                node: literal,
            },
            LiteralArray(_) => TypeDefinition::Literal {
                id: type_id,
                origin_id: EMBEDDED_TYPES.array,
                node: literal,
            },
        }
    }

    fn declare_expr_constraints(&mut self, expr_id: usize, constraints: &[Constraint]) {
        self.hierarchy
            .expr_constraints
            .entry(scope::ExprId(expr_id))
            .or_default()
            .extend(constraints.iter().cloned());
    }

    fn declare_expr_type(&mut self, expr_id: usize, type_id: TypeId) {
        let expr_id = scope::ExprId(expr_id);

        self.hierarchy.expr_to_type.deferred.remove(&expr_id);

        self.hierarchy.expr_to_type.ok.insert(expr_id, type_id);
    }

    fn add_type_definition(&mut self, expr_id: usize, type_definition: TypeDefinition<'a>) {
        self.hierarchy
            .expr_to_type
            .ok
            .insert(scope::ExprId(expr_id), type_definition.id());

        self.hierarchy
            .types
            .insert(type_definition.id(), type_definition);
    }
}

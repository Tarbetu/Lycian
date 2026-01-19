use crate::definition::TypeDefinition;
use crate::error::{TypeError, TypeErrorKind, TypeResult};
use crate::hierarchy::EMBEDDED_TYPES;
use crate::type_bounds::{self, *};
use crate::{TypeId, hierarchy::Hierarchy};
use scope::ExprId;
use scope::SyntaxNode;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

pub(crate) struct TypeChecker<'a> {
    pub hierarchy: Hierarchy<'a>,
    pub errors: Vec<TypeError>,

    // Every expression points to it's parents (Roots don't have any parent)
    expr_to_parent: HashMap<ExprId, ExprId>,

    // Roots expressions to type
    root_to_type: HashMap<ExprId, TypeConstraint>,

    // Union by rank
    // Root to type
    rank: HashMap<ExprId, usize>,

    // For optimization
    empty_type_constraint: TypeConstraint,

    // Collecting the overload types
    method_overload_types: HashMap<Rc<String>, HashSet<TypeId>>,

    // Current classes scope
    class_scope_id: scope::ScopeId,

    // Current scope, can be changed by the expression
    scope_id: scope::ScopeId,
}

impl<'a> TypeChecker<'a> {
    pub fn new(hierarchy: Hierarchy<'a>) -> Self {
        Self {
            hierarchy,
            errors: Vec::new(),
            expr_to_parent: HashMap::new(),
            root_to_type: HashMap::new(),
            rank: HashMap::new(),
            empty_type_constraint: TypeConstraint::default(),
            method_overload_types: HashMap::new(),
            class_scope_id: scope::ScopeId(0),
            scope_id: scope::ScopeId(0),
        }
    }

    pub fn traverse(&mut self) -> TypeResult<()> {
        self.traverse_classes(0)
    }

    pub fn get_type(&mut self, expr: &syntax::Expression) -> &TypeConstraint {
        let root_expr = self.find_root(ExprId(expr.id));
        self.lookup_for_type_of_root(root_expr)
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
            children_ids,
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
            self.class_scope_id = scope_id;
            self.scope_id = scope_id;

            self.traverse_class()?;
        }

        self.traverse_classes(class_scope_index + 1)
    }

    fn traverse_class(&mut self) -> TypeResult<()> {
        let class_scope = self
            .hierarchy
            .scope_hierarchy
            .scopes
            .remove(&self.class_scope_id)
            .expect("Scope does not exist!");

        for child_scope_id in class_scope.children_ids.iter() {
            use scope::SyntaxNode::*;

            let child_scope = self
                .hierarchy
                .scope_hierarchy
                .scopes
                .get(child_scope_id)
                .expect("Scope does not exist!");

            match child_scope.node {
                Constructor(..) => {}
                Function(method) => {
                    self.scope_id = *child_scope_id;

                    let TypeConstraint::Exact(type_id) = self.extract_type_declaration(
                        method
                            .return_type
                            .as_ref()
                            .expect("Every method definition required to include a return type"),
                    )?
                    else {
                        unreachable!()
                    };

                    if let Err(err) = self.check(&method.body, TypeConstraint::Exact(type_id)) {
                        self.errors.push(err);
                    }

                    self.method_overload_types
                        .entry(method.name.clone())
                        .or_default()
                        .insert(type_id);
                }
                otherwise => unreachable!("Expected constructor or function, found: {otherwise:?}"),
            }
        }

        self.hierarchy
            .scope_hierarchy
            .scopes
            .insert(self.class_scope_id, class_scope);

        Ok(())
    }

    fn synthesize(&mut self, expr: &syntax::Expression) -> TypeResult<TypeConstraint> {
        use TypeConstraint::*;
        use syntax::ExpressionKind::*;

        match expr.kind.as_ref() {
            Grouping(inner_expr) => self.synthesize(inner_expr),
            Literal(literal) => {
                let literal_type_id = self.synthesize_literal_type(expr, literal)?;
                Ok(self.declare_type(expr, literal_type_id)?)
            }
            Binary(lhs, op, rhs) if op.is_logical() => {
                self.check(lhs, Exact(EMBEDDED_TYPES.boolean))?;
                self.check(rhs, Exact(EMBEDDED_TYPES.boolean))?;
                self.declare_type(expr, Exact(EMBEDDED_TYPES.boolean))?;
                Ok(EMBEDDED_TYPES.boolean.into())
            }
            Binary(lhs, op, rhs) if op.is_comparison() => {
                let lhs_type = self.synthesize(lhs)?;
                self.check(rhs, lhs_type)?;
                self.declare_type(expr, Exact(EMBEDDED_TYPES.boolean))?;
                self.declare_similarity(lhs, rhs)?;
                Ok(EMBEDDED_TYPES.boolean.into())
            }
            Binary(lhs, op, rhs) if op.is_arithmetic() => {
                let lhs_type = self.synthesize(lhs)?;
                let constraints = self.check(rhs, lhs_type)?;
                self.declare_similarity(lhs, rhs)?;
                self.declare_similarity(lhs, expr)?;
                Ok(constraints)
            }
            Binary(..) => unreachable!(),
            Unary(syntax::Operator::Not, inner_expr) => {
                self.check(inner_expr, Exact(EMBEDDED_TYPES.boolean))?;
                self.declare_type(expr, Exact(EMBEDDED_TYPES.boolean))?;
                Ok(EMBEDDED_TYPES.boolean.into())
            }
            Unary(syntax::Operator::Negate, inner_expr) => {
                let expected_type = TypeConstraint::needs_infer(TypeBounds::default().numeric());
                let expr_type = self.check(inner_expr, expected_type)?;
                self.declare_type(expr, expr_type)
            }
            IndexOperator(..) => self.check(expr, TypeConstraint::default()),
            Function(fn_def) => unimplemented!(),
            Call {
                caller,
                callee: None,
                args,
                block,
                ..
            } => {
                // TODO: Find the "block" parameter of the function

                let (binding_id, resolve_status) = self.find_binding(caller);

                let arg_types: TypeResult<Vec<TypeConstraint>> =
                    args.iter().map(|arg| self.synthesize(arg)).collect();
                let arg_types = arg_types?;

                let binding = self
                    .hierarchy
                    .scope_hierarchy
                    .bindings
                    .get(&binding_id)
                    .expect("Binding must exist!");

                use scope::ResolvedReferenceStatus;
                match resolve_status {
                    ResolvedReferenceStatus::Ok => {
                        use SyntaxNode::*;
                        match binding.node {
                            Root => unreachable!(),
                            Class(syntax::Class { name, .. }) => {
                                // Synthesized Class is a type that contains the constructors as methods
                                // Declared class is a type is supertype of variant types
                                // So, there might be a problem in here later
                                let origin_type_id = self
                                    .hierarchy
                                    .name_to_origin_id
                                    .get(name)
                                    .copied()
                                    .expect("Class must exist");

                                if !arg_types.is_empty() {
                                    Err(TypeError {
                                        kind: TypeErrorKind::ParameterFailure,
                                        message: "Generic types are not yet supported",
                                        type_id: origin_type_id,
                                        span: expr.span.clone(),
                                    })
                                } else {
                                    Ok(TypeConstraint::Exact(origin_type_id))
                                }
                            }
                            Constructor(
                                syntax::Class {
                                    name: class_name, ..
                                },
                                (name, params),
                            ) => self.synthesize_constructor_call(
                                class_name, name, params, &arg_types, &expr.span,
                            ),
                            Function(function) => {
                                let param_types: TypeResult<Vec<TypeConstraint>> = function
                                    .params
                                    .iter()
                                    .map(|param| self.extract_type_declaration(&param.value))
                                    .collect();
                                let param_types = param_types?;

                                if arg_types == param_types {
                                    if let Some(return_type_expr) = function.return_type.as_ref() {
                                        let return_type =
                                            self.extract_type_declaration(return_type_expr)?;

                                        Ok(return_type)
                                    } else {
                                        let mut type_bounds = TypeBounds::default().callable();

                                        if block.is_some() {
                                            type_bounds = type_bounds.accepts_block();
                                        }

                                        Ok(TypeConstraint::needs_infer(type_bounds))
                                    }
                                } else if arg_types.len() != param_types.len() {
                                    Err(TypeError {
                                        kind: TypeErrorKind::ParameterFailure,
                                        message: "Argument does not have same number of parameters",
                                        type_id: TypeId(0),
                                        span: expr.span.clone(),
                                    })
                                } else {
                                    Err(TypeError {
                                        kind: TypeErrorKind::ParameterFailure,
                                        message: "Argument types do not match parameter types",
                                        type_id: TypeId(0),
                                        span: expr.span.clone(),
                                    })
                                }
                            }
                            Pattern(syntax::Pattern {
                                value, condition, ..
                            }) => {
                                if let Some(condition) = condition {
                                    self.check(condition, Exact(EMBEDDED_TYPES.boolean))?;
                                }

                                let pattern_type = self.extract_type_declaration(value)?;

                                if let Exact(pattern_type) = pattern_type
                                    && pattern_type == EMBEDDED_TYPES.function
                                {
                                    unimplemented!("Pattern callables are another beast")
                                } else {
                                    Ok(pattern_type)
                                }
                            }
                            Method(_) => {
                                unreachable!("Method can not delivered with `Ok` Reference")
                            }
                            Expression(_) => {
                                unreachable!("Unnamed syntax nodes are not expected!")
                            }
                        }
                    }
                    ResolvedReferenceStatus::CheckOverload => {
                        let SyntaxNode::Method(overloads) = binding.node else {
                            unreachable!("It should be filtered at scope analysis!");
                        };

                        self.synthesize_method_call(
                            expr, overloads, binding_id, &arg_types, &expr.span,
                        )
                    }
                    ResolvedReferenceStatus::MaybeInherited => {
                        let binding_id = self.hierarchy.find_method(self.scope_id, caller);

                        if let Some(binding_id) = binding_id {
                            let binding = self
                                .hierarchy
                                .scope_hierarchy
                                .bindings
                                .get(&binding_id)
                                .expect("Binding is exist if binding_id is exist");

                            use scope::SyntaxNode::{Constructor, Method};
                            match binding.node {
                                Constructor(
                                    syntax::Class {
                                        name: class_name, ..
                                    },
                                    (constructor_name, params),
                                ) => self.synthesize_constructor_call(
                                    class_name,
                                    constructor_name,
                                    params,
                                    &arg_types,
                                    &expr.span,
                                ),
                                Method(overloads) => self.synthesize_method_call(
                                    expr, overloads, binding_id, &arg_types, &expr.span,
                                ),
                                _ => unreachable!(),
                            }
                        } else {
                            Err(TypeError {
                                kind: TypeErrorKind::UnboundSymbol,
                                message: "Symbol does not exist in the inherited classes",
                                type_id: TypeId(0),
                                span: expr.span.clone(),
                            })
                        }
                    }
                    ResolvedReferenceStatus::CheckMethodCall => {
                        unreachable!("Method call while function is not a method?")
                    }
                }
            }
            Call {
                caller,
                callee: Some(receiver),
                args,
                ..
            } => {
                let receiver_type_info = self.synthesize(receiver)?;

                let arg_types: TypeResult<Vec<TypeConstraint>> =
                    args.iter().map(|arg| self.synthesize(arg)).collect();
                let arg_types = arg_types?;

                match receiver_type_info {
                    Exact(receiver_type_id) => self.extract_type_from_method(
                        expr.id,
                        caller,
                        &arg_types,
                        receiver_type_id,
                        &expr.span,
                    ),
                    ExactLiteral(_literal) => Err(TypeError {
                        kind: TypeErrorKind::LiteralFailure,
                        message: "Literals are not callables",
                        type_id: TypeId(0),
                        span: expr.span.clone(),
                    }),
                    UnresolvedFunction { return_type, .. } => Ok(*return_type),
                    NeedsInfer(bounds) => {
                        if bounds.borrow().upper_bounds.is_empty() {
                            unimplemented!()
                        } else {
                            unimplemented!()
                        }
                    }
                }
            }
            _ => unimplemented!(),
        }
    }

    fn check(
        &mut self,
        expr: &syntax::Expression,
        declared_type: TypeConstraint,
    ) -> TypeResult<TypeConstraint> {
        use TypeConstraint::*;
        use syntax::ExpressionKind::*;

        match (expr.kind.as_ref(), declared_type) {
            (_, Exact(object_type_id)) if EMBEDDED_TYPES.object == object_type_id => {
                self.declare_type(expr, Exact(object_type_id))
            }
            (Grouping(inner_expr), declared_type) => self.check(inner_expr, declared_type),
            (Literal(literal), declared_type) => {
                self.declare_literal(expr, literal.clone(), declared_type)
            }
            // NOTE: Literal types can only composed with other literal types
            // This is constexpr evaluation based on types
            // For runtime values, literal types can not be used
            (Binary(lhs, op, rhs), Exact(declared_type_id))
                if op.is_logical()
                    && (EMBEDDED_TYPES.literal_true == declared_type_id
                        || EMBEDDED_TYPES.literal_false == declared_type_id) =>
            {
                let Exact(lhs_type) = self.synthesize(lhs)? else {
                    return Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Expected literal boolean which built by literal booleans",
                        type_id: declared_type_id,
                        span: expr.span.clone(),
                    });
                };

                let Exact(rhs_type) = self.synthesize(rhs)? else {
                    return Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Expected literal true which built by literal booleans",
                        type_id: declared_type_id,
                        span: expr.span.clone(),
                    });
                };

                use syntax::Operator::*;
                match op {
                    And | Or
                        if declared_type_id == EMBEDDED_TYPES.literal_true
                            && EMBEDDED_TYPES.literal_true == rhs_type
                            && EMBEDDED_TYPES.literal_true == lhs_type =>
                    {
                        self.declare_type(expr, Exact(declared_type_id))
                    }
                    And if declared_type_id == EMBEDDED_TYPES.literal_true => Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Expected literal true built by a \"AND\" operator",
                        type_id: declared_type_id,
                        span: expr.span.clone(),
                    }),
                    Or if declared_type_id == EMBEDDED_TYPES.literal_true
                        && (EMBEDDED_TYPES.literal_true == lhs_type
                            || EMBEDDED_TYPES.literal_true == rhs_type) =>
                    {
                        self.declare_type(expr, Exact(declared_type_id))
                    }
                    Or if declared_type_id == EMBEDDED_TYPES.literal_true => Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Expected literal true built by a \"OR\" operator",
                        type_id: declared_type_id,
                        span: expr.span.clone(),
                    }),
                    And | Or
                        if declared_type_id == EMBEDDED_TYPES.literal_false
                            && EMBEDDED_TYPES.literal_false == rhs_type
                            && EMBEDDED_TYPES.literal_false == lhs_type =>
                    {
                        self.declare_type(expr, Exact(declared_type_id))
                    }
                    Or if declared_type_id == EMBEDDED_TYPES.literal_false => Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Expected literal false built by a \"OR\" operator",
                        type_id: declared_type_id,
                        span: expr.span.clone(),
                    }),
                    And if declared_type_id == EMBEDDED_TYPES.literal_false
                        && (EMBEDDED_TYPES.literal_false == lhs_type
                            || EMBEDDED_TYPES.literal_false == rhs_type) =>
                    {
                        self.declare_type(expr, Exact(declared_type_id))
                    }
                    And if declared_type_id == EMBEDDED_TYPES.literal_false => Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Expected literal false built by a \"AND\" operator",
                        type_id: declared_type_id,
                        span: expr.span.clone(),
                    }),
                    _ => unreachable!(),
                }
            }
            (Binary(lhs, op, rhs), Exact(declared_type_id))
                if op.is_logical() && EMBEDDED_TYPES.boolean == declared_type_id =>
            {
                self.check(lhs, Exact(EMBEDDED_TYPES.boolean))?;
                self.check(rhs, Exact(EMBEDDED_TYPES.boolean))?;
                self.declare_type(expr, Exact(EMBEDDED_TYPES.boolean))
            }
            (Binary(_, op, _), Exact(declared_type_id)) if op.is_logical() => Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Expected boolean inside a logical expression",
                type_id: declared_type_id,
                span: expr.span.clone(),
            }),
            (Binary(lhs, op, rhs), NeedsInfer(expected_bounds))
                if op.is_logical() && expected_bounds.borrow().can_be_casted_to_boolean() =>
            {
                self.check(lhs, Exact(EMBEDDED_TYPES.boolean))?;
                self.check(rhs, Exact(EMBEDDED_TYPES.boolean))?;
                self.declare_type(expr, Exact(EMBEDDED_TYPES.boolean))
            }
            (Binary(_, op, _), ExactLiteral(_expected_bounds)) if op.is_logical() => {
                Err(TypeError {
                    kind: TypeErrorKind::TypeMismatch,
                    message: "Expected boolean inside a logical expression",
                    type_id: TypeId(0),
                    span: expr.span.clone(),
                })
            }
            (Binary(lhs, op, rhs), Exact(declared_type_id))
                if op.is_arithmetic() && EMBEDDED_TYPES.is_number(declared_type_id) =>
            {
                self.check(lhs, Exact(declared_type_id))?;
                self.check(rhs, Exact(declared_type_id))?;
                self.declare_similarity(lhs, rhs)?;
                self.declare_similarity(expr, lhs)?;
                self.declare_type(expr, Exact(declared_type_id))
            }
            (Binary(lhs, op, rhs), ExactLiteral(target_literal)) if op.is_arithmetic() => {
                use syntax::Literal::*;

                let ExactLiteral(ref lhs_literal @ Integer(_) | ref lhs_literal @ Float(_)) =
                    self.synthesize(expr)?
                else {
                    return Err(TypeError {
                        kind: TypeErrorKind::LiteralFailure,
                        message: "Literal types must composed by other literals",
                        type_id: TypeId(0),
                        span: expr.span.clone(),
                    });
                };
                let ExactLiteral(ref rhs_literal @ Integer(_) | ref rhs_literal @ Float(_)) =
                    self.synthesize(expr)?
                else {
                    return Err(TypeError {
                        kind: TypeErrorKind::LiteralFailure,
                        message: "Literal types must composed by other literals",
                        type_id: TypeId(0),
                        span: expr.span.clone(),
                    });
                };

                match target_literal {
                    Integer(_) | Float(_) => {
                        let target_literal = {
                            use syntax::Operator::*;
                            (match op {
                                Add => lhs_literal + rhs_literal,
                                Substract => lhs_literal - rhs_literal,
                                Multiply => lhs_literal * rhs_literal,
                                Divide => lhs_literal / rhs_literal,
                                Modulo => lhs_literal % rhs_literal,
                                _ => unreachable!(),
                            })
                            .expect("The values must be a number")
                        };

                        self.declare_similarity(lhs, rhs)?;
                        self.declare_similarity(expr, lhs)?;
                        self.declare_type(expr, ExactLiteral(target_literal))
                    }
                    _ => Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Expected numeric, found another kind of literal",
                        type_id: TypeId(0),
                        span: expr.span.clone(),
                    }),
                }
            }
            (Binary(lhs, op, rhs), NeedsInfer(expected_bounds))
                if op.is_arithmetic() && expected_bounds.borrow().can_be_casted_to_boolean() =>
            {
                self.check(lhs, NeedsInfer(expected_bounds.clone()))?;
                self.check(rhs, NeedsInfer(expected_bounds.clone()))?;
                self.declare_similarity(lhs, rhs)?;
                self.declare_type(expr, NeedsInfer(expected_bounds))
            }
            (Binary(_lhs, op, _rhs), NeedsInfer(_)) if op.is_arithmetic() => Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Expression can not casted into a boolean!",
                type_id: TypeId(0),
                span: expr.span.clone(),
            }),
            (Binary(lhs, op, rhs), Exact(declared_type_id))
                if op.is_comparison() && EMBEDDED_TYPES.boolean == declared_type_id =>
            {
                self.check(
                    lhs,
                    TypeConstraint::needs_infer(TypeBounds::default().numeric()),
                )?;
                self.check(
                    rhs,
                    TypeConstraint::needs_infer(TypeBounds::default().numeric()),
                )?;
                self.declare_similarity(lhs, rhs)?;
                self.declare_type(expr, Exact(EMBEDDED_TYPES.boolean))
            }
            (Binary(rhs, op, lhs), NeedsInfer(type_bounds))
                if op.is_comparison() && type_bounds.borrow().can_be_casted_to_boolean() =>
            {
                self.check(
                    lhs,
                    TypeConstraint::needs_infer(TypeBounds::default().numeric()),
                )?;
                self.check(
                    rhs,
                    TypeConstraint::needs_infer(TypeBounds::default().numeric()),
                )?;
                self.declare_similarity(lhs, rhs)?;
                self.declare_type(expr, Exact(EMBEDDED_TYPES.boolean))
            }
            (Binary(lhs, op, rhs), Exact(declared_type_id))
                if op.is_comparison() && EMBEDDED_TYPES.literal_true == declared_type_id
                    || EMBEDDED_TYPES.literal_false == declared_type_id =>
            {
                use syntax::Literal::*;

                let ExactLiteral(ref lhs_literal @ Integer(_) | ref lhs_literal @ Float(_)) =
                    self.synthesize(lhs)?
                else {
                    return Err(TypeError {
                        kind: TypeErrorKind::LiteralFailure,
                        message: "Literal types must composed by other literals",
                        type_id: TypeId(0),
                        span: expr.span.clone(),
                    });
                };
                let ExactLiteral(ref rhs_literal @ Integer(_) | ref rhs_literal @ Float(_)) =
                    self.synthesize(rhs)?
                else {
                    return Err(TypeError {
                        kind: TypeErrorKind::LiteralFailure,
                        message: "Literal types must composed by other literals",
                        type_id: TypeId(0),
                        span: expr.span.clone(),
                    });
                };

                use syntax::Operator::*;
                let operation_result = match op {
                    Equal => lhs_literal == rhs_literal,
                    NotEqual => lhs_literal != rhs_literal,
                    _ => {
                        let Boolean(result) = match op {
                            Smaller => lhs_literal.less(rhs_literal),
                            SmallerOrEqual => lhs_literal.less_equal(rhs_literal),
                            Greater => lhs_literal.greater(rhs_literal),
                            GreaterOrEqual => lhs_literal.greater_equal(rhs_literal),
                            _ => unreachable!(),
                        }
                        .expect("The values must be a number") else {
                            panic!("These shouldn't produce another kind of literal!")
                        };

                        result
                    }
                };

                if (operation_result && EMBEDDED_TYPES.literal_true == declared_type_id)
                    || (!operation_result && EMBEDDED_TYPES.literal_false == declared_type_id)
                {
                    self.declare_similarity(lhs, rhs)?;
                    self.declare_similarity(expr, lhs)?;
                    self.declare_type(expr, Exact(declared_type_id))
                } else {
                    Err(TypeError {
                        kind: TypeErrorKind::LiteralFailure,
                        message: "These values can not produce the answer",
                        type_id: declared_type_id,
                        span: expr.span.clone(),
                    })
                }
            }
            (Binary(_, op, _), Exact(declared_type_id)) if op.is_comparison() => Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Expected boolean as a result of comparison expression",
                type_id: declared_type_id,
                span: expr.span.clone(),
            }),
            (Binary(_, op, _), NeedsInfer(_)) if op.is_comparison() => Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Expected boolean as a result of comparison expression",
                type_id: TypeId(0),
                span: expr.span.clone(),
            }),
            (Binary(..), _) => unreachable!(),
            (Unary(syntax::Operator::Not, inner_expr), Exact(declared_type_id))
                if declared_type_id == EMBEDDED_TYPES.literal_true =>
            {
                self.check(inner_expr, Exact(EMBEDDED_TYPES.literal_false))?;
                self.declare_similarity(expr, inner_expr)?;
                self.declare_type(inner_expr, Exact(EMBEDDED_TYPES.literal_true))
            }
            (Unary(syntax::Operator::Not, inner_expr), Exact(declared_type_id))
                if declared_type_id == EMBEDDED_TYPES.literal_false =>
            {
                self.check(inner_expr, Exact(EMBEDDED_TYPES.literal_true))?;
                self.declare_similarity(expr, inner_expr)?;
                self.declare_type(inner_expr, Exact(EMBEDDED_TYPES.literal_false))
            }
            (Unary(syntax::Operator::Not, inner_expr), Exact(declared_type_id))
                if declared_type_id == EMBEDDED_TYPES.boolean =>
            {
                self.check(inner_expr, Exact(EMBEDDED_TYPES.boolean))?;
                self.declare_similarity(expr, inner_expr)?;
                self.declare_type(inner_expr, Exact(EMBEDDED_TYPES.boolean))
            }
            (Unary(syntax::Operator::Not, inner_expr), NeedsInfer(expected_bounds))
                if expected_bounds.borrow().can_be_casted_to_boolean() =>
            {
                self.check(inner_expr, Exact(EMBEDDED_TYPES.boolean))?;
                self.declare_similarity(expr, inner_expr)?;
                self.declare_type(inner_expr, Exact(EMBEDDED_TYPES.boolean))
            }
            (Unary(syntax::Operator::Not, _), NeedsInfer(_)) => Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Expected boolean inside a Not expression",
                type_id: TypeId(0),
                span: expr.span.clone(),
            }),
            (Unary(syntax::Operator::Not, _), Exact(declared_type_id)) => Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Expected boolean inside a Not expression",
                type_id: declared_type_id,
                span: expr.span.clone(),
            }),
            (Unary(syntax::Operator::Negate, _), Exact(declared_type_id))
                if EMBEDDED_TYPES.is_unsigned_number(declared_type_id) =>
            {
                Err(TypeError {
                    kind: TypeErrorKind::TypeMismatch,
                    message: "Cannot negate an unsigned number",
                    type_id: declared_type_id,
                    span: expr.span.clone(),
                })
            }
            (Unary(syntax::Operator::Negate, inner_expr), Exact(declared_type_id))
                if EMBEDDED_TYPES.is_number(declared_type_id) =>
            {
                self.check(inner_expr, Exact(declared_type_id))?;
                self.declare_similarity(expr, inner_expr)?;
                self.declare_type(expr, Exact(declared_type_id))
            }
            (Unary(syntax::Operator::Negate, inner_expr), ExactLiteral(literal)) => {
                use syntax::Literal::*;
                match literal {
                    Integer(_) | Float(_) => {
                        let literal = (-&literal).expect("The value must be a number");
                        self.check(inner_expr, ExactLiteral(literal.clone()))?;
                        self.declare_similarity(expr, inner_expr)?;
                        self.declare_type(expr, ExactLiteral(literal))
                    }
                    _ => Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Expected numeric, found another kind of literal",
                        type_id: TypeId(0),
                        span: expr.span.clone(),
                    }),
                }
            }
            (Unary(syntax::Operator::Negate, inner_expr), NeedsInfer(expected_bounds))
                if expected_bounds.borrow().can_be_casted_to_number() =>
            {
                expected_bounds.borrow_mut().must_be_numeric = true;
                expected_bounds.borrow_mut().must_be_signed = true;
                self.check(inner_expr, NeedsInfer(expected_bounds.clone()))?;
                self.declare_similarity(expr, inner_expr)?;
                self.declare_type(expr, NeedsInfer(expected_bounds))
            }
            (Unary(syntax::Operator::Negate, _), NeedsInfer(_)) => Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Expression can not casted into a integer!",
                type_id: TypeId(0),
                span: expr.span.clone(),
            }),
            (Unary(..), _) => unreachable!(),
            (IndexOperator(container, indexer), declared_type) => {
                self.check(indexer, Exact(EMBEDDED_TYPES.uIntSize))?;
                use syntax::Literal::{LiteralArray, LiteralList};

                match self.synthesize(container)? {
                    Exact(container_type_id) => {
                        let container_type = self
                            .hierarchy
                            .types
                            .get(&container_type_id)
                            .expect("The type must exist on hierarchy");

                        let TypeDefinition::TypeInstance { args, .. } = container_type else {
                            unreachable!("Container must be a type instance");
                        };

                        if args.first() == Some(&container_type_id) {
                            self.declare_type(container, declared_type.clone())?;
                            Ok(declared_type)
                        } else {
                            Err(TypeError {
                                kind: TypeErrorKind::TypeMismatch,
                                message: "Container type does not match the declared type",
                                type_id: container_type_id,
                                span: expr.span.clone(),
                            })
                        }
                    }
                    ExactLiteral(LiteralArray(array)) => {
                        if let Some(first_expr) = array.borrow().first() {
                            let result_type = self.check(first_expr, declared_type)?;
                            self.declare_type(expr, result_type)
                        } else {
                            Err(TypeError {
                                kind: TypeErrorKind::LiteralFailure,
                                message: "Indexed array is empty!",
                                type_id: TypeId(0),
                                span: expr.span.clone(),
                            })
                        }
                    }
                    ExactLiteral(LiteralList(list)) => {
                        if let Some(first_expr) = list.borrow().front() {
                            let result_type = self.check(first_expr, declared_type)?;
                            self.declare_type(expr, result_type)
                        } else {
                            Err(TypeError {
                                kind: TypeErrorKind::LiteralFailure,
                                message: "Indexed list is empty!",
                                type_id: TypeId(0),
                                span: expr.span.clone(),
                            })
                        }
                    }
                    _ => Err(TypeError {
                        kind: TypeErrorKind::LiteralFailure,
                        message: "Invalid usage of index operator",
                        type_id: TypeId(0),
                        span: expr.span.clone(),
                    }),
                }
            }
            (
                Call {
                    caller,
                    callee,
                    args,
                    block,
                    ..
                },
                defined_type,
            ) => {
                unimplemented!()
            }
            _ => unimplemented!(),
        }
    }

    fn check_integer(
        &mut self,
        expr: &syntax::Expression,
        declared_type_id: TypeId,
        number: &syntax::BigFloat,
    ) -> Result<TypeConstraint, TypeError> {
        let integer = number.to_integer().expect("to be finite!");

        // Why the hell is that every DRYing attempt is ending with "Nah. Not worth it. Let's hardcode it" in Rust?
        // If you make a macro in here, it will end with passing everything as an argument because these are "hygenic"
        // Closures might be a better alternative in here. But oh, RugInteger.to_i16 isn't return the same thing with RugInteger.to_i8 but you're just caring about is it a Some or None?
        // Generic closures aren't a thing for now.
        // To avoid situtations like this, the method call also could be done with literal strings and literal string concatination is must be something to exist.
        // Another problem is the definition of syntax.
        // You can not transfer the syntax, it's not a thing that you can pass to the functions
        // Lisp folks were right for some details.
        if !EMBEDDED_TYPES.is_integer(declared_type_id) {
            let type_def = self
                .hierarchy
                .types
                .get(&declared_type_id)
                .expect("Type must exist at this point");

            match type_def {
                TypeDefinition::Literal {
                    node: syntax::Literal::Integer(number_from_type),
                    ..
                } if number_from_type == number => {
                    self.declare_type(expr, TypeConstraint::Exact(declared_type_id))
                }
                TypeDefinition::Literal { .. } => Err(TypeError {
                    kind: TypeErrorKind::TypeMismatch,
                    message: "Literal type does not match",
                    type_id: declared_type_id,
                    span: expr.span.clone(),
                }),
                _ => Err(TypeError {
                    kind: TypeErrorKind::TypeMismatch,
                    message: "Expected integer type",
                    type_id: declared_type_id,
                    span: expr.span.clone(),
                }),
            }
        } else if (declared_type_id == EMBEDDED_TYPES.int8 && integer.to_i8().is_some()) ||
                            (declared_type_id == EMBEDDED_TYPES.int16 && integer.to_i16().is_some()) ||
                            (declared_type_id == EMBEDDED_TYPES.int32 && integer.to_i32().is_some()) ||
                            (declared_type_id == EMBEDDED_TYPES.int64 && integer.to_i64().is_some()) ||
                            (declared_type_id == EMBEDDED_TYPES.int128 && integer.to_i128().is_some()) ||
                            // TODO: Get the compilation information later
                            (declared_type_id == EMBEDDED_TYPES.intSize && integer.to_isize().is_some()) ||
                            (declared_type_id == EMBEDDED_TYPES.uInt8 && integer.to_u8().is_some()) ||
                            (declared_type_id == EMBEDDED_TYPES.uInt16 && integer.to_u16().is_some()) ||
                            (declared_type_id == EMBEDDED_TYPES.uInt32 && integer.to_u32().is_some()) ||
                            (declared_type_id == EMBEDDED_TYPES.uInt64 && integer.to_u64().is_some()) ||
                            (declared_type_id == EMBEDDED_TYPES.uInt128 && integer.to_u128().is_some()) ||
                            (declared_type_id == EMBEDDED_TYPES.uIntSize && integer.to_usize().is_some())
        {
            self.declare_type(expr, TypeConstraint::Exact(declared_type_id))
        } else {
            Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Integer type overflow!",
                type_id: declared_type_id,
                span: expr.span.clone(),
            })
        }
    }

    fn extract_type_declaration(
        &mut self,
        expr: &'a syntax::Expression,
    ) -> TypeResult<TypeConstraint> {
        use TypeConstraint::Exact;
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

                Ok(Exact(origin_type_id))
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
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidType,
                        message: "Invalid return type expression: Expected `ClassName.VariantName` or `ClassName`",
                        type_id: TypeId(0),
                        span: expr.span.clone(),
                    });
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

                Ok(Exact(variant_id))
            }
            Literal(literal) => self.synthesize_literal_type(expr, literal),
            Grouping(inner_expr) => self.extract_type_declaration(inner_expr),
            _ => Err(TypeError {
                kind: TypeErrorKind::InvalidType,
                message: "Invalid return type",
                type_id: TypeId(0),
                span: expr.span.clone(),
            }),
        }
    }

    fn synthesize_literal_type(
        &mut self,
        expr: &syntax::Expression,
        literal: &syntax::Literal,
    ) -> TypeResult<TypeConstraint> {
        use TypeConstraint::*;
        use syntax::Literal::*;

        match literal.clone() {
            literal @ Integer(_) | literal @ Float(_) => {
                Ok(self.declare_type(expr, ExactLiteral(literal))?)
            }
            Boolean(true) => Ok(self.declare_type(expr, EMBEDDED_TYPES.literal_true.into())?),
            Boolean(false) => Ok(self.declare_type(expr, EMBEDDED_TYPES.literal_false.into())?),
            Char(_) => {
                let type_constraint = match self.hierarchy.literal_types.chars.get(literal).copied()
                {
                    Some(type_id) => Exact(type_id),
                    None => {
                        let type_id = self.hierarchy.next_id();
                        let type_def = TypeDefinition::Literal {
                            id: type_id,
                            origin_id: EMBEDDED_TYPES.char,
                            node: literal.clone(),
                        };
                        self.hierarchy.types.insert(type_id, type_def);
                        Exact(type_id)
                    }
                };

                Ok(self.declare_type(expr, type_constraint)?)
            }
            Str(_) => {
                let type_constraint =
                    match self.hierarchy.literal_types.strings.get(literal).copied() {
                        Some(type_id) => Exact(type_id),
                        None => {
                            let type_id = self.hierarchy.next_id();
                            let type_def = TypeDefinition::Literal {
                                id: type_id,
                                origin_id: EMBEDDED_TYPES.string,
                                node: literal.clone(),
                            };
                            self.hierarchy.types.insert(type_id, type_def);
                            Exact(type_id)
                        }
                    };

                Ok(self.declare_type(expr, type_constraint)?)
            }
            LiteralList(expr_list) => {
                if let Some(first_expr) = expr_list.borrow().front() {
                    let mut type_constraints = self.synthesize(first_expr)?;

                    for expr in expr_list.borrow().iter().skip(1) {
                        type_constraints = self.check(expr, type_constraints)?;
                        self.declare_similarity(first_expr, expr)?;
                    }
                };

                Ok(self.declare_type(expr, ExactLiteral(LiteralList(expr_list.clone())))?)
            }
            LiteralArray(expr_array) => {
                if let Some(first_expr) = expr_array.borrow().first() {
                    let mut type_constraints = self.synthesize(first_expr)?;

                    for expr in expr_array.borrow().iter().skip(1) {
                        type_constraints = self.check(expr, type_constraints)?;
                        self.declare_similarity(first_expr, expr)?;
                    }
                };

                Ok(self.declare_type(expr, ExactLiteral(LiteralArray(expr_array.clone())))?)
            }
        }
    }

    fn synthesize_constructor_call(
        &mut self,
        class_name: &'a String,
        constructor_name: &'a String,
        params: &'a [syntax::Pattern],
        arg_types: &[TypeConstraint],
        span: &scanner::Span,
    ) -> TypeResult<TypeConstraint> {
        let origin_type_id = self
            .hierarchy
            .name_to_origin_id
            .get(class_name)
            .copied()
            .expect("Class must exist");

        let variant_id = self
            .hierarchy
            .variants_of_origin
            .get(&origin_type_id)
            .expect("Class must exist!")
            .get(constructor_name)
            .copied()
            .expect("Variant must exist!");

        let param_types: TypeResult<Vec<TypeConstraint>> = params
            .iter()
            .map(|param| self.extract_type_declaration(&param.value))
            .collect();
        let param_types = param_types?;

        if arg_types == param_types {
            Ok(TypeConstraint::Exact(variant_id))
        } else if arg_types.len() != param_types.len() {
            Err(TypeError {
                kind: TypeErrorKind::ParameterFailure,
                message: "Argument does not have same number of parameters",
                type_id: variant_id,
                span: span.clone(),
            })
        } else {
            Err(TypeError {
                kind: TypeErrorKind::ParameterFailure,
                message: "Argument types do not match parameter types",
                type_id: variant_id,
                span: span.clone(),
            })
        }
    }

    fn synthesize_method_call(
        &mut self,
        expr: &syntax::Expression,
        overloads: &'a [syntax::Function],
        binding_id: scope::BindingId,
        arg_types: &[TypeConstraint],
        span: &scanner::Span,
    ) -> TypeResult<TypeConstraint> {
        if overloads.len() == 1 {
            let method = &overloads[0];

            let param_types: TypeResult<Vec<TypeConstraint>> = method
                .params
                .iter()
                .map(|param| self.extract_type_declaration(&param.value))
                .collect();
            let param_types = param_types?;

            if param_types == arg_types {
                Ok(self.extract_type_declaration(
                    method
                        .return_type
                        .as_ref()
                        .expect("Every method required to have return type declaration"),
                )?)
            } else {
                Err(TypeError {
                    kind: TypeErrorKind::OverloadFailure,
                    message: "Ambiguous overload candidate",
                    type_id: TypeId(0),
                    span: span.clone(),
                })
            }
        } else if arg_types
            .iter()
            .all(|arg_type| arg_type.as_exact().is_some())
        {
            let mut overload_selection = None;
            for method in overloads {
                let param_types: TypeResult<Vec<TypeConstraint>> = method
                    .params
                    .iter()
                    .map(|param| self.extract_type_declaration(&param.value))
                    .collect();
                let param_types = param_types?;

                if param_types == arg_types {
                    if overload_selection.is_none() {
                        overload_selection =
                            Some(self.extract_type_declaration(
                                method.return_type.as_ref().expect(
                                    "Every method required to have return type declaration",
                                ),
                            )?);
                    } else {
                        return Err(TypeError {
                            kind: TypeErrorKind::OverloadFailure,
                            message: "Ambiguous overload candidate",
                            type_id: TypeId(0),
                            span: span.clone(),
                        });
                    }
                }
            }

            if let Some(overload) = overload_selection {
                Ok(overload)
            } else {
                Err(TypeError {
                    kind: TypeErrorKind::OverloadFailure,
                    message: "Does not match with any candidate",
                    type_id: TypeId(0),
                    span: span.clone(),
                })
            }
        } else {
            Ok(self.declare_type(
                expr,
                TypeConstraint::needs_infer(
                    TypeBounds::default().callable().results_of(binding_id),
                ),
            )?)
        }
    }

    fn find_root(&mut self, expr_id: ExprId) -> ExprId {
        self.declare_expression(expr_id);

        let parent_id = self.expr_to_parent.get(&expr_id).copied().unwrap();

        if expr_id == parent_id {
            parent_id
        } else {
            let root = self.find_root(parent_id);
            self.expr_to_parent.insert(expr_id, parent_id);
            root
        }
    }

    fn lookup_for_type_of_root(&mut self, root_expr_id: ExprId) -> &TypeConstraint {
        self.root_to_type
            .get(&root_expr_id)
            .unwrap_or(&self.empty_type_constraint)
    }

    fn declare_literal(
        &mut self,
        expr: &syntax::Expression,
        literal: syntax::Literal,
        declared_type: TypeConstraint,
    ) -> TypeResult<TypeConstraint> {
        use TypeConstraint::*;
        use syntax::Literal::*;

        match (literal, declared_type) {
            (anyliteral, ExactLiteral(declared_literal)) if anyliteral == declared_literal => {
                self.declare_type(expr, ExactLiteral(declared_literal))
            }
            (_anyliteral, ExactLiteral(_declared_literal)) => Err(TypeError {
                kind: TypeErrorKind::LiteralFailure,
                message: "Literal types are not the same",
                type_id: TypeId(0),
                span: expr.span.clone(),
            }),
            (_anyliteral, UnresolvedFunction { .. }) => Err(TypeError {
                kind: TypeErrorKind::LiteralFailure,
                message: "Literal types can not be a function type",
                type_id: TypeId(0),
                span: expr.span.clone(),
            }),
            (LiteralArray(elements), Exact(declared_type_id)) => {
                use TypeDefinition::{Literal, TypeInstance};

                match self
                    .hierarchy
                    .types
                    .get(&declared_type_id)
                    .expect("Type must be exist!")
                {
                    TypeInstance { origin_id, .. } | Literal { origin_id, .. }
                        if *origin_id != EMBEDDED_TYPES.array =>
                    {
                        Err(TypeError {
                            kind: TypeErrorKind::TypeMismatch,
                            message: "Expected array type",
                            type_id: declared_type_id,
                            span: expr.span.clone(),
                        })
                    }
                    TypeInstance { args, .. } => {
                        if elements.borrow().is_empty() {
                            self.declare_type(expr, Exact(declared_type_id))
                        } else {
                            let element_type = args.first().copied().expect(
                                "Empty type instance arg must be checked before this point!",
                            );

                            let first_element_expr = elements.borrow();
                            let first_element_expr = first_element_expr
                                .first()
                                .expect("Existence must be checked before this point!");
                            self.declare_type(first_element_expr, Exact(element_type))?;

                            for element_expr in elements.borrow().iter().skip(1) {
                                self.declare_type(element_expr, Exact(element_type))?;
                                self.declare_similarity(first_element_expr, element_expr)?
                            }

                            self.declare_type(expr, Exact(declared_type_id))
                        }
                    }
                    Literal { node, .. } => {
                        let syntax::Literal::LiteralArray(_) = node else {
                            return Err(TypeError {
                                kind: TypeErrorKind::TypeMismatch,
                                message: "Expected array literal",
                                type_id: declared_type_id,
                                span: expr.span.clone(),
                            });
                        };

                        self.declare_type(expr, Exact(declared_type_id))
                    }
                    _ => Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Expected qualified array type",
                        type_id: declared_type_id,
                        span: expr.span.clone(),
                    }),
                }
            }
            (LiteralList(elements), Exact(declared_type_id)) => {
                use TypeDefinition::{Literal, TypeInstance};

                match self
                    .hierarchy
                    .types
                    .get(&declared_type_id)
                    .expect("Type must be exist!")
                {
                    TypeInstance { origin_id, .. } | Literal { origin_id, .. }
                        if *origin_id != EMBEDDED_TYPES.linked_list =>
                    {
                        Err(TypeError {
                            kind: TypeErrorKind::TypeMismatch,
                            message: "Expected array type",
                            type_id: declared_type_id,
                            span: expr.span.clone(),
                        })
                    }
                    TypeInstance { args, .. } => {
                        if elements.borrow().is_empty() {
                            self.declare_type(expr, Exact(declared_type_id))
                        } else {
                            let element_type = args.first().copied().expect(
                                "Empty type instance arg must be checked before this point!",
                            );

                            let first_element_expr = elements.borrow();
                            let first_element_expr = first_element_expr
                                .iter()
                                .nth(0)
                                .expect("Existence must be checked before this point!");
                            self.declare_type(first_element_expr, Exact(element_type))?;

                            for element_expr in elements.borrow().iter().skip(1) {
                                self.declare_type(element_expr, Exact(element_type))?;
                                self.declare_similarity(first_element_expr, element_expr)?
                            }

                            self.declare_type(expr, Exact(declared_type_id))
                        }
                    }
                    Literal { node, .. } => {
                        let syntax::Literal::LiteralList(_) = node else {
                            return Err(TypeError {
                                kind: TypeErrorKind::TypeMismatch,
                                message: "Expected list literal",
                                type_id: declared_type_id,
                                span: expr.span.clone(),
                            });
                        };

                        self.declare_type(expr, Exact(declared_type_id))
                    }
                    _ => Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Expected qualified list type",
                        type_id: declared_type_id,
                        span: expr.span.clone(),
                    }),
                }
            }
            (container @ LiteralArray(_) | container @ LiteralList(_), NeedsInfer(type_bounds))
                if type_bounds.borrow().can_be_casted_to_container() =>
            {
                self.declare_type(expr, ExactLiteral(container))
            }
            (LiteralArray(_), NeedsInfer(_)) => Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Expected array type",
                type_id: TypeId(0),
                span: expr.span.clone(),
            }),
            (LiteralList(_), NeedsInfer(_)) => Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Expected list type",
                type_id: TypeId(0),
                span: expr.span.clone(),
            }),
            (Integer(number), Exact(declared_type_id)) => {
                self.check_integer(expr, declared_type_id, &number)
            }
            (Float(number), Exact(declared_type_id)) => {
                if EMBEDDED_TYPES.is_integer(declared_type_id) {
                    self.check_integer(expr, declared_type_id, &number)
                } else if !EMBEDDED_TYPES.is_float(declared_type_id) {
                    Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Expected integer type",
                        type_id: declared_type_id,
                        span: expr.span.clone(),
                    })
                } else if (declared_type_id == EMBEDDED_TYPES.float32
                    && (f32::MIN..=f32::MAX).contains(number.as_ref()))
                    || (declared_type_id == EMBEDDED_TYPES.float64
                        && (f64::MIN..=f64::MAX).contains(number.as_ref()))
                {
                    self.declare_type(expr, Exact(declared_type_id))
                } else {
                    Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Float type overflow!",
                        type_id: declared_type_id,
                        span: expr.span.clone(),
                    })
                }
            }
            (number @ Integer(_) | number @ Float(_), NeedsInfer(type_bounds))
                if type_bounds.borrow().can_be_casted_to_integer() =>
            {
                self.declare_type(expr, ExactLiteral(number))
            }
            (number @ Float(_), NeedsInfer(type_bounds))
                if type_bounds.borrow().can_be_casted_to_number() =>
            {
                self.declare_type(expr, ExactLiteral(number))
            }
            (Integer(_) | Float(_), NeedsInfer(_)) => Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Expected integer type",
                type_id: TypeId(0),
                span: expr.span.clone(),
            }),
            (Boolean(true), Exact(declared_type_id))
                if declared_type_id == EMBEDDED_TYPES.literal_false =>
            {
                Err(TypeError {
                    kind: TypeErrorKind::TypeMismatch,
                    message: "No means no!",
                    type_id: declared_type_id,
                    span: expr.span.clone(),
                })
            }
            (Boolean(false), Exact(declared_type_id))
                if declared_type_id == EMBEDDED_TYPES.literal_true =>
            {
                Err(TypeError {
                    kind: TypeErrorKind::TypeMismatch,
                    message: "Type is literally `true` and you give an `false`",
                    type_id: declared_type_id,
                    span: expr.span.clone(),
                })
            }
            (Boolean(_), Exact(declared_type_id))
                if EMBEDDED_TYPES.is_boolean(declared_type_id) =>
            {
                self.declare_type(expr, Exact(declared_type_id))
            }
            (Boolean(_), Exact(declared_type_id)) => Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Expected boolean type",
                type_id: declared_type_id,
                span: expr.span.clone(),
            }),
            (Boolean(value), NeedsInfer(type_bounds))
                if type_bounds.borrow().can_be_casted_to_boolean() && value =>
            {
                self.declare_type(expr, Exact(EMBEDDED_TYPES.literal_true))
            }
            (Boolean(_), NeedsInfer(type_bounds))
                if type_bounds.borrow().can_be_casted_to_boolean() =>
            {
                self.declare_type(expr, Exact(EMBEDDED_TYPES.literal_false))
            }
            (Boolean(_), NeedsInfer(_)) => Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Expected boolean type",
                type_id: TypeId(0),
                span: expr.span.clone(),
            }),
            (Char(_), Exact(declared_type_id)) if EMBEDDED_TYPES.char == declared_type_id => {
                self.declare_type(expr, Exact(declared_type_id))
            }
            (Char(value), Exact(declared_type_id)) => {
                let type_def = self
                    .hierarchy
                    .types
                    .get(&declared_type_id)
                    .expect("The type must exist in this point!");

                match type_def {
                    TypeDefinition::Literal {
                        node: syntax::Literal::Char(char_from_node),
                        ..
                    } if *char_from_node == value => {
                        self.declare_type(expr, Exact(declared_type_id))
                    }
                    _ => Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Expected char type",
                        type_id: declared_type_id,
                        span: expr.span.clone(),
                    }),
                }
            }
            (char @ Char(_), NeedsInfer(type_bounds))
                if type_bounds.borrow().can_be_casted_to_char() =>
            {
                self.declare_type(expr, ExactLiteral(char))
            }
            (Char(_), NeedsInfer(_)) => Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Expected char type",
                type_id: TypeId(0),
                span: expr.span.clone(),
            }),
            (Str(_), Exact(declared_type_id)) if EMBEDDED_TYPES.string == declared_type_id => {
                self.declare_type(expr, Exact(declared_type_id))
            }
            (Str(value), Exact(declared_type_id)) => {
                let type_def = self
                    .hierarchy
                    .types
                    .get(&declared_type_id)
                    .expect("The type must exist in this point!");

                match type_def {
                    TypeDefinition::Literal {
                        node: syntax::Literal::Str(string_from_node),
                        ..
                    } if string_from_node == &value => {
                        self.declare_type(expr, Exact(declared_type_id))
                    }
                    _ => Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Expected string type",
                        type_id: declared_type_id,
                        span: expr.span.clone(),
                    }),
                }
            }
            (str @ Str(_), NeedsInfer(type_bounds))
                if type_bounds.borrow().can_be_casted_to_string() =>
            {
                self.declare_type(expr, ExactLiteral(str))
            }
            (Str(_), NeedsInfer(_)) => Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Expected string type",
                type_id: TypeId(0),
                span: expr.span.clone(),
            }),
        }
    }

    fn declare_similarity(
        &mut self,
        first_expr: &syntax::Expression,
        second_expr: &syntax::Expression,
    ) -> TypeResult<()> {
        let first_root = self.find_root(ExprId(first_expr.id));
        let second_root = self.find_root(ExprId(second_expr.id));

        if let (Some(first_exact_type), Some(second_exact_type)) = (
            self.lookup_for_type_of_root(first_root).as_exact(),
            self.lookup_for_type_of_root(second_root).as_exact(),
        ) && !(first_exact_type != second_exact_type
            || self
                .hierarchy
                .is_supertype(first_exact_type, second_exact_type)
            || self
                .hierarchy
                .is_supertype(second_exact_type, first_exact_type))
        {
            return Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Types are not compatible",
                type_id: first_exact_type,
                span: first_expr.span.clone(),
            });
        }

        let first_rank = self.rank.get(&first_root).unwrap();
        let second_rank = self.rank.get(&second_root).unwrap();

        let (new_root, old_root) = {
            use std::cmp::Ordering::*;

            match first_rank.cmp(second_rank) {
                Greater => (first_root, second_root),
                Equal => {
                    self.rank.insert(first_root, first_rank + 1);
                    (first_root, second_root)
                }
                Less => (second_root, first_root),
            }
        };

        self.expr_to_parent.insert(old_root, new_root);

        {
            use TypeConstraint::*;
            match (
                self.lookup_for_type_of_root(first_root).clone(),
                self.lookup_for_type_of_root(second_root).clone(),
            ) {
                (Exact(exact_type_id), NeedsInfer(type_bounds))
                | (NeedsInfer(type_bounds), Exact(exact_type_id)) => {
                    type_bounds.borrow_mut().upper_bounds.insert(exact_type_id);
                    Ok(())
                }
                (Exact(_first_exact_type_id), Exact(_second_exact_type_id)) => Ok(()),
                _ => unimplemented!(),
            }
        }
    }

    fn declare_type(
        &mut self,
        expr: &syntax::Expression,
        declared_type: TypeConstraint,
    ) -> TypeResult<TypeConstraint> {
        use TypeConstraint::*;
        let expr_id = ExprId(expr.id);
        let root = self.find_root(expr_id);

        match (self.root_to_type.remove(&root), declared_type) {
            (Some(Exact(existing_type_id)), Exact(declared_type_id)) => {
                if existing_type_id == declared_type_id
                    || self
                        .hierarchy
                        .is_supertype(existing_type_id, declared_type_id)
                    || self
                        .hierarchy
                        .is_supertype(declared_type_id, existing_type_id)
                {
                    self.root_to_type.insert(root, existing_type_id.into());
                    Ok(Exact(declared_type_id))
                } else {
                    Err(TypeError {
                        kind: TypeErrorKind::UnexpectedType,
                        message: "Declared type is different than the existing type",
                        type_id: existing_type_id,
                        span: expr.span.clone(),
                    })
                }
            }
            (Some(ExactLiteral(literal)), declared_type)
            | (Some(declared_type), ExactLiteral(literal)) => {
                self.declare_literal(expr, literal, declared_type)
            }
            (
                Some(existing_function @ UnresolvedFunction { .. }),
                declared_function @ UnresolvedFunction { .. },
            ) if existing_function == declared_function => {
                self.root_to_type.insert(root, existing_function);
                Ok(declared_function)
            }
            (
                Some(existing_function @ UnresolvedFunction { .. }),
                declared_function @ UnresolvedFunction { .. },
            ) if existing_function == declared_function => {
                self.root_to_type.insert(root, existing_function);
                Ok(declared_function)
            }
            (Some(UnresolvedFunction { .. }), UnresolvedFunction { .. }) => Err(TypeError {
                kind: TypeErrorKind::UnexpectedType,
                message: "Declared function type is different than the existing function type",
                type_id: TypeId(0),
                span: expr.span.clone(),
            }),
            (Some(Exact(known_type_id)), unresolved_function @ UnresolvedFunction { .. })
            | (Some(unresolved_function @ UnresolvedFunction { .. }), Exact(known_type_id)) => {
                self.declare_function_type(unresolved_function, known_type_id, &expr.span)
            }
            (Some(NeedsInfer(type_bounds)), function @ UnresolvedFunction { .. })
            | (Some(function @ UnresolvedFunction { .. }), NeedsInfer(type_bounds))
                if type_bounds.borrow().can_be_casted_to_function() =>
            {
                self.root_to_type.insert(expr_id, function.clone());
                Ok(function)
            }
            (Some(NeedsInfer(_)), UnresolvedFunction { .. })
            | (Some(UnresolvedFunction { .. }), NeedsInfer(_)) => Err(TypeError {
                kind: TypeErrorKind::UnexpectedType,
                message: "Literal type can not be a function type",
                type_id: TypeId(0),
                span: expr.span.clone(),
            }),
            (Some(NeedsInfer(type_bounds)), Exact(known_type_id))
            | (Some(Exact(known_type_id)), NeedsInfer(type_bounds)) => {
                let type_bounds = type_bounds.borrow();
                let is_upper_bounds_empty = type_bounds.upper_bounds.is_empty();
                let is_lower_bounds_empty = type_bounds.lower_bounds.is_empty();

                if is_upper_bounds_empty && is_lower_bounds_empty {
                    // We allow casts integer to float, but not the otherwise
                    if EMBEDDED_TYPES.is_integer(known_type_id) && type_bounds.must_be_floating {
                        Err(TypeError {
                            kind: TypeErrorKind::UnexpectedType,
                            message: "You can not cast a float to integer",
                            type_id: known_type_id,
                            span: expr.span.clone(),
                        })
                    } else {
                        self.root_to_type.insert(root, Exact(known_type_id));
                        Ok(Exact(known_type_id))
                    }
                } else if is_upper_bounds_empty {
                    if type_bounds
                        .lower_bounds
                        .iter()
                        .copied()
                        .any(|lower_bound| self.hierarchy.is_supertype(known_type_id, lower_bound))
                    {
                        self.root_to_type.insert(root, Exact(known_type_id));
                        Ok(Exact(known_type_id))
                    } else {
                        Err(TypeError {
                            kind: TypeErrorKind::UnexpectedType,
                            message: "Declared type is not compatible with existing lower bounds",
                            type_id: known_type_id,
                            span: expr.span.clone(),
                        })
                    }
                } else if is_lower_bounds_empty {
                    if type_bounds
                        .upper_bounds
                        .iter()
                        .copied()
                        .any(|upper_bound| self.hierarchy.is_supertype(upper_bound, known_type_id))
                    {
                        self.root_to_type.insert(root, Exact(known_type_id));
                        Ok(Exact(known_type_id))
                    } else {
                        Err(TypeError {
                            kind: TypeErrorKind::UnexpectedType,
                            message: "Declared type is not compatible with existing upper bounds",
                            type_id: known_type_id,
                            span: expr.span.clone(),
                        })
                    }
                } else {
                    let is_compatible =
                        type_bounds.lower_bounds.iter().copied().any(|lower_bound| {
                            self.hierarchy.is_supertype(known_type_id, lower_bound)
                        }) && type_bounds.upper_bounds.iter().copied().any(|upper_bound| {
                            self.hierarchy.is_supertype(upper_bound, known_type_id)
                        });

                    if is_compatible {
                        self.root_to_type.insert(root, Exact(known_type_id));
                        Ok(Exact(known_type_id))
                    } else {
                        Err(TypeError {
                            kind: TypeErrorKind::UnexpectedType,
                            message: "Declared type is not compatible with existing bounds",
                            type_id: known_type_id,
                            span: expr.span.clone(),
                        })
                    }
                }
            }
            (Some(NeedsInfer(known_type_bounds)), NeedsInfer(declared_type_bounds)) => {
                known_type_bounds.borrow_mut().merge(declared_type_bounds);
                self.root_to_type
                    .insert(root, NeedsInfer(known_type_bounds.clone()));
                Ok(NeedsInfer(known_type_bounds))
            }
            (None, type_knowledge) => {
                self.root_to_type.insert(root, type_knowledge.clone());
                Ok(type_knowledge)
            }
        }
    }

    fn declare_function_type(
        &mut self,
        unresolved_function: TypeConstraint,
        function_type_id: TypeId,
        span: &scanner::Span,
    ) -> TypeResult<TypeConstraint> {
        let TypeConstraint::UnresolvedFunction {
            params: unresolved_params,
            return_type: unresolved_return_type,
        } = unresolved_function
        else {
            panic!("Unexpected parameters")
        };

        let Some(TypeDefinition::Function {
            id,
            params: existing_params,
            return_type: existing_return_type,
        }) = self.hierarchy.types.get(&function_type_id)
        else {
            return Err(TypeError {
                kind: TypeErrorKind::UnexpectedType,
                message: "Declared type is not a function type",
                type_id: function_type_id,
                span: span.clone(),
            });
        };

        use TypeConstraint::*;
        match unresolved_return_type.as_ref() {
            Exact(declared_return_type) if declared_return_type == existing_return_type => {}
            Exact(declared_return_type) => {
                return Err(TypeError {
                    kind: TypeErrorKind::TypeMismatch,
                    message: "Return types does not match of functions",
                    type_id: *declared_return_type,
                    span: span.clone(),
                });
            }
            ExactLiteral(literal) => {
                let Some(TypeDefinition::Literal { node, .. }) =
                    self.hierarchy.types.get(existing_return_type)
                else {
                    return Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Return type of function is not a literal type",
                        type_id: *existing_return_type,
                        span: span.clone(),
                    });
                };

                if node != literal {
                    return Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch,
                        message: "Return literal types does not match of functions",
                        type_id: *existing_return_type,
                        span: span.clone(),
                    });
                }
            }
            UnresolvedFunction {
                params: declared_params,
                return_type: declared_result_type,
            } => {}
            NeedsInfer(type_bounds) => {}
        }

        unimplemented!()
    }

    fn declare_expression(&mut self, expr_id: ExprId) {
        use std::collections::hash_map::Entry::Vacant;

        if let Vacant(map) = self.expr_to_parent.entry(expr_id) {
            map.insert(expr_id);
            self.rank.insert(expr_id, 0);
        }
    }

    fn find_binding(
        &self,
        binding_name: &Rc<String>,
    ) -> (scope::BindingId, scope::ResolvedReferenceStatus) {
        let scope = self
            .hierarchy
            .scope_hierarchy
            .scopes
            .get(&self.scope_id)
            .expect("Scope must exist");
        scope
            .resolved_references
            .get(&binding_name.into())
            .copied()
            .expect("Binding must exist at this point")
    }

    fn extract_type_from_method(
        &mut self,
        expr_id: usize,
        method_name: &Rc<String>,
        arg_types: &[TypeConstraint],
        receiver_type_id: TypeId,
        span: &scanner::Span,
    ) -> TypeResult<TypeConstraint> {
        use TypeDefinition::*;

        let receiver_type = self
            .hierarchy
            .types
            .get(&receiver_type_id)
            .expect("Receiver type must exist");

        match receiver_type {
            Origin { id, node, .. } => {
                if let Some(methods) = node.methods.get(method_name) {
                    for method in methods {
                        let param_types: TypeResult<Vec<TypeConstraint>> = method
                            .params
                            .iter()
                            .map(|param| self.extract_type_declaration(&param.value))
                            .collect();
                        let param_types = param_types?;

                        if param_types == arg_types {
                            return self.extract_type_declaration(
                                method
                                    .return_type
                                    .as_ref()
                                    .expect("All methods required to have a return expression"),
                            );
                        }
                    }

                    Err(TypeError {
                        kind: TypeErrorKind::OverloadFailure,
                        message: "Does not match with any candidate",
                        type_id: receiver_type_id,
                        span: span.clone(),
                    })
                } else {
                    Err(TypeError {
                        kind: TypeErrorKind::UnboundSymbol,
                        message: "Method not found in the class",
                        type_id: *id,
                        span: span.clone(),
                    })
                }
            }
            Variant { id, node, .. } => {
                let id = *id;
                let method_name: syntax::PatternName = method_name.into();
                let expr_id = scope::ExprId(expr_id);

                let scope_id = self.scope_id;
                self.scope_id = self
                    .hierarchy
                    .scope_hierarchy
                    .expr_to_scope_id
                    .get(&expr_id)
                    .copied()
                    .expect("Scope must exist");

                for syntax::Pattern { name, value, .. } in node.iter() {
                    if name == &method_name {
                        let result = self.extract_type_declaration(value);

                        self.scope_id = scope_id;
                        return result;
                    }
                }

                Err(TypeError {
                    kind: TypeErrorKind::UnboundSymbol,
                    message: "Method not found in the variant",
                    type_id: id,
                    span: span.clone(),
                })
            }
            TypeInstance { origin_id, .. } => {
                self.extract_type_from_method(expr_id, method_name, arg_types, *origin_id, span)
            }
            _ => Err(TypeError {
                kind: TypeErrorKind::InvalidMethodCall,
                message: "Invalid method call type",
                type_id: receiver_type.id(),
                span: span.clone(),
            }),
        }
    }
}

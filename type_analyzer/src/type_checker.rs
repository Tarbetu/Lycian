use crate::definition::TypeDefinition;
use crate::error::{TypeError, TypeErrorKind, TypeResult};
use crate::hierarchy::EMBEDDED_TYPES;
use crate::type_bounds::{self, *};
use crate::{hierarchy::Hierarchy, TypeId};
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
    root_to_type: HashMap<ExprId, TypeInfo>,

    // Union by rank
    // Root to type
    rank: HashMap<ExprId, usize>,

    // For optimization
    empty_type_info: TypeInfo,

    // Instead of stopping at the first concrete type error, just collect them
    early_errors: Vec<TypeError>,

    // Collecting the overload types
    method_overload_types: HashMap<Rc<String>, HashSet<TypeId>>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(hierarchy: Hierarchy<'a>) -> Self {
        Self {
            hierarchy,
            errors: Vec::new(),
            expr_to_parent: HashMap::new(),
            root_to_type: HashMap::new(),
            rank: HashMap::new(),
            empty_type_info: TypeInfo::default(),
            early_errors: Vec::new(),
            method_overload_types: HashMap::new(),
        }
    }

    pub fn traverse(&mut self) -> TypeResult<()> {
        self.traverse_classes(0)
    }

    pub fn get_type(&mut self, expr: &syntax::Expression) -> &TypeInfo {
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
            self.traverse_class(scope_id)?;
        }

        self.traverse_classes(class_scope_index + 1)
    }

    fn traverse_class(&mut self, class_scope_id: scope::ScopeId) -> TypeResult<()> {
        let class_scope = self
            .hierarchy
            .scope_hierarchy
            .scopes
            .remove(&class_scope_id)
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
                    let TypeInfo::Exact(type_id) = self.extract_type_declaration(
                        class_scope_id,
                        method
                            .return_type
                            .as_ref()
                            .expect("Every method definition required to include a return type"),
                    )?
                    else {
                        unreachable!()
                    };

                    if let Err(err) = self.check(&method.body, TypeInfo::Exact(type_id)) {
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
            .insert(class_scope_id, class_scope);

        Ok(())
    }

    fn synthesize(
        &mut self,
        scope_id: scope::ScopeId,
        expr: &'a syntax::Expression,
    ) -> TypeResult<TypeInfo> {
        use syntax::ExpressionKind::*;
        use TypeInfo::*;

        match expr.kind.as_ref() {
            Grouping(inner_expr) => self.synthesize(scope_id, inner_expr),
            Literal(literal) => {
                let literal_type_id = self.synthesize_literal_type(scope_id, literal)?;
                self.declare_constraints(expr, &literal_type_id)?;
                Ok(literal_type_id.into())
            }
            Binary(lhs, op, rhs) if op.is_logical() => {
                self.check(lhs, Exact(EMBEDDED_TYPES.boolean))?;
                self.check(rhs, Exact(EMBEDDED_TYPES.boolean))?;
                self.declare_constraints(expr, &Exact(EMBEDDED_TYPES.boolean))?;
                Ok(EMBEDDED_TYPES.boolean.into())
            }
            Binary(lhs, op, rhs) if op.is_comparison() => {
                let lhs_type = self.synthesize(scope_id, lhs)?;
                self.check(rhs, lhs_type)?;
                self.declare_constraints(expr, &Exact(EMBEDDED_TYPES.boolean))?;
                Ok(EMBEDDED_TYPES.boolean.into())
            }
            Binary(lhs, op, rhs) if op.is_arithmetic() => {
                let lhs_type = self.synthesize(scope_id, lhs)?;
                let constraints = self.check(rhs, lhs_type)?;
                self.declare_constraints(expr, &constraints)?;
                Ok(constraints)
            }
            Binary(..) => unreachable!(),
            Unary(syntax::Operator::Not, inner_expr) => {
                self.check(inner_expr, Exact(EMBEDDED_TYPES.boolean))?;
                self.declare_constraints(expr, &Exact(EMBEDDED_TYPES.boolean))?;
                Ok(EMBEDDED_TYPES.boolean.into())
            }
            Unary(syntax::Operator::Negate, inner_expr) => {
                let expected_type = TypeInfo::needs_infer(TypeBounds::default().numeric());
                let expr_type = self.check(inner_expr, expected_type)?;
                self.declare_constraints(expr, &expr_type)?;
                Ok(expr_type)
            }
            IndexOperator(container, indexer) => {
                let container_type = self.synthesize(scope_id, container)?;
                // Indexer always assumed as uIntSize!
                self.check(indexer, Exact(EMBEDDED_TYPES.uIntSize))?;

                let TypeInfo::NeedsInfer(type_bounds) = container_type else {
                    unreachable!("Exact types are not expected in synthesis mode!");
                };

                // Check for the clone later
                Ok(type_bounds
                    .borrow()
                    .type_arguments
                    .first()
                    .cloned()
                    .expect("Type arguments needed!"))
            }
            Call {
                caller,
                callee: None,
                args,
                block,
                ..
            } => {
                let block_type = if let Some(block) = block {
                    Some(self.synthesize(scope_id, block)?)
                } else {
                    None
                };

                let (binding_id, resolve_status) = self.find_binding(scope_id, caller);

                let arg_types: TypeResult<Vec<TypeInfo>> = args
                    .iter()
                    .map(|arg| self.synthesize(scope_id, arg))
                    .collect();
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
                                    Ok(TypeInfo::Exact(origin_type_id))
                                }
                            }
                            Constructor(
                                syntax::Class {
                                    name: class_name, ..
                                },
                                (name, params),
                            ) => {
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
                                    .get(name)
                                    .copied()
                                    .expect("Variant must exist!");

                                let param_types: TypeResult<Vec<TypeInfo>> = params
                                    .iter()
                                    .map(|param| {
                                        self.extract_type_declaration(scope_id, &param.value)
                                    })
                                    .collect();
                                let param_types = param_types?;

                                if arg_types == param_types {
                                    Ok(TypeInfo::Exact(variant_id))
                                } else if arg_types.len() != param_types.len() {
                                    Err(TypeError {
                                        kind: TypeErrorKind::ParameterFailure,
                                        message: "Argument does not have same number of parameters",
                                        type_id: variant_id,
                                        span: expr.span.clone(),
                                    })
                                } else {
                                    Err(TypeError {
                                        kind: TypeErrorKind::ParameterFailure,
                                        message: "Argument types do not match parameter types",
                                        type_id: variant_id,
                                        span: expr.span.clone(),
                                    })
                                }
                            }
                            Function(function) => {
                                let param_types: TypeResult<Vec<TypeInfo>> = function
                                    .params
                                    .iter()
                                    .map(|param| {
                                        self.extract_type_declaration(scope_id, &param.value)
                                    })
                                    .collect();
                                let param_types = param_types?;

                                if arg_types == param_types {
                                    if let Some(return_type_expr) = function.return_type.as_ref() {
                                        let return_type = self
                                            .extract_type_declaration(scope_id, return_type_expr)?;

                                        Ok(return_type)
                                    } else {
                                        let mut type_bounds = TypeBounds::default().callable();

                                        if block.is_some() {
                                            type_bounds = type_bounds.accepts_block();
                                        }

                                        Ok(TypeInfo::needs_infer(type_bounds))
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

                                let pattern_type =
                                    self.extract_type_declaration(scope_id, value)?;

                                if let Exact(pattern_type) = pattern_type && pattern_type == EMBEDDED_TYPES.function {
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

                        if overloads.len() == 1 {
                            unimplemented!()
                        } else if arg_types
                            .iter()
                            .all(|arg_type| arg_type.as_exact().is_some())
                        {
                            let mut overload_selection = None;
                            for method in overloads {
                                let param_types: TypeResult<Vec<TypeInfo>> = method
                                    .params
                                    .iter()
                                    .map(|param| {
                                        self.extract_type_declaration(scope_id, &param.value)
                                    })
                                    .collect();
                                let param_types = param_types?;

                                if param_types == arg_types {
                                    if overload_selection.is_none() {
                                        overload_selection = Some(self.extract_type_declaration(
                                                    scope_id,
                                                    method.return_type.as_ref().expect("Every method required to have return type declaration"),
                                                )?);
                                    } else {
                                        return Err(TypeError {
                                            kind: TypeErrorKind::OverloadFailure,
                                            message: "Ambiguous overload candidate",
                                            type_id: TypeId(0),
                                            span: expr.span.clone(),
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
                                    span: expr.span.clone(),
                                })
                            }
                        } else {
                            unimplemented!()
                        }
                    }
                    _ => todo!(),
                }
            }
            Call {
                caller,
                callee: Some(receiver),
                ..
            } => {
                // Check method call
                let receiver_ty = self.synthesize(scope_id, receiver)?;
                unimplemented!()
            }
            _ => unimplemented!(),
        }
    }

    fn check(
        &mut self,
        expr: &syntax::Expression,
        expected_type: TypeInfo,
    ) -> TypeResult<TypeInfo> {
        unimplemented!()
    }

    fn extract_type_declaration(
        &mut self,
        scope_id: scope::ScopeId,
        expr: &'a syntax::Expression,
    ) -> TypeResult<TypeInfo> {
        use syntax::ExpressionKind::*;
        use TypeInfo::Exact;

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

                Ok(Exact(variant_id))
            }
            Literal(literal) => Ok(self.synthesize_literal_type(scope_id, literal))?,
            Grouping(inner_expr) => self.extract_type_declaration(scope_id, &inner_expr),
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

    fn synthesize_literal_type(
        &mut self,
        scope_id: scope::ScopeId,
        literal: &'a syntax::Literal,
    ) -> TypeResult<TypeInfo> {
        use syntax::Literal::*;
        use TypeInfo::*;

        match literal {
            Integer(_) => Ok(TypeInfo::needs_infer(TypeBounds::default().integer())),
            Float(_) => Ok(TypeInfo::needs_infer(TypeBounds::default().floating())),
            Boolean(true) => Ok(Exact(EMBEDDED_TYPES.literal_true)),
            Boolean(false) => Ok(Exact(EMBEDDED_TYPES.literal_false)),
            Char(_) => match self.hierarchy.literal_types.chars.get(literal).copied() {
                Some(type_id) => Ok(Exact(type_id)),
                None => {
                    let type_id = self.hierarchy.next_id();
                    let type_def = TypeDefinition::Literal {
                        id: type_id,
                        origin_id: EMBEDDED_TYPES.char,
                        node: literal,
                    };
                    self.hierarchy.types.insert(type_id, type_def);
                    Ok(Exact(type_id))
                }
            },
            Str(_) => match self.hierarchy.literal_types.strings.get(literal).copied() {
                Some(type_id) => Ok(Exact(type_id)),
                None => {
                    let type_id = self.hierarchy.next_id();
                    let type_def = TypeDefinition::Literal {
                        id: type_id,
                        origin_id: EMBEDDED_TYPES.string,
                        node: literal,
                    };
                    self.hierarchy.types.insert(type_id, type_def);
                    Ok(Exact(type_id))
                }
            },
            LiteralList(expr_list) => {
                if let Some(expr) = expr_list.front() {
                    let type_constraints = {
                        let mut type_constraints = self.synthesize(scope_id, expr)?;

                        for expr in expr_list.iter().skip(1) {
                            type_constraints = self.check(expr, type_constraints)?;
                        }

                        type_constraints
                    };

                    Ok(TypeInfo::needs_infer(
                        TypeBounds::default().list().with_argument(type_constraints),
                    ))
                } else {
                    Ok(TypeInfo::needs_infer(TypeBounds::default().list()))
                }
            }
            LiteralArray(expr_array) => {
                if let Some(expr) = expr_array.get(0) {
                    let type_constraints = {
                        let mut type_constraints = self.synthesize(scope_id, expr)?;

                        for expr in expr_array.iter().skip(1) {
                            type_constraints = self.check(expr, type_constraints)?;
                        }

                        type_constraints
                    };

                    Ok(TypeInfo::needs_infer(
                        TypeBounds::default()
                            .array()
                            .with_argument(type_constraints),
                    ))
                } else {
                    Ok(TypeInfo::needs_infer(TypeBounds::default().array()))
                }
            }
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

    fn lookup_for_type_of_root(&mut self, root_expr_id: ExprId) -> &TypeInfo {
        self.root_to_type
            .get(&root_expr_id)
            .unwrap_or(&self.empty_type_info)
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
        ) {
            if !(first_exact_type != second_exact_type
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
            use TypeInfo::*;
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

    fn declare_constraints(
        &mut self,
        expr: &syntax::Expression,
        constraints: &TypeInfo,
    ) -> TypeResult<()> {
        // use TypeInfo::*;
        // let expr_id = ExprId(expr.id);
        // let root = self.find_root(expr_id);

        // match self.root_to_type.get(&root) {
        //     Some(Exact(existing_type))
        //         if *existing_type == inferred_type_id
        //             || self
        //                 .hierarchy
        //                 .is_supertype(*existing_type, inferred_type_id) =>
        //     {
        //         Ok(())
        //     }
        //     Some(Exact(existing_type)) => Err(TypeError {
        //         kind: TypeErrorKind::TypeMismatch,
        //         message: "Infered type is not compatible with declared type",
        //         type_id: *existing_type,
        //         span: expr.span.clone(),
        //     }),
        //     Some(NeedsInfer(type_bounds)) => {
        //         type_bounds
        //             .borrow_mut()
        //             .lower_bounds
        //             .insert(inferred_type_id);

        //         Ok(())
        //     }
        //     None => {
        //         // self.root_to_type.insert(expr_id, v);
        //         unimplemented!()
        //         // self.root_to_type.insert(expr_id, TypeInfo::Exact(inf_id));
        //         // Ok(())
        //     }
        // }

        unimplemented!()
    }

    fn declare_type(
        &mut self,
        expr: &syntax::Expression,
        declared_type_id: TypeId,
    ) -> TypeResult<()> {
        use TypeInfo::*;
        let expr_id = ExprId(expr.id);
        let root = self.find_root(expr_id);

        match self.root_to_type.remove(&root) {
            Some(Exact(existing_type)) if existing_type == declared_type_id => {
                self.root_to_type.insert(root, Exact(existing_type));
                Ok(())
            }
            Some(Exact(existing_type)) => Err(TypeError {
                kind: TypeErrorKind::UnexpectedType,
                message: "Declared type is different than the existing type",
                type_id: existing_type,
                span: expr.span.clone(),
            }),
            Some(NeedsInfer(type_bounds)) => {
                let type_bounds = type_bounds.borrow();
                let is_upper_bounds_empty = type_bounds.upper_bounds.is_empty();
                let is_lower_bounds_empty = type_bounds.lower_bounds.is_empty();

                if is_upper_bounds_empty && is_lower_bounds_empty {
                    // We allow casts integer to float, but not the otherwise
                    if EMBEDDED_TYPES.is_integer(declared_type_id) && type_bounds.must_be_floating {
                        Err(TypeError {
                            kind: TypeErrorKind::UnexpectedType,
                            message: "You can not cast a float to integer",
                            type_id: declared_type_id,
                            span: expr.span.clone(),
                        })
                    } else {
                        self.root_to_type.insert(expr_id, Exact(declared_type_id));
                        Ok(())
                    }
                } else if is_upper_bounds_empty {
                    if type_bounds.lower_bounds.iter().copied().any(|lower_bound| {
                        self.hierarchy.is_supertype(declared_type_id, lower_bound)
                    }) {
                        self.root_to_type.insert(expr_id, Exact(declared_type_id));
                        Ok(())
                    } else {
                        Err(TypeError {
                            kind: TypeErrorKind::UnexpectedType,
                            message: "Declared type is not compatible with existing lower bounds",
                            type_id: declared_type_id,
                            span: expr.span.clone(),
                        })
                    }
                } else if is_lower_bounds_empty {
                    if type_bounds.upper_bounds.iter().copied().any(|upper_bound| {
                        self.hierarchy.is_supertype(upper_bound, declared_type_id)
                    }) {
                        self.root_to_type.insert(expr_id, Exact(declared_type_id));
                        Ok(())
                    } else {
                        Err(TypeError {
                            kind: TypeErrorKind::UnexpectedType,
                            message: "Declared type is not compatible with existing upper bounds",
                            type_id: declared_type_id,
                            span: expr.span.clone(),
                        })
                    }
                } else {
                    let is_compatible =
                        type_bounds.lower_bounds.iter().copied().any(|lower_bound| {
                            self.hierarchy.is_supertype(declared_type_id, lower_bound)
                        }) && type_bounds.upper_bounds.iter().copied().any(|upper_bound| {
                            self.hierarchy.is_supertype(upper_bound, declared_type_id)
                        });

                    if is_compatible {
                        self.root_to_type.insert(expr_id, Exact(declared_type_id));
                        Ok(())
                    } else {
                        Err(TypeError {
                            kind: TypeErrorKind::UnexpectedType,
                            message: "Declared type is not compatible with existing bounds",
                            type_id: declared_type_id,
                            span: expr.span.clone(),
                        })
                    }
                }
            }
            None => {
                self.root_to_type.insert(expr_id, Exact(declared_type_id));
                Ok(())
            }
        }
    }

    fn declare_expression(&mut self, expr_id: ExprId) {
        if !self.expr_to_parent.contains_key(&expr_id) {
            self.expr_to_parent.insert(expr_id, expr_id);
            self.rank.insert(expr_id, 0);
        }
    }

    fn find_binding(
        &self,
        scope_id: scope::ScopeId,
        binding_name: &Rc<String>,
    ) -> (scope::BindingId, scope::ResolvedReferenceStatus) {
        let scope = self
            .hierarchy
            .scope_hierarchy
            .scopes
            .get(&scope_id)
            .expect("Scope must exist");
        scope
            .resolved_references
            .get(&binding_name.into())
            .copied()
            .expect("Binding must exist")
    }
}

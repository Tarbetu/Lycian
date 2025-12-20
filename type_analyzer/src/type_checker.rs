use crate::definition::TypeDefinition;
use crate::error::{TypeError, TypeErrorKind, TypeResult};
use crate::hierarchy::EMBEDDED_TYPES;
use crate::type_bounds::*;
use crate::{hierarchy::Hierarchy, TypeId};
use scope::ExprId;
use scope::SyntaxNode;
use std::collections::HashMap;
use std::collections::HashSet;

pub(crate) struct TypeChecker<'a> {
    pub hierarchy: Hierarchy<'a>,
    pub errors: Vec<TypeError>,

    // Every expression points to it's parents (Roots don't have any parent)
    expr_to_parent: HashMap<ExprId, ParentStatus>,

    // Roots expressions to type
    root_to_type: HashMap<ExprId, TypeInfo>,

    // Union by rank
    // Root to type
    rank: HashMap<ExprId, usize>,

    // For optimization
    empty_type_info: TypeInfo,

    // Instead of stopping at the first concrete type error, just collect them
    early_errors: Vec<TypeError>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum ParentStatus {
    HasParent(ExprId),
    Root,
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
            expr_to_parent: HashMap::new(),
            root_to_type: HashMap::new(),
            rank: HashMap::new(),
            empty_type_info: TypeInfo::default(),
            early_errors: Vec::new(),
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
                self.declare_expr_type(expr, literal_type_id)?;
                Ok(literal_type_id)
            }
            Binary(lhs, op, rhs) if op.is_logical() => {
                self.check(lhs, Exactly(EMBEDDED_TYPES.boolean))?;
                self.check(rhs, Exactly(EMBEDDED_TYPES.boolean))?;
                self.declare_expr_type(expr, EMBEDDED_TYPES.boolean)?;
                Ok(EMBEDDED_TYPES.boolean)
            }
            Binary(lhs, op, rhs) if op.is_comparison() => {
                let lhs_type = self.synthesize(lhs)?;
                self.check(rhs, Exactly(lhs_type))?;
                self.declare_expr_type(expr, EMBEDDED_TYPES.boolean)?;
                Ok(EMBEDDED_TYPES.boolean)
            }
            Binary(lhs, op, rhs) if op.is_arithmetic() => {
                let lhs_type = self.synthesize(lhs)?;
                self.check(rhs, Exactly(lhs_type))?;
                self.declare_expr_type(expr, lhs_type)?;
                Ok(lhs_type)
            }
            Binary(..) => unreachable!(),
            Unary(syntax::Operator::Not, inner_expr) => {
                self.check(inner_expr, Exactly(EMBEDDED_TYPES.boolean))?;
                self.declare_expr_type(expr, EMBEDDED_TYPES.boolean)?;
                Ok(EMBEDDED_TYPES.boolean)
            }
            Unary(syntax::Operator::Negate, inner_expr) => {
                let expr_type = self.check(inner_expr, Numeric)?;
                self.declare_expr_type(expr, expr_type)?;
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

    fn find_root(&mut self, expr_id: ExprId) -> ExprId {
        self.declare_expression(expr_id);

        let parent = self.expr_to_parent.get(&expr_id).copied().unwrap();

        if let ParentStatus::HasParent(parent_id) = parent {
            let root = self.find_root(parent_id);
            self.expr_to_parent
                .insert(expr_id, ParentStatus::HasParent(parent_id));
            root
        } else {
            expr_id
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
            if !(self
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

        self.expr_to_parent
            .insert(old_root, ParentStatus::HasParent(new_root));

        {
            use TypeInfo::*;
            match (
                self.lookup_for_type_of_root(first_root).clone(),
                self.lookup_for_type_of_root(second_root).clone(),
            ) {
                (Exact(exact_type_id), NeedsInfer(type_bounds))
                | (NeedsInfer(type_bounds), Exact(exact_type_id)) => {
                    type_bounds.borrow_mut().can_be_cast_to.push(exact_type_id);
                    Ok(())
                }
                (Exact(first_exact_type_id), Exact(second_exact_type_id)) => Ok(()),
                _ => unimplemented!(),
            }
        }
    }

    fn declare_expr_type(&mut self, expr: &syntax::Expression, type_id: TypeId) -> TypeResult<()> {
        let expr_id = ExprId(expr.id);
        let root = self.find_root(expr_id);

        // match self.root_to_type.get(&root) {
        //     Some(TypeInfo::Exact(existing_type)) if *existing_type == type_id => Ok(()),
        //     Some(TypeInfo::Exact(existing_type)) => Err(TypeError {
        //         kind: TypeErrorKind::TypeMismatch,
        //         message: "Type already set to different value",
        //         type_id: *existing_type,
        //         span: expr.span.clone(),
        //     }),
        //     None => {
        //         self.root_to_type.insert(expr_id, TypeInfo::Exact(type_id));
        //         Ok(())
        //     }
        // }
        unimplemented!()
    }

    fn declare_expression(&mut self, expr_id: ExprId) {
        if !self.expr_to_parent.contains_key(&expr_id) {
            self.expr_to_parent
                .insert(expr_id, ParentStatus::HasParent(expr_id));
            self.rank.insert(expr_id, 0);
        }
    }
}

use syntax::{Function, Pattern};

use crate::{ExprId, ScopeId};
use crate::{Scope, SyntaxNode};
use ahash::AHashMap;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Hierarchy<'a> {
    pub scopes: HashMap<ScopeId, Scope<'a>>,
    pub last_id: ScopeId,
    pub expr_to_scope_id: HashMap<ExprId, ScopeId>,
    pub delayed_scopes: DelayedScopes,
}

#[derive(Default)]
pub struct DelayedScopes {
    maybe_inherited: Vec<ScopeId>,
    check_overload: Vec<ScopeId>,
    type_mismatch: Vec<ScopeId>,
}

pub const ROOT_ID: ScopeId = ScopeId(0);

impl Default for Hierarchy<'_> {
    fn default() -> Self {
        Hierarchy {
            scopes: HashMap::from([(ROOT_ID, Scope::root())]),
            last_id: ScopeId(1),
            expr_to_scope_id: HashMap::new(),
            delayed_scopes: DelayedScopes::default(),
        }
    }
}

impl<'a> Hierarchy<'a> {
    pub(crate) fn root(&'a self) -> &'a Scope<'a> {
        self.scopes.get(&ROOT_ID).unwrap()
    }

    pub(crate) fn build(mut self, classes: &'a [syntax::Class]) -> Self {
        let mut root = self.scopes.remove(&ROOT_ID).unwrap();

        for class in classes {
            let class_scope_id = self.next_id();

            root.children_ids.push(class_scope_id);

            self.scopes.insert(
                class_scope_id,
                Scope {
                    id: class_scope_id,
                    parent_id: ROOT_ID,
                    node: SyntaxNode::Class(class),
                    ..Scope::default()
                },
            );

            self.build_constructors(&class.constructors, class_scope_id);
            self.build_methods(&class.methods, class_scope_id);
        }

        self.scopes.insert(ROOT_ID, root);
        self
    }

    fn build_constructors(
        &mut self,
        constructors: &'a [(Rc<String>, Vec<Pattern>)],
        class_scope_id: ScopeId,
    ) {
        for constructor in constructors {
            let constructor_id = self.next_id();

            self.scopes.insert(
                constructor_id,
                Scope {
                    id: constructor_id,
                    parent_id: class_scope_id,
                    node: SyntaxNode::Constructor(constructor),
                    ..Scope::default()
                },
            );
            self.push_children(class_scope_id, constructor_id);
        }
    }

    fn build_methods(
        &mut self,
        methods: &'a AHashMap<Rc<String>, Vec<Function>>,
        class_scope_id: ScopeId,
    ) {
        for (_method_name, method_overloads) in methods {
            for method_overload in method_overloads {
                let method_id = self.next_id();

                self.scopes.insert(
                    method_id,
                    Scope {
                        id: method_id,
                        parent_id: class_scope_id,
                        node: SyntaxNode::Function(method_overload),
                        ..Scope::default()
                    },
                );

                self.push_children(class_scope_id, method_id);

                self.build_expression(&method_overload.body, method_id)
            }
        }
    }

    fn build_expression(&mut self, expression: &'a syntax::Expression, parent_id: ScopeId) {
        use syntax::ExpressionKind::*;
        match expression.kind.as_ref() {
            Function(function) => {
                let function_id = self.next_id();
                self.push_expr_id(expression.id, function_id);

                self.scopes.insert(
                    parent_id,
                    Scope {
                        id: function_id,
                        parent_id,
                        node: SyntaxNode::Function(function),
                        ..Scope::default()
                    },
                );

                self.push_children(parent_id, function_id);
                self.build_expression(&function.body, function_id);
            }
            Block {
                expressions,
                value,
                params: _params,
            } => {
                let block_id = self.next_id();
                self.push_expr_id(expression.id, block_id);

                self.scopes.insert(
                    parent_id,
                    Scope {
                        id: block_id,
                        parent_id,
                        node: SyntaxNode::Expression(expression),
                        ..Scope::default()
                    },
                );

                self.push_children(parent_id, block_id);
                for expression in expressions {
                    self.build_expression(expression, block_id);
                }

                self.build_expression(value, block_id);
            }
            Match { scrutinee, arms } => {
                self.build_expression(scrutinee, parent_id);

                for (_pattern, expr) in arms {
                    self.build_expression(expr, parent_id)
                }
            }
            Binary(left, _, right) => {
                self.build_expression(left, parent_id);
                self.build_expression(right, parent_id);
            }
            Unary(_, expr) => self.build_expression(expr, parent_id),
            Grouping(expr) => self.build_expression(expr, parent_id),
            IndexOperator(container, index) => {
                self.build_expression(container, parent_id);
                self.build_expression(index, parent_id);
            }
            Call {
                callee,
                block,
                args,
                ..
            } => {
                if let Some(callee) = callee.as_ref() {
                    self.build_expression(callee, parent_id);
                }
                if let Some(block) = block.as_ref() {
                    self.build_expression(block, parent_id);
                }
                for expr in args {
                    self.build_expression(expr, parent_id)
                }
            }
            _ => {}
        }
    }

    fn push_expr_id(&mut self, expr_id: usize, scope_id: ScopeId) {
        self.expr_to_scope_id.insert(ExprId(expr_id), scope_id);
    }

    fn push_children(&mut self, parent_id: ScopeId, children_id: ScopeId) {
        self.scopes
            .get_mut(&parent_id)
            .unwrap()
            .children_ids
            .push(children_id);
    }

    fn next_id(&mut self) -> ScopeId {
        let id = self.last_id;
        self.last_id = ScopeId(self.last_id.0 + 1);
        id
    }
}

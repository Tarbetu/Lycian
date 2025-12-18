use super::TypeInfo;
use crate::error::{TypeError, TypeErrorKind, TypeResult};
use crate::TypeId;
use scope::ExprId;
use std::collections::HashMap;

#[derive(Default)]
pub struct TypeUnion {
    // Every expression points to it's parents (Roots don't have any parent)
    expr_to_parent: HashMap<ExprId, ParentStatus>,

    // Roots expressions to type
    root_to_type: HashMap<ExprId, TypeInfo>,

    // Union by rank
    // Root to type
    rank: HashMap<ExprId, usize>,

    // For optimization
    empty_type_info: TypeInfo,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum ParentStatus {
    HasParent(ExprId),
    Root,
}

impl TypeUnion {
    pub fn lookup_for_type(&mut self, expr_id: ExprId) -> &TypeInfo {
        let root = self.find_root(expr_id);
        self.lookup_for_type_of_root(root)
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

    pub fn declare_similarity(
        &mut self,
        first_expr: &syntax::Expression,
        second_expr: &syntax::Expression,
    ) -> TypeResult<()> {
        let first_root = self.find_root(ExprId(first_expr.id));
        let second_root = self.find_root(ExprId(second_expr.id));

        if first_root == second_root {
            return Ok(());
        }

        let first_type = self.lookup_for_type_of_root(first_root);
        let second_type = self.lookup_for_type_of_root(second_root);

        if let (TypeInfo::Exact(first_type), TypeInfo::Exact(second_type)) =
            (first_type, second_type)
        {
            if first_type != second_type {
                return Err(TypeError {
                    kind: TypeErrorKind::TypeMismatch,
                    message: "Types are not same - Unification error",
                    type_id: *second_type,
                    span: second_expr.span.clone(),
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

        // if let Some(TypeInfo::Exact(known_type)) = first_type.or(second_type) {
        //     self.root_to_type.insert(new_root, known_type);
        // }

        Ok(())
    }

    pub fn declare_type(&mut self, expr: &syntax::Expression, type_id: TypeId) -> TypeResult<()> {
        let expr_id = ExprId(expr.id);
        let root = self.find_root(expr_id);

        match self.root_to_type.get(&root) {
            Some(TypeInfo::Exact(existing_type)) if *existing_type == type_id => Ok(()),
            Some(TypeInfo::Exact(existing_type)) => Err(TypeError {
                kind: TypeErrorKind::TypeMismatch,
                message: "Type already set to different value",
                type_id: *existing_type,
                span: expr.span.clone(),
            }),
            None => {
                self.root_to_type.insert(expr_id, TypeInfo::Exact(type_id));
                Ok(())
            }
        }
    }

    pub fn get_type(&mut self, expr: &syntax::Expression) -> Option<&TypeInfo> {
        let root = self.find_root(ExprId(expr.id));
        self.root_to_type.get(&root)
    }

    fn declare_expression(&mut self, expr_id: ExprId) {
        if !self.expr_to_parent.contains_key(&expr_id) {
            self.expr_to_parent.insert(expr_id, expr_id);
            self.rank.insert(expr_id, 0);
        }
    }
}

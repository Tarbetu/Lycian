use crate::binding::{Binding, BindingKind};
use crate::error::{ScopeError, ScopeErrorKind};
use crate::hierarchy::ROOT_ID;
use crate::scope::ResolvedReferenceStatus;
use crate::{ExprId, Hierarchy, Scope, ScopeId, ScopeResult, SyntaxNode};
use std::mem;
use std::rc::Rc;

pub fn resolve(mut hierarchy: Hierarchy) -> ScopeResult<Hierarchy> {
    let children_ids = mem::take(&mut hierarchy.scopes.get_mut(&ROOT_ID).unwrap().children_ids);
    for class_id in children_ids {
        resolve_class(&mut hierarchy, class_id)?
    }

    Ok(hierarchy)
}

fn resolve_class(hierarchy: &mut Hierarchy, scope_id: ScopeId) -> ScopeResult<()> {
    hierarchy
        .scopes
        .get_mut(&ROOT_ID)
        .unwrap()
        .children_ids
        .push(scope_id);
    let children_ids = mem::take(&mut hierarchy.scopes.get_mut(&scope_id).unwrap().children_ids);

    for declaration_id in children_ids {
        let declaration_scope = hierarchy.scopes.get(&declaration_id).unwrap();

        match declaration_scope.node {
            SyntaxNode::Function(_) => resolve_function(hierarchy, scope_id, declaration_id)?,
            SyntaxNode::Constructor(_) => resolve_constructor(hierarchy, scope_id, declaration_id)?,
            _ => unreachable!(),
        }
    }

    Ok(())
}

fn resolve_constructor(
    hierarchy: &mut Hierarchy<'_>,
    parent_id: ScopeId,
    declaration_id: ScopeId,
) -> ScopeResult<()> {
    hierarchy
        .scopes
        .get_mut(&parent_id)
        .unwrap()
        .children_ids
        .push(declaration_id);

    let SyntaxNode::Constructor((_, patterns)) =
        hierarchy.scopes.get(&declaration_id).unwrap().node
    else {
        panic!("Expected constructor in constructor")
    };
    resolve_patterns(hierarchy, parent_id, patterns)
}

fn resolve_function(
    hierarchy: &mut Hierarchy<'_>,
    parent_id: ScopeId,
    fn_scope_id: ScopeId,
) -> ScopeResult<()> {
    let fn_scope = hierarchy.scopes.get(&fn_scope_id).unwrap();

    let SyntaxNode::Function(function) = fn_scope.node else {
        panic!("Unexpected syntax node kind!")
    };
    resolve_patterns(hierarchy, parent_id, &function.params)?;

    // It smells! Needed to re-borrow the scope after mutable borrow.
    let fn_scope = hierarchy.scopes.get(&fn_scope_id).unwrap();

    let SyntaxNode::Function(syntax::Function {
        return_type: return_type_expr,
        body,
        ..
    }) = fn_scope.node
    else {
        panic!("Expected function in resolve_function!")
    };

    if let Some(expr) = return_type_expr {
        resolve_expression(hierarchy, fn_scope_id, expr)?;
    }

    resolve_expression(hierarchy, fn_scope_id, body)
}

fn resolve_expression(
    hierarchy: &mut Hierarchy<'_>,
    parent_id: ScopeId,
    expr: &syntax::Expression,
) -> ScopeResult<()> {
    use syntax::ExpressionKind::*;

    match expr.kind.as_ref() {
        Binary(left, _, right) => {
            resolve_expression(hierarchy, parent_id, left)?;
            resolve_expression(hierarchy, parent_id, right)
        }
        Unary(_, expr) => resolve_expression(hierarchy, parent_id, expr),
        Match { scrutinee, arms } => {
            resolve_expression(hierarchy, parent_id, scrutinee)?;
            for (pattern, expr) in arms.iter() {
                resolve_pattern(hierarchy, parent_id, pattern)?;
                resolve_expression(hierarchy, parent_id, expr)?;
            }
            Ok(())
        }
        Grouping(expr) => resolve_expression(hierarchy, parent_id, expr),
        Function(_) => {
            let fn_scope_id = hierarchy.expr_to_scope_id.get(&ExprId(expr.id)).unwrap();
            resolve_function(hierarchy, parent_id, *fn_scope_id)
        }
        Block {
            expressions,
            value,
            params,
        } => {
            resolve_patterns(hierarchy, parent_id, params)?;

            let scope_id = *hierarchy.expr_to_scope_id.get(&ExprId(expr.id)).unwrap();

            for expr in expressions {
                resolve_expression(hierarchy, scope_id, expr)?
            }

            resolve_expression(hierarchy, scope_id, value)
        }

        Call {
            caller,
            callee,
            args,
            block,
            ..
        } => {
            if let Some(expr) = block {
                resolve_expression(hierarchy, parent_id, expr)?
            }

            for arg in args {
                resolve_expression(hierarchy, parent_id, arg)?
            }

            if let Some(callee) = callee {
                let mut callee = callee;
                loop {
                    match callee.kind.as_ref() {
                        Call {
                            callee: Some(previous_callee),
                            ..
                        } => callee = previous_callee,
                        Call {
                            callee: None,
                            caller,
                            ..
                        } => {
                            add_resolved_name_to_scope(
                                hierarchy,
                                parent_id,
                                &syntax::PatternName::Name(caller.clone()),
                                ResolvedReferenceStatus::CheckMethodCall,
                            );
                            return Ok(());
                        }
                        _ => return Ok(()),
                    }
                }
            } else {
                let name = syntax::PatternName::Name(caller.clone());
                match search_name(hierarchy, parent_id, &name) {
                    Some((scope_id, status)) => {
                        add_resolved_name_to_scope(hierarchy, scope_id, &name, status);
                        Ok(())
                    }
                    None => Err(ScopeError {
                        kind: ScopeErrorKind::UnboundSymbol,
                        message: "Symbol does not found",
                        scope_id: parent_id,
                        span: expr.span.clone(),
                    })?,
                }
            }
        }
        IndexOperator(container_expr, index_expr) => {
            resolve_expression(hierarchy, parent_id, container_expr)?;
            resolve_expression(hierarchy, parent_id, index_expr)
        }
        _ => Ok(()),
    }
}

fn resolve_patterns(
    hierarchy: &mut Hierarchy<'_>,
    parent_id: ScopeId,
    patterns: &[syntax::Pattern],
) -> ScopeResult<()> {
    for pattern in patterns {
        resolve_pattern(hierarchy, parent_id, pattern)?;
    }

    Ok(())
}

fn resolve_pattern(
    hierarchy: &mut Hierarchy<'_>,
    parent_id: ScopeId,
    pattern: &syntax::Pattern,
) -> ScopeResult<()> {
    match pattern.value.kind.as_ref() {
        syntax::ExpressionKind::Call { callee, caller, .. } => {
            let (name, is_method) = extract_type_from_call(callee, caller, false, parent_id)?;
            match search_name(hierarchy, parent_id, &name) {
                Some((scope_id, ResolvedReferenceStatus::Ok)) => {
                    if is_method {
                        add_resolved_name_to_scope(
                            hierarchy,
                            parent_id,
                            &name,
                            ResolvedReferenceStatus::CheckMethodCall,
                        );
                        Ok(())
                    } else {
                        add_resolved_name_to_scope(
                            hierarchy,
                            scope_id,
                            &name,
                            ResolvedReferenceStatus::Ok,
                        );
                        Ok(())
                    }
                }
                Some((scope_id, status)) => {
                    add_resolved_name_to_scope(hierarchy, scope_id, &name, status);
                    Ok(())
                }
                None => {
                    let span = hierarchy.scopes.get(&parent_id).unwrap().node.span();

                    Err(ScopeError {
                        kind: ScopeErrorKind::UnboundSymbol,
                        message: "Symbol does not found",
                        scope_id: parent_id,
                        span,
                    })
                }
            }
        }
        _ => Ok(()),
    }
}

fn search_name(
    hierarchy: &Hierarchy<'_>,
    beginning_scope: ScopeId,
    name: &syntax::PatternName,
) -> Option<(ScopeId, ResolvedReferenceStatus)> {
    let beginning_scope = hierarchy.scopes.get(&beginning_scope).unwrap();

    let mut is_inherited = false;
    let mut current_scope = beginning_scope;

    loop {
        match current_scope.bindings.get(name) {
            Some(Binding {
                kind: BindingKind::Method,
                ..
            }) => return Some((current_scope.id, ResolvedReferenceStatus::CheckOverload)),
            Some(_) => return Some((current_scope.id, ResolvedReferenceStatus::Ok)),
            None => match hierarchy.scopes.get(&current_scope.parent_id) {
                Some(
                    scope @ Scope {
                        node: SyntaxNode::Class(syntax::Class { ancestors, .. }),
                        ..
                    },
                ) if !ancestors.is_empty() => {
                    is_inherited = true;
                    current_scope = scope
                }
                Some(scope) => current_scope = scope,
                None => {
                    return if is_inherited {
                        Some((beginning_scope.id, ResolvedReferenceStatus::MaybeInherited))
                    } else {
                        None
                    }
                }
            },
        }
    }
}

fn extract_type_from_call(
    callee: &Option<syntax::Expression>,
    caller: &Rc<String>,
    is_method: bool,
    scope_id: ScopeId,
) -> ScopeResult<(syntax::PatternName, bool)> {
    if let Some(expr) = callee {
        let syntax::ExpressionKind::Call { callee, caller, .. } = expr.kind.as_ref() else {
            return Err(ScopeError {
                kind: ScopeErrorKind::InvalidPatternValue,
                message: "Type annonation is invalid",
                span: expr.span.clone(),
                scope_id,
            });
        };

        extract_type_from_call(callee, caller, true, scope_id)
    } else {
        Ok((syntax::PatternName::Name(caller.clone()), is_method))
    }
}

fn add_resolved_name_to_scope(
    hierarchy: &mut Hierarchy<'_>,
    scope: ScopeId,
    name: &syntax::PatternName,
    status: ResolvedReferenceStatus,
) {
    let scope = hierarchy.scopes.get_mut(&scope).unwrap();
    if scope.resolved_references.contains_key(name) {
        return;
    }

    scope
        .resolved_references
        .insert(name.clone(), (scope.id, status));

    use ResolvedReferenceStatus::*;
    match status {
        Ok => {}
        CheckOverload => hierarchy.delayed_scopes.check_overload.push(scope.id),
        MaybeInherited => hierarchy.delayed_scopes.maybe_inherited.push(scope.id),
        CheckMethodCall => hierarchy.delayed_scopes.check_method_call.push(scope.id),
    }
}

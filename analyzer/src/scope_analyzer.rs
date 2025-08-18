mod resolution;
use crate::{
    resolution_error::{ResolutionErrorKind, ScopeError},
    Analyzer, ResolutionError, ResolutionResult,
};
use ahash::{HashMap, HashMapExt, HashSet, HashSetExt};
use rayon::prelude::*;
pub use resolution::*;
use std::sync::mpsc::{channel, Sender};

use parser::{
    Class, Entity, EntityIndex, EntityKind, EntityTable, Expression, Function, Pattern, PatternName,
};

#[derive(Debug, Copy, Clone)]
pub struct ScopeIndex(usize);

/// A node in the scope tree
#[derive(Debug)]
pub struct Scope {
    /// The entity this scope represents
    pub entity: EntityIndex,

    /// The parent scope's index (None for global scope)
    pub parent: Option<ScopeIndex>,

    /// Child scopes
    pub children: Vec<ScopeIndex>,

    /// Local declarations in this scope (name -> entity)
    pub declarations: HashMap<String, EntityIndex>,
}

/// The scope analyzer that resolves names and builds the scope hierarchy
pub struct ScopeAnalyzer {
    /// Entity table from the parser
    entity_table: EntityTable,

    /// Classes from the parser
    classes: HashMap<EntityIndex, Class>,

    /// The scope tree
    scopes: Vec<Scope>,

    /// Maps entity indices to their containing scope indices
    entity_to_scope: HashMap<EntityIndex, ScopeIndex>,

    /// Name resolutions for all entity references
    pub resolutions: HashMap<EntityIndex, Resolution>,

    /// Current scope being analyzed
    current_scope: ScopeIndex,

    /// Errors accumulated during analysis
    pub errors: Vec<ScopeError>,
    // Line number information for entities
    // entity_lines: HashMap<EntityIndex, usize>,
    // Method resolution order for each class (linearized inheritance)
    method_resolution_order: HashMap<EntityIndex, Vec<EntityIndex>>,
}

impl Analyzer for ScopeAnalyzer {
    type Output = HashMap<EntityIndex, Resolution>;
    /// Analyze scopes and resolve names
    fn analyze(mut self) -> ResolutionResult<Self::Output> {
        // Build inheritance graph
        // self.build_inheritance_graph();
        // self.collect_entity_lines();

        // Calculate method resolution order for all classes
        // self.calculate_mro();

        // Build the scope hierarchy
        self.build_scope_tree();

        // Resolve all references in expressions
        self.resolve_expressions();

        if self.errors.is_empty() {
            Ok(self.resolutions)
        } else {
            Err(ResolutionError {
                source: "",
                kind: ResolutionErrorKind::ScopeError(self.errors),
            })
        }
    }
}

impl ScopeAnalyzer {
    /// Create a new scope analyzer from parser output
    pub fn new(mut entity_table: EntityTable, classes: HashMap<EntityIndex, Class>) -> Self {
        let global_entity = entity_table
            .remove(&EntityIndex(0))
            .expect("Global entity does not exist!");

        let mut analyzer = Self {
            entity_table,
            classes,
            scopes: Vec::new(),
            entity_to_scope: HashMap::new(),
            resolutions: HashMap::new(),
            current_scope: ScopeIndex(0),
            errors: Vec::new(),
            method_resolution_order: HashMap::new(),
        };

        // Create the global scope
        let global_scope = analyzer.create_scope(global_entity.index, None);
        analyzer.current_scope = global_scope;

        analyzer
    }

    fn create_scope(&mut self, entity: EntityIndex, parent: Option<ScopeIndex>) -> ScopeIndex {
        let scope_idx = ScopeIndex(self.scopes.len());

        let scope = Scope {
            entity,
            parent,
            children: Vec::new(),
            declarations: HashMap::new(),
        };

        self.scopes.push(scope);
        self.entity_to_scope.insert(entity, scope_idx);

        // Add this scope as a child of its parent
        if let Some(parent_idx) = parent {
            self.scopes[parent_idx.0].children.push(scope_idx);
        }

        scope_idx
    }

    fn entity_lines(&mut self) -> HashMap<EntityIndex, usize> {
        // Store line information for error reporting
        self.classes
            .par_iter()
            .map(|(idx, class)| (*idx, class.line))
            .collect()
    }

    fn inheritance_graph(&mut self) -> HashMap<EntityIndex, Vec<EntityIndex>> {
        self.classes
            .par_iter()
            .map(|(class_idx, class)| (*class_idx, class.ancestors.clone()))
            .collect()
    }

    /// Calculate Method Resolution Order (MRO) for all classes
    /// Using C3 linearization algorithm (similar to Python's)
    fn method_order_resolution(&mut self) {
        let (sender, receiver) = channel();

        let mro_map: HashMap<EntityIndex, Vec<EntityIndex>> = self
            .classes
            .keys()
            .par_bridge()
            .map_with(sender, |sender, class_idx| {
                let mro = self.compute_mro(sender.clone(), *class_idx);
                (*class_idx, mro)
            })
            .collect();

        while let Ok(message) = receiver.recv() {
            match message {
                ScopeErrorMessage::Error(err) => self.errors.push(err),
                ScopeErrorMessage::Done => break,
            }
        }

        self.method_resolution_order.par_extend(mro_map);
    }

    /// Compute Method Resolution Order for a class using C3 linearization
    fn compute_mro(
        &mut self,
        error_sender: Sender<ScopeErrorMessage>,
        inheritance_graph: 
        class_idx: EntityIndex,
    ) -> Vec<EntityIndex> {
        // If already computed, return it
        if let Some(mro) = self.method_resolution_order.get(&class_idx) {
            return mro.clone();
        }

        // Start with the class itself
        let mut result = vec![class_idx];

        // Get direct ancestors
        let ancestors = self
            .inheritance_graph
            .get(&class_idx)
            .cloned()
            .unwrap_or_default();

        if ancestors.is_empty() {
            return result;
        }

        // Build merge lists: MRO of each parent + the list of parents
        let mut merge_lists: Vec<Vec<EntityIndex>> = ancestors
            .par_iter()
            .map_with(&error_sender, |error_sender, &ancestor| {
                self.compute_mro(error_sender.clone(), ancestor)
            })
            .collect();

        merge_lists.push(ancestors);

        // Perform C3 merge
        while !merge_lists.iter().all(|list| list.is_empty()) {
            // Find a candidate for the next element in the MRO
            let mut candidate = None;

            'outer: for list in &merge_lists {
                if list.is_empty() {
                    continue;
                }

                let head = list[0];

                // Check if this head is not in the tail of any other list
                let mut valid = true;
                for other_list in &merge_lists {
                    if other_list.len() > 1 && other_list[1..].contains(&head) {
                        valid = false;
                        break;
                    }
                }

                if valid {
                    candidate = Some(head);
                    break 'outer;
                }
            }

            match candidate {
                Some(next) => {
                    // Add to result if not already included
                    if !result.contains(&next) {
                        result.push(next);
                    }

                    // Remove from all merge lists
                    for list in &mut merge_lists {
                        if !list.is_empty() && list[0] == next {
                            list.remove(0);
                        }
                    }
                }
                None => {
                    // Cannot compute a valid MRO - inconsistent inheritance
                    if let Some(entity) = self.entity_table.get(&class_idx) {
                        error_sender.send(ScopeErrorMessage::Error(ScopeError::InheritanceError {
                            class: entity.name.clone(),
                            line: self.entity_lines.get(&class_idx).copied().unwrap_or(0),
                        }));
                    }

                    // Return partial result to avoid getting stuck
                    return result;
                }
            }
        }

        result
    }

    /// Build the complete scope hierarchy from entities
    fn build_scope_tree(&self) {
        // Get the global entity
        if let Some(global_entity) = self.entity_table.remove(&EntityIndex(0)) {
            // Process all classes from the global scope
            for &class_idx in &global_entity.sub_entities {
                self.process_class(class_idx);
            }
        }
    }

    /// Process a class entity and build its scope
    fn process_class(&self, class_idx: EntityIndex) {
        if let Some(class_entity) = self.entity_table.get(&class_idx) {
            let class_scope = self.create_scope(class_entity.index, Some(self.current_scope));
            let class_entity = &self
                .scopes
                .get(class_scope.0)
                .expect("Class scope not found")
                .entity;
            let prev_scope = self.current_scope;
            self.current_scope = class_scope;

            // Giving up in there, should think about other ways
            for decl_idx in class_entity.sub_entities.clone() {
                self.process_declaration(decl_idx);
            }

            self.current_scope = prev_scope;
        }
    }

    /// Process a declaration entity (method or state)
    fn process_declaration(&mut self, decl_idx: EntityIndex) {
        if let Some(decl_entity) = self.entity_table.remove(&decl_idx) {
            self.scopes[self.current_scope.0]
                .declarations
                .insert(decl_entity.name.clone(), decl_idx);

            let decl_scope = self.create_scope(decl_entity, Some(self.current_scope));
            let decl_entity = &self
                .scopes
                .get(decl_scope.0)
                .expect("Declaration scope not found")
                .entity;
            let prev_scope = self.current_scope;
            self.current_scope = decl_scope;

            for sub_idx in decl_entity.sub_entities.clone() {
                if let Some(sub_entity) = self.entity_table.get(&sub_idx) {
                    match sub_entity.kind {
                        EntityKind::Local => self.process_local(sub_idx),
                        EntityKind::Call => self.process_call(sub_idx),
                        _ => {} // Unexpected entity type
                    }
                }
            }

            self.current_scope = prev_scope;
        }
    }

    /// Process a local entity (function definition)
    fn process_local(&mut self, local_idx: EntityIndex) {
        if let Some(local_entity) = self.entity_table.remove(&local_idx) {
            self.scopes[self.current_scope.0]
                .declarations
                .insert(local_entity.name.clone(), local_idx);

            // Create a scope for this local
            let local_scope = self.create_scope(local_entity, Some(self.current_scope));
            let local_entity = &self
                .scopes
                .get(local_scope.0)
                .expect("Local scope not found")
                .entity;
            let prev_scope = self.current_scope;
            self.current_scope = local_scope;

            // Register local name in parent scope
            // Process all calls within this local
            for sub_idx in local_entity.sub_entities.clone() {
                if let Some(sub_entity) = self.entity_table.get(&sub_idx) {
                    if sub_entity.kind == EntityKind::Call {
                        self.process_call(sub_idx);
                    }
                }
            }

            self.current_scope = prev_scope;
        }
    }

    /// Process a call entity (reference to another entity)
    fn process_call(&mut self, call_idx: EntityIndex) {
        if let Some(_) = self.entity_table.remove(&call_idx) {
            // Store line information for error reporting
            if let Some(class) = self
                .classes
                .get(&self.get_containing_class(self.current_scope))
            {
                self.entity_lines.insert(call_idx, class.line);
            }

            // Call entities are resolved later during expression analysis
        }
    }

    /// Resolve all expressions in the program
    fn resolve_expressions(&mut self) -> &mut Self {
        if let Some(classes) = classes {
            if classes.is_empty() {
                self
            } else {
                self.resolve_class(classes.iter().next().unwrap().1)
                    .resolve_expressions(Some(classes.into_iter().skip(1).collect::<Vec<_>>()))
            }
        } else {
            self.resolve_expressions(Some(self.classes.iter().collect()))
        }
    }

    fn resolve_class(&mut self, class: &Class) -> &mut Self {
        // Process all methods in the class
        for (method_idx, functions) in &class.methods {
            // Get the scope for this method
            if let Some(&method_scope) = self.entity_to_scope.get(method_idx) {
                self.current_scope = method_scope;

                // Process each function implementation
                for function in functions {
                    self.process_function(function);
                }
            }
        }

        self
    }

    /// Process a function and resolve all names in it
    fn process_function(&mut self, function: &Function) {
        // Process parameters
        for param in &function.params {
            if let Some(value) = &param.value {
                self.resolve_expression(value);
            }
        }

        // Process return type if any
        if let Some(return_type) = &function.return_type {
            self.resolve_expression(return_type);
        }

        // Process the function body
        self.resolve_expression(&function.body);
    }

    fn resolve_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Block {
                params,
                expressions,
                value,
            } => {
                // Create a new scope for this block
                let block_scope = self.create_scope(EntityIndex(0), Some(self.current_scope));
                let prev_scope = self.current_scope;
                self.current_scope = block_scope;

                // Register parameters in the block scope
                for param in params {
                    if let PatternName::Name(name_idx) = &param.name {
                        if let Some(entity) = self.entity_table.get(name_idx) {
                            self.scopes[self.current_scope.0]
                                .declarations
                                .insert(entity.name.clone(), *name_idx);
                        }
                    }
                }

                // Process expressions in the block
                for expr in expressions {
                    self.resolve_expression(expr);
                }
                self.resolve_expression(value);

                self.current_scope = prev_scope;
            }

            Expression::Match { scrutinee, arms } => {
                // Process the scrutinee
                self.resolve_expression(scrutinee);

                // Process each match arm
                for (pattern, arm_expr) in arms {
                    // Create a new scope for this arm
                    let arm_scope = self.create_scope(EntityIndex(0), Some(self.current_scope));
                    let prev_scope = self.current_scope;
                    self.current_scope = arm_scope;

                    // Register any names from the pattern
                    if let PatternName::Name(name_idx) = &pattern.name {
                        if let Some(entity) = self.entity_table.get(name_idx) {
                            self.scopes[self.current_scope.0]
                                .declarations
                                .insert(entity.name.clone(), *name_idx);
                        }
                    }

                    // Process the arm expression
                    self.resolve_expression(arm_expr);

                    self.current_scope = prev_scope;
                }
            }

            Expression::Call {
                name_id,
                caller,
                args,
                block,
            } => {
                // Process the caller if exists
                if let Some(caller_expr) = caller {
                    self.resolve_expression(caller_expr);
                }

                // Process arguments
                for arg in args {
                    if let Some(value) = &arg.value {
                        self.resolve_expression(value);
                    }
                }

                // Process block if exists
                if let Some(block_expr) = block {
                    self.resolve_expression(block_expr);
                }

                // Resolve this call
                self.resolve_name(*name_id, args);
            }

            Expression::Binary(left, _, right) => {
                self.resolve_expression(left);
                self.resolve_expression(right);
            }

            Expression::Unary(_, expr) => {
                self.resolve_expression(expr);
            }

            Expression::Grouping(expr) => {
                self.resolve_expression(expr);
            }

            // Other expression types don't contain names to resolve
            _ => {}
        }
    }

    fn resolve_name(&mut self, call_idx: EntityIndex, args: &[Pattern]) {
        if let Some(call_entity) = self.entity_table.get(&call_idx) {
            // Get the name to resolve
            let name = &call_entity.name;

            // Find all candidate declarations
            let candidates = self.lookup_name(name);

            // Filter candidates by argument compatibility
            let mut filtered_candidates = Vec::new();

            for candidate in candidates {
                // Check if candidate is a function
                if let Some(functions) = self
                    .classes
                    .get(&self.get_containing_class(self.current_scope))
                    .and_then(|class| class.methods.get(&candidate))
                {
                    // Try each function implementation
                    for (impl_idx, function) in functions.iter().enumerate() {
                        if self.is_compatible_call(args, &function.params) {
                            filtered_candidates.push((candidate, Some(impl_idx)));
                        }
                    }
                } else {
                    // Not a function, just a regular entity
                    filtered_candidates.push((candidate, None));
                }
            }

            // Store the resolution result
            let resolution = match filtered_candidates.len() {
                0 => {
                    // No candidates found
                    self.errors.push(ScopeError::UndefinedName {
                        name: name.clone(),
                        line: self.entity_lines.get(&call_idx).copied().unwrap_or(0),
                    });
                    Resolution::Candidates(Vec::new())
                }
                1 => {
                    // Single candidate found
                    let (entity, impl_idx) = filtered_candidates[0];
                    match impl_idx {
                        Some(idx) => Resolution::Function(entity, idx),
                        None => Resolution::Entity(entity),
                    }
                }
                _ => {
                    // Multiple candidates - need type info to resolve
                    Resolution::Candidates(filtered_candidates)
                }
            };

            self.resolutions.insert(call_idx, resolution);
        }
    }

    fn lookup_name(&self, name: &str) -> Vec<EntityIndex> {
        let mut candidates = Vec::new();
        let mut scope_idx = Some(self.current_scope);
        let mut visited_classes = HashSet::new();

        // First look in local scopes (up the scope hierarchy)
        while let Some(idx) = scope_idx {
            let scope = &self.scopes[idx.0];

            // Check if name exists in this scope
            if let Some(&entity_idx) = scope.declarations.get(name) {
                candidates.push(entity_idx);

                // Found in immediate scope, don't check ancestors
                // This implements "closest definition wins"
                if scope.entity.kind() == EntityKind::Class {
                    visited_classes.insert(scope.entity);
                }
            }

            // Move to parent scope
            scope_idx = scope.parent;
        }

        // If we've found candidates already, return them
        if !candidates.is_empty() {
            return candidates;
        }

        // Otherwise, check class hierarchies based on MRO
        let current_class = self.get_containing_class(self.current_scope);

        if let Some(mro) = self.method_resolution_order.get(&current_class) {
            for &class_idx in mro {
                if visited_classes.contains(&class_idx) {
                    continue;
                }

                visited_classes.insert(class_idx);

                // Check if this class has the name
                if let Some(class_entity) = self.entity_table.get(&class_idx) {
                    for &sub_idx in &class_entity.sub_entities {
                        if let Some(entity) = self.entity_table.get(&sub_idx) {
                            if entity.name == name {
                                candidates.push(sub_idx);
                                return candidates; // Return first match based on MRO
                            }
                        }
                    }
                }
            }
        }

        candidates
    }

    /// Check if a call's arguments are compatible with a function's parameters
    fn is_compatible_call(&self, args: &[Pattern], params: &[Pattern]) -> bool {
        // Simple compatibility check based on argument count
        // A more sophisticated implementation would check argument types
        args.len() == params.len()
    }

    fn get_containing_class(&self, mut scope_idx: ScopeIndex) -> EntityIndex {
        loop {
            let scope = &self.scopes[scope_idx.0];
            let entity = &self.entity_table[&scope.entity];

            if entity.kind == EntityKind::Class {
                return scope.entity;
            }

            if let Some(parent) = scope.parent {
                scope_idx = parent;
            } else {
                // If we can't find a containing class, return the global entity
                return EntityIndex(0);
            }
        }
    }

    /// Get the method resolution order for a class
    pub fn get_mro(&self, class_idx: EntityIndex) -> Option<&Vec<EntityIndex>> {
        self.method_resolution_order.get(&class_idx)
    }
}

pub enum ScopeErrorMessage {
    Error(ScopeError),
    Done,
}

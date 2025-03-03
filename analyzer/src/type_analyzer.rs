mod r#type;
use crate::Analyzer;
use crate::{AnalysisPipeline, ResolutionResult};
use ahash::{HashMap, HashMapExt};
use parser::{EntityIndex, Statement};
pub use r#type::*;
use rayon::prelude::*;

pub type TypeRegistry = HashMap<EntityIndex, Type>;

pub struct TypeAnalyzer<'a> {
    pipeline: &'a AnalysisPipeline,
}

impl<'a> Analyzer for TypeAnalyzer<'a> {
    type Output = TypeRegistry;

    fn analyze(self) -> ResolutionResult<TypeRegistry> {
        self.register_classes(&[])
    }
}

impl TypeAnalyzer<'_> {
    pub fn new(pipeline: &AnalysisPipeline) -> TypeAnalyzer {
        TypeAnalyzer { pipeline }
    }

    fn register_classes(self, known_classes: &[EntityIndex]) -> ResolutionResult<TypeRegistry> {
        let type_registry: ResolutionResult<HashMap<_, _>> = self
            .pipeline
            .classes
            .par_iter()
            .filter_map(|(_index, class_info)| {
                class_info
                    .ancestors
                    .par_iter()
                    .all(|index| known_classes.contains(index))
                    .then_some(self.register_class(class_info))
            })
            .collect();
        let mut type_registry = type_registry?;

        if known_classes.len() + type_registry.len() != self.pipeline.classes.len() {
            let known_classes: Vec<_> = type_registry
                .keys()
                .par_bridge()
                .filter_map(|index| self.pipeline.classes.contains_key(index).then_some(*index))
                .collect();
            type_registry.par_extend(self.register_classes(&known_classes)?);
        }
        Ok(type_registry)
    }

    fn register_class(&self, class_info: &parser::Class) -> ResolutionResult<(EntityIndex, Type)> {
        let states: ResolutionResult<_> = class_info
            .states
            .par_iter()
            .map(|state| self.register_state(class_info, state))
            .collect();

        Ok((
            class_info.name,
            Type::Class {
                entity: class_info.name,
                ancestors: class_info.ancestors.clone(),
                states: states?,
            },
        ))
    }

    fn register_state(
        &self,
        class_info: &parser::Class,
        state: &Statement,
    ) -> ResolutionResult<ClassState> {
        let Statement::ClassState { name, patterns } = state else {
            panic!("Invalid state: {:?}", state)
        };

        unimplemented!()
    }

    fn register_local(&self) -> ResolutionResult<EntityIndex> {
        unimplemented!()
    }

    //     pub fn register_class(
    //         &mut self,
    //         class: &parser::Class,
    //     ) -> ResolutionResult<TypeIndex> {
    //         let type_index = TypeIndex(self.types.len());
    //         self.type_names.insert(class.name, type_index);
    //         let ancestors = class
    //             .ancestors
    //             .map(|ancestor_name| {
    //                 let ancestor_index = self.type_names.get(ancestor_name);

    //                 match ancestor_index {
    //                     Some(ancestor_index) if self.types.contains_key(ancestor_index) => {
    //                         ancestor_index
    //                     }
    //                     Some(_) => {
    //                         panic!(
    //                             "TypeRegistry::register_class: Ancestor {:?} not found",
    //                             pipeline
    //                                 .names
    //                                 .get_by_left(ancestor_name)
    //                                 .expect("TypeRegistry::register_class: Ancestor not found")
    //                         );
    //                     }
    //                     None => {
    //                         let Some(ancestor_class) = pipeline.classes.get(ancestor_name) else {
    //                             panic!(
    //                                 "TypeRegistry::register_class: Ancestor {:?} not found",
    //                                 ancestor_name,
    //                             )
    //                         };
    //                         match self.register_class(&pipeline, ancestor_class) {
    //                             Ok(ancestor_index) => ancestor_index,
    //                             Err(e) => return Err(e),
    //                         }
    //                     }
    //                 }
    //             })
    //             .collect();
    //         let states = unimplemented!();
    //         self.types.insert(
    //             type_index,
    //             Type::Class {
    //                 name: class.name,
    //                 ancestors,
    //                 states,
    //             },
    //         );

    //         Ok(type_index)
    //     }

    //     pub fn check_state_pattern() -> ResolutionResult<bool> {
    //         unimplemented!()
    //     }

    //     pub fn is_subtype() -> ResolutionResult<bool> {
    //         unimplemented!()
    //     }
}

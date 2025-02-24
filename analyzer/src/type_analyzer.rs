mod r#type;
use crate::{AnalysisPipeline, ResolutionResult};
use ahash::AHashMap;
use parser::EntityIndex;
pub use r#type::*;

pub type TypeRegistry = AHashMap<EntityIndex, Type>;

pub struct TypeAnalyzer<'a> {
    current_pipeline: &'a AnalysisPipeline,
    type_registry: TypeRegistry,
}

impl TypeAnalyzer<'_> {
    pub fn new(pipeline: &AnalysisPipeline) -> TypeAnalyzer {
        TypeAnalyzer {
            current_pipeline: pipeline,
            type_registry: TypeRegistry::new(),
        }
    }

    pub fn analyze(self) -> ResolutionResult<TypeRegistry> {
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

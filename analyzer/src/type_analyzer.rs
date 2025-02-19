mod state_pattern;
mod r#type;
mod type_registry;
use crate::{AnalysisPipeline, ResolutionResult};
pub use r#type::*;
pub use type_registry::TypeRegistry;

pub struct TypeAnalyzer<'a> {
    current_pipeline: &'a AnalysisPipeline,
}

impl TypeAnalyzer<'_> {
    pub fn new(pipeline: &AnalysisPipeline) -> TypeAnalyzer {
        TypeAnalyzer {
            current_pipeline: pipeline,
        }
    }

    pub fn analyze(&self) -> ResolutionResult<TypeRegistry> {
        unimplemented!()
    }
}

mod typed_ast;
pub use typed_ast::TypedClass;
mod r#type;
use crate::Analyzer;
use crate::{AnalysisPipeline, ResolutionResult};
// use ahash::{HashMap, HashMapExt};
// use parser::{EntityIndex, Statement};
pub use r#type::*;
// use rayon::prelude::*;

pub struct TypeAnalyzer<'a> {
    pipeline: &'a AnalysisPipeline,
}

impl<'a> Analyzer for TypeAnalyzer<'a> {
    type Output = Vec<TypedClass>;

    fn analyze(self) -> ResolutionResult<Vec<TypedClass>> {
        self.analyze_classes()
    }
}

impl TypeAnalyzer<'_> {
    pub fn new(pipeline: &AnalysisPipeline) -> TypeAnalyzer {
        TypeAnalyzer { pipeline }
    }

    pub fn analyze_classes(self) -> ResolutionResult<Vec<TypedClass>> {
        unimplemented!()
    }
}

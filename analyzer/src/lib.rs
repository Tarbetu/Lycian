mod analyzer;
mod inheritance_analyzer;
mod resolution_error;
mod scope_analyzer;
mod type_analyzer;
use parser::ParserProduct;
use parser::{Class, Entity, EntityIndex, EntityTable, Literal, LiteralIndex, Parser};

use ahash::AHashMap;
pub use analyzer::Analyzer;
use inheritance_analyzer::InheritanceAnalyzer;
pub use resolution_error::ResolutionError;
pub use resolution_error::ResolutionResult;
use type_analyzer::{TypeAnalyzer, TypedClass};

/// The AnalysisPipeline struct contains all the analyzers and optimizers that are used to
/// analyze the AST and produce a more optimized version of the AST.
///
/// InheritanceAnalyzer: Detects inheritance cycles and invalid inheritance names.
/// ScopeResolver: Revolves the scope hierarchy and names of a program.
/// TypeAnalyzer: A classic for AOT compiler, type analyzer checks and deduce the types
/// MemoryAnalyzer: Lycian handles the memory statically instead of using a garbage collector.
/// EffectAnalyzer: Lycian aims to be a pure language, but it allows effects for IO. Also we check the branches to determine if the code suits for GPU.
/// Codegen and Optimizer: Lycian will have a backend, and we can use MLIR optimization passes.
pub struct AnalysisPipeline {
    global_entity: Entity,
    entities: EntityTable,
    classes: AHashMap<EntityIndex, Class>,
    literals: AHashMap<LiteralIndex, Literal>,
}

impl From<ParserProduct> for AnalysisPipeline {
    fn from(parser: ParserProduct) -> AnalysisPipeline {
        AnalysisPipeline {
            global_entity: parser.global_entity,
            entities: parser.entities,
            literals: parser.literals,
            classes: parser.classes,
        }
    }
}

impl AnalysisPipeline {
    pub fn analyze_inheritance(&self) -> ResolutionResult<()> {
        InheritanceAnalyzer::new(&self.classes).analyze()
    }

    pub fn analyze_scopes(&self) -> ResolutionResult<()> {
        // ScopeResolver::new(self).analyze()
        Ok(())
    }

    pub fn analyze_types(&self) -> ResolutionResult<Vec<TypedClass>> {
        TypeAnalyzer::new(self).analyze()
    }
}

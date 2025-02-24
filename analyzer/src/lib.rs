mod resolution_error;
mod type_analyzer;
use parser::{Class, Entity, EntityIndex, EntityTable, Literal, LiteralIndex, Parser};

use ahash::AHashMap;
pub use resolution_error::ResolutionError;
pub use resolution_error::ResolutionResult;
use type_analyzer::{TypeAnalyzer, TypeRegistry};

/// The AnalysisPipeline struct contains all the analyzers and optimizers that are used to
/// analyze the AST and produce a more optimized version of the AST.
///
/// TypeAnalyzer: A classic for AOT compiler, type analyzer checks and deduce the types
/// InheritanceResolver: Lycian staticly resolves inheritance and checks for cycles.
/// ScopeResolver: Revolves the scope hierarchy and names of a program.
/// MemoryAnalyzer: Lycian handles the memory statically instead of using a garbage collector.
/// EffectAnalyzer: Lycian aims to be a pure language, but it allows effects for IO. Also we check the branches to determine if the code suits for GPU.
/// Codegen and Optimizer: Lycian will have a backend, and we can use MLIR optimization passes.
pub struct AnalysisPipeline {
    global_entity: Entity,
    entities: EntityTable,
    classes: AHashMap<EntityIndex, Class>,
    literals: AHashMap<LiteralIndex, Literal>,
}

impl AnalysisPipeline {
    pub fn new(parser: Parser) -> AnalysisPipeline {
        AnalysisPipeline {
            global_entity: parser.global_entity,
            entities: parser.entities,
            literals: parser.literals,
            classes: parser.classes,
        }
    }

    fn analyze_types(&self) -> ResolutionResult<TypeRegistry> {
        TypeAnalyzer::new(&self).analyze()
    }
}

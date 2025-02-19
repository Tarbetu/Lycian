mod resolution_error;
mod type_analyzer;
use parser::{Class, Literal, LiteralIndex, Name, NameIndex, Parser};

use ahash::AHashMap;
use bimap::BiHashMap;
pub use resolution_error::ResolutionError;
pub use resolution_error::ResolutionResult;
use type_analyzer::{TypeAnalyzer, TypeRegistry};

/// The AnalysisPipeline struct contains all the analyzers and optimizers that are used to
/// analyze the AST and produce a more optimized version of the AST.
///
/// TypeAnalyzer: A classic for AOT compiler, type checker checks and deduce the types
/// InheritanceResolver: Lycian staticly resolves inheritance and checks for cycles.
/// ScopeResolver: Revolves the scope hierarchy and names of a program.
/// MemoryAnalyzer: Lycian handles the memory statically instead of using a garbage collector.
/// EffectAnalyzer: Lycian aims to be a pure language, but it allows effects for IO. Also we check the branches to determine if the code suits for GPU.
/// Codegen and Optimizer: Lycian will have a backend, and we can use MLIR optimization passes.
pub struct AnalysisPipeline {
    names: BiHashMap<NameIndex, Name>,
    classes: AHashMap<NameIndex, Class>,
    literals: AHashMap<LiteralIndex, Literal>,
}

impl AnalysisPipeline {
    pub fn new(parser: Parser) -> AnalysisPipeline {
        AnalysisPipeline {
            names: parser.names,
            literals: parser.literals,
            classes: parser.classes,
        }
    }

    fn analyze_types(&self) -> ResolutionResult<TypeRegistry> {
        TypeAnalyzer::new(&self).analyze()
    }
}

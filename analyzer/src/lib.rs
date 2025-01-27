mod name_resolver;
mod resolver;
pub use name_resolver::NameResolver;

struct AnalysisPipeline {
    name_resolver: NameResolver,
    // inheritance_resolver: InheritanceResolver,
    // type_checker: TypeChecker,
    // memory_analyzer: MemoryAnalyzer,
    // effect_analyzer: EffectAnalyzer,
    // optimizer: Optimizer,
}
pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}

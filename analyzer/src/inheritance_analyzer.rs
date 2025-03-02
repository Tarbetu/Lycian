use crate::Analyzer;
use ahash::{AHashMap, AHashSet};
use either::Either;
use parser::{Class, EntityIndex};
use rayon::prelude::*;

use crate::{
    resolution_error::{InheritanceError, InheritanceErrorKind, ResolutionErrorKind},
    ResolutionError, ResolutionResult,
};

pub struct InheritanceAnalyzer<'a> {
    pub classes: &'a AHashMap<EntityIndex, Class>,
}

impl<'a> Analyzer for InheritanceAnalyzer<'a> {
    type Output = ();

    fn analyze(self) -> ResolutionResult<()> {
        let result: Vec<InheritanceError> = self
            .classes
            .par_iter()
            .flat_map(|(_, class)| {
                self.analyze_class(Either::Left(class), &mut AHashSet::new(), &mut Vec::new())
            })
            .collect();

        if result.is_empty() {
            Ok(())
        } else {
            Err(ResolutionError {
                source: "Inheritance Analyzer",
                kind: ResolutionErrorKind::InheritanceError(result),
            })
        }
    }
}

impl<'a> InheritanceAnalyzer<'a> {
    pub fn new(classes: &'a AHashMap<EntityIndex, Class>) -> Self {
        InheritanceAnalyzer { classes }
    }

    /// Detects classes that are not defined in the program, non-classes or cyclic inheritance.
    fn analyze_class(
        &self,
        class: Either<&Class, EntityIndex>,
        visited: &mut AHashSet<EntityIndex>,
        path: &mut Vec<EntityIndex>,
    ) -> Vec<InheritanceError> {
        let class = match class {
            Either::Left(class) => class,
            Either::Right(index) => match self.classes.get(&index) {
                Some(class) => class,
                None => {
                    return vec![InheritanceError {
                        index,
                        kind: InheritanceErrorKind::UndefinedClass(index),
                        line: 0,
                    }]
                }
            },
        };

        if visited.contains(&class.name) {
            let cycle_index = path.iter().position(|&x| x == class.name).unwrap();

            return vec![InheritanceError {
                index: class.name,
                kind: InheritanceErrorKind::Cycle(path[cycle_index..].to_vec()),
                line: 0,
            }];
        }

        visited.insert(class.name);
        path.push(class.name);

        class
            .ancestors
            .iter()
            .flat_map(|ancestor| self.analyze_class(Either::Right(*ancestor), visited, path))
            .collect()
    }
}

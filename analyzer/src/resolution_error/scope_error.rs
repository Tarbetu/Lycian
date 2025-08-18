use parser::EntityIndex;
use std::fmt;

#[derive(Debug, Clone)]
pub enum ScopeError {
    /// Name not found in any accessible scope
    UndefinedName { name: String, line: Option<usize> },

    /// Multiple entities with the same name found
    AmbiguousName {
        name: String,
        candidates: Vec<EntityIndex>,
        line: usize,
    },

    /// Entity exists but is not accessible due to visibility rules
    VisibilityViolation {
        name: String,
        entity: EntityIndex,
        line: usize,
    },

    /// Inheritance isn't in the right order
    InheritanceError { class: String, line: usize },
}

impl fmt::Display for ScopeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UndefinedName { name, line } => {
                write!(f, "[line {}] Error: Undefined name '{}'", line, name)
            }
            Self::AmbiguousName {
                name,
                candidates,
                line,
            } => {
                write!(
                    f,
                    "[line {}] Error: Ambiguous name '{}', found {} candidates",
                    line,
                    name,
                    candidates.len()
                )
            }
            Self::VisibilityViolation {
                name,
                entity: _,
                line,
            } => {
                write!(
                    f,
                    "[line {}] Error: '{}' is not accessible in this context",
                    line, name
                )
            }
            Self::InheritanceError { class, line } => {
                write!(
                    f,
                    "[line {}] Error: Invalid inheritance in class '{}'",
                    line, class
                )
            }
        }
    }
}

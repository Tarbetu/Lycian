use crate::Scanner;
use std::{fmt::Display, rc::Rc};

#[derive(Debug, Clone, PartialEq, Default, Hash)]
pub struct Span {
    pub file: Rc<String>,
    pub line: usize,
    pub position: usize,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} on {} (at position {})",
            self.file, self.line, self.position
        )
    }
}

impl Span {
    pub fn from_scanner(scanner: &Scanner, position: usize) -> Self {
        Self {
            file: scanner.file.clone(),
            line: scanner.line,
            position,
        }
    }

    pub fn two_char(self) -> Self {
        Self {
            position: self.position + 1,
            ..self
        }
    }

    pub fn three_char(self) -> Self {
        Self {
            position: self.position + 2,
            ..self
        }
    }

    pub fn variable_char(self, position: usize) -> Self {
        Self { position, ..self }
    }
}

use crate::Scanner;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Span {
    pub file: Rc<String>,
    pub line: usize,
    pub position: usize,
}

impl Span {
    pub fn from_scanner(scanner: &Scanner, position: usize) -> Self {
        Self {
            file: scanner.file.clone(),
            line: scanner.line,
            position: position,
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
        Self {
            position: position,
            ..self
        }
    }
}

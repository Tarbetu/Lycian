use ahash::AHashMap;

use crate::EntityIndex;
use crate::Expression;
use crate::Pattern;

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: EntityIndex,
    pub params: Vec<Pattern>,
    pub return_type: Option<Expression>,
    pub environment: Option<AHashMap<EntityIndex, Vec<Function>>>,
    pub body: Expression,
    pub decorator: String,
}

impl Function {
    pub fn force_to_gpu(&self) -> bool {
        let decorator = self.parse_decorator();
        decorator.first().map(|text| text == "gpu").unwrap_or(false)
            && (self.decorator.len() == 1
                || decorator
                    .get(1)
                    .map(|text| text == "force")
                    .unwrap_or(false))
    }

    pub fn pretend_pure(&self) -> bool {
        let decorator = self.parse_decorator();
        decorator
            .first()
            .map(|text| text == "pure")
            .unwrap_or(false)
            && (self.decorator.len() == 1
                || decorator
                    .get(1)
                    .map(|text| text == "pretend")
                    .unwrap_or(false))
    }

    fn parse_decorator(&self) -> Vec<String> {
        self.decorator
            .split(':')
            .map(|text| text.trim().to_lowercase())
            .collect()
    }
}

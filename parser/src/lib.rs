mod error;
mod expression;
mod function;
mod literal;
mod newtypes;
mod operator;
mod statement;

pub use crate::literal::*;
pub use expression::Expression;
pub use function::Function;
pub use statement::Statement;

pub use newtypes::*;

pub use error::ParserResult;

use scanner::{Token, TokenType};

pub struct Parser<'a> {
    tokens: &'a [Token],
    position: usize,
    constants: Vec<String>,
    identifiers: Vec<String>,
    wildcards: Vec<String>,
    functions: Vec<Function>,
    literals: Vec<Literal>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Parser<'a> {
        Parser {
            tokens,
            position: 0,
            constants: vec![],
            identifiers: vec![],
            wildcards: vec![],
            functions: vec![],
            literals: vec![],
        }
    }

    pub fn get_constant(&mut self, index: ConstantIndex) -> &str {
        self.constants.get(index.0).expect("Invalid constant index")
    }

    pub fn get_identifier(&mut self, index: IdentifierIndex) -> &str {
        self.identifiers
            .get(index.0)
            .expect("Invalid identifier index")
    }

    pub fn get_wildcard(&mut self, index: WildcardIndex) -> &str {
        self.wildcards.get(index.0).expect("Invalid wildcard index")
    }

    pub fn get_function(&mut self, index: FunctionIndex) -> &Function {
        self.functions.get(index.0).expect("Invalid function index")
    }

    pub fn get_literal(&mut self, index: LiteralIndex) -> &Literal {
        self.literals.get(index.0).expect("Invalid literal index")
    }

    pub fn parse(&mut self) -> ParserResult<Vec<Statement>> {
        let mut statements = Vec::new();

        while self.peek().is_some() {
            let program = self.class_declaration();

            match program {
                Ok(class) => statements.push(class),
                Err(e) => {
                    self.sync();
                    return Err(e);
                }
            }
        }

        Ok(statements)
    }

    fn peek(&self) -> Option<&Token> {
        match self.tokens.get(self.position) {
            Some(token) if token.kind == TokenType::Eof => None,
            Some(token) => Some(token),
            None => None,
        }
    }
}

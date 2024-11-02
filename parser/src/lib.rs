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

pub use error::ParserError;
pub use error::ParserResult;

use scanner::{Token, TokenType};

pub struct Parser<'a> {
    lexemes: &'a [&'a str],
    tokens: &'a [Token],
    position: usize,
    names: Vec<Name>,
    functions: Vec<Function>,
    literals: Vec<Literal>,
}

impl<'a> Parser<'a> {
    pub fn new(lexemes: &'a [&'a str], tokens: &'a [Token]) -> Parser<'a> {
        Parser {
            lexemes,
            tokens,
            position: 0,
            names: vec![],
            functions: vec![],
            literals: vec![],
        }
    }

    pub fn get_name(&mut self, index: NameIndex) -> &Name {
        self.names.get(index.0).expect("Invalid wildcard index")
    }

    pub fn get_function(&mut self, index: FunctionIndex) -> &Function {
        self.functions.get(index.0).expect("Invalid function index")
    }

    pub fn get_literal(&mut self, index: LiteralIndex) -> &Literal {
        self.literals.get(index.0).expect("Invalid literal index")
    }

    pub fn parse(&mut self) -> ParserResult<Vec<Statement>> {
        let mut classes = Vec::new();

        while self.peek().is_some() {
            let program = self.class_declaration();

            match program {
                Ok(class) => classes.push(class),
                Err(e) => {
                    return Err(e);
                }
            }
        }

        Ok(classes)
    }

    pub fn class_declaration(&mut self) -> ParserResult<Statement> {
        // Get decorator!
        use TokenType::{Colon, Endline, Implementing, Indent};
        let name = self.consume_name()?;

        self.consume(Colon, "':'")?;
        self.consume(Endline, "End of line")?;
        self.consume(Indent, "Indendation start")?;

        let mut implementing_list = Vec::new();

        if self.is_match(&[Implementing]) {
            implementing_list.push(self.consume_name()?);

            while let Some(Implementing) = self.peek().map(|t| t.kind) {
                self.advance();
                implementing_list.push(self.consume_name()?);
            }
        }

        let states = vec![];
        let methods = vec![];

        Ok(Statement::Class {
            name,
            implementing_list,
            states,
            methods,
            decorator: String::new(),
        })
    }

    fn peek(&self) -> Option<Token> {
        match self.tokens.get(self.position) {
            Some(token) if token.kind == TokenType::Eof => None,
            Some(token) => Some(*token),
            None => None,
        }
    }

    fn previous(&self) -> Token {
        *self.tokens.get(self.position).unwrap()
    }

    fn advance(&mut self) -> Token {
        if self.peek().is_some() {
            self.position += 1;
        }

        self.previous()
    }

    fn consume_name(&mut self) -> ParserResult<NameIndex> {
        use ParserError::UnexpectedToken;
        use TokenType::{Constant, Identifier, Wildcard};

        match self.peek() {
            Some(token)
                if token.kind == Constant || token.kind == Identifier || token.kind == Wildcard =>
            {
                self.advance();
                Ok(self.push_name(&token))
            }
            Some(token) => Err(UnexpectedToken {
                expected: "Class Name",
                found: token.kind,
                line: Some(token.line),
            }),
            None => Err(UnexpectedToken {
                expected: "Class Name",
                found: TokenType::Eof,
                line: None,
            }),
        }
    }

    fn push_name(&mut self, token: &Token) -> NameIndex {
        use TokenType::{Constant, Identifier, Wildcard};
        let index = NameIndex(self.names.len());

        let string = self.lexemes[token.start..token.end].join("");

        let name = match token.kind {
            Constant => Name::Public(string),
            Identifier => Name::Protected(string),
            Wildcard => Name::Private(string),
            _ => unreachable!(),
        };
        self.names.push(name);

        index
    }

    fn is_match(&mut self, kinds: &[TokenType]) -> bool {
        kinds
            .iter()
            .any(|kind| self.peek().map_or(false, |token| token.kind == *kind))
            .then(|| self.advance())
            .is_some()
    }

    fn consume(&mut self, kind: TokenType, expected: &'static str) -> ParserResult<()> {
        if !self.is_match(&[kind]) {
            let token = self.peek().unwrap();

            return Err(ParserError::UnexpectedToken {
                expected,
                found: token.kind,
                line: Some(token.line),
            });
        }

        Ok(())
    }
}

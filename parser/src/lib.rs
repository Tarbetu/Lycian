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

    pub fn get_function(&mut self, index: FunctionIndex) -> &mut Function {
        self.functions
            .get_mut(index.0)
            .expect("Invalid function index")
    }

    pub fn get_literal(&mut self, index: LiteralIndex) -> &Literal {
        self.literals.get(index.0).expect("Invalid literal index")
    }

    pub fn parse(&mut self) -> ParserResult<Vec<Statement>> {
        let mut classes = Vec::new();

        while self.peek().is_some() {
            let program = self.class();

            match program {
                Ok(class) => classes.push(class),
                Err(e) => {
                    return Err(e);
                }
            }
        }

        Ok(classes)
    }

    pub fn class(&mut self) -> ParserResult<Statement> {
        use Statement::*;
        use TokenType::*;

        let decorator = self.consume_decorator()?;

        let name = self.consume_name()?;

        self.consume(Colon, "':'")?;
        self.consume(Endline, "End of line")?;
        self.consume(Indent, "Indendation start")?;

        let mut implementing_list = Vec::new();

        if self.is_match(&[TokenType::Implementing]) {
            implementing_list.push(self.consume_name()?);

            while let Some(TokenType::Implementing) = self.peek().map(|t| t.kind) {
                self.advance();
                implementing_list.push(self.consume_name()?);
            }
        }

        let mut states = vec![];
        let mut methods = vec![];

        while let Some(declaration) = self.declaration()? {
            match declaration {
                state @ ClassState { .. } => states.push(state),
                Method(index) => {
                    self.get_function(index).class = Some(name);
                    methods.push(index)
                }
                _ => unreachable!(),
            };
        }

        Ok(Statement::Class {
            name,
            implementing_list,
            states,
            methods,
            decorator,
        })
    }

    fn declaration(&mut self) -> ParserResult<Option<Statement>> {
        use Statement::{ClassState, Method};
        use TokenType::{Arrow, Equal, Semicolon};

        let decorator = self.consume_decorator()?;

        let name = self.consume_name()?;

        let patterns = self.pattern_list()?;

        if self.is_match(&[Semicolon]) {
            Ok(Some(ClassState { name, patterns }))
        } else if self.is_match(&[TokenType::Dedent]) {
            Ok(None)
        } else {
            let return_type = if self.is_match(&[Arrow]) {
                self.advance();

                self.expression()?
            } else {
                return Err(ParserError::MissingReturnType {
                    function: self.get_name(name).to_string(),
                    line: self.previous().line,
                });
            };

            self.consume(Equal, "Equal sign")?;

            self.next_if(&[Semicolon]);

            let body = self.block()?;

            Ok(Some(Method(self.push_function(
                name,
                None,
                patterns,
                return_type,
                body,
                decorator,
            ))))
        }
    }

    fn pattern_list(&mut self) -> ParserResult<Vec<Expression>> {
        use TokenType::{Comma, ParenClose, ParenOpen, Semicolon};

        let mut patterns = Vec::new();

        if self.is_match(&[ParenOpen]) {
            self.next_if(&[Semicolon]);

            patterns.push(self.pattern()?);

            while self.is_match(&[Comma]) {
                self.advance();
                self.next_if(&[Semicolon]);

                patterns.push(self.pattern()?);
                self.next_if(&[Semicolon]);
            }

            self.consume(ParenClose, "Right Paranthesis")?;
        }

        Ok(patterns)
    }

    fn pattern(&mut self) -> ParserResult<Expression> {
        use TokenType::*;

        if self.is_match(&[Wildcard, Identifier, Constant]) {
            let name = self.push_name(&self.peek().unwrap());

            if self.is_match(&[Colon]) {
                if self.is_match(&[Colon]) {
                    self.advance();
                }

                let value = Box::new(self.expression()?);

                Ok(Expression::Pattern {
                    name: Some(name),
                    value,
                })
            } else {
                Ok(Expression::Pattern {
                    name: None,
                    value: Box::new(Expression::Type(name)),
                })
            }
        } else {
            Ok(Expression::Pattern {
                name: None,
                value: Box::new(self.expression()?),
            })
        }
    }

    fn block(&mut self) -> ParserResult<Expression> {
        use TokenType::{Dedent, Semicolon};

        let mut expressions = vec![];

        while !self.is_match(&[Dedent]) {
            expressions.push(self.expression()?);

            self.consume(Semicolon, "Endline")?;
        }

        let value = expressions.pop().unwrap();

        if value.is_constexpr() {
            Ok(value)
        } else {
            Ok(Expression::Block {
                expressions,
                value: Box::new(value),
            })
        }
    }

    fn expression(&mut self) -> ParserResult<Expression> {
        self.function()
    }

    fn function(&mut self) -> ParserResult<Expression> {
        use TokenType::*;

        let expr = self.match_expr()?;

        if self.is_match(&[ParenOpen]) {
            let params = self.pattern_list()?;

            let return_type = if self.is_match(&[Arrow]) {
                self.advance();
                Some(self.expression()?)
            } else {
                None
            };

            self.consume(Equal, "Equal sign")?;

            let value = self.block()?;

            let name = match expr {
                Expression::Type(name) => name,
                _ => return Err(ParserError::InvalidAssignmentTarget(self.previous().line)),
            };

            let return_type = if let Some(return_type) = return_type {
                return_type
            } else {
                match value {
                    Expression::Block { value, .. } if value.is_constexpr() => *value,
                    _ => {
                        return Err(ParserError::MissingReturnType {
                            function: self.get_name(name).to_string(),
                            line: self.previous().line,
                        })
                    }
                }
            };

            Ok(Expression::Function(self.push_function(
                name,
                None,
                params,
                return_type,
                value,
                String::new(),
            )))
        } else {
            Ok(expr)
        }
    }

    fn match_expr(&mut self) -> ParserResult<Expression> {
        unimplemented!()
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

    fn consume_decorator(&mut self) -> ParserResult<String> {
        use TokenType::{Decorator, Semicolon};

        if self.is_match(&[Decorator]) {
            let token = self.peek().unwrap();

            let res = self.lexemes[token.start..token.end].join("");
            self.advance();
            self.consume(Semicolon, "Endline")?;
            Ok(res)
        } else {
            Ok(String::new())
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

    fn push_function(
        &mut self,
        name: NameIndex,
        class: Option<NameIndex>,
        params: Vec<Expression>,
        return_type: Expression,
        body: Expression,
        decorator: String,
    ) -> FunctionIndex {
        let index = FunctionIndex(self.functions.len());

        self.functions.push(Function {
            name,
            class,
            params,
            return_type,
            body,
            decorator,
        });

        index
    }

    fn next_if(&mut self, kinds: &[TokenType]) {
        if self.is_match(kinds) {
            self.advance();
        }
    }
}

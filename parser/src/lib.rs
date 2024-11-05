mod error;
mod expression;
mod function;
mod literal;
mod newtypes;
mod operator;
mod pattern;
mod statement;

pub use crate::literal::*;
pub use expression::Expression;
pub use function::Function;
pub use pattern::Pattern;
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

    pub fn get_literal(&self, index: LiteralIndex) -> &Literal {
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
        use TokenType::{Arrow, Dedent, Equal, Semicolon};

        let decorator = self.consume_decorator()?;

        let name = self.consume_name()?;

        let patterns = self.pattern_list()?;

        if self.is_match(&[Semicolon]) {
            Ok(Some(ClassState { name, patterns }))
        } else if self.is_match(&[Dedent]) {
            Ok(None)
        } else {
            let return_type = if self.is_match(&[Arrow]) {
                self.advance();

                Some(self.expression()?)
            } else {
                None
            };

            self.consume(Equal, "Equal sign")?;

            self.skip_if(&[Semicolon]);

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

    fn pattern_list(&mut self) -> ParserResult<Vec<Pattern>> {
        use TokenType::{Comma, ParenClose, ParenOpen, Semicolon};

        let mut patterns = Vec::new();

        if self.is_match(&[ParenOpen]) {
            self.skip_if(&[Semicolon]);

            patterns.push(self.pattern()?);

            while self.is_match(&[Comma]) {
                self.advance();
                self.skip_if(&[Semicolon]);

                patterns.push(self.pattern()?);
                self.skip_if(&[Semicolon]);
            }

            self.consume(ParenClose, "Right Paranthesis")?;
        }

        Ok(patterns)
    }

    fn pattern(&mut self) -> ParserResult<Pattern> {
        use TokenType::*;

        let (name, value) = if self.is_match(&[Wildcard, Identifier, Constant]) {
            let name = self.push_name(&self.peek().unwrap());

            if self.is_match(&[Colon]) {
                if self.is_match(&[Colon]) {
                    self.advance();
                }

                let value = self.expression()?;

                (Some(name), value)
            } else {
                (None, Expression::Type(name))
            }
        } else {
            (None, self.expression()?)
        };

        let condition = if self.is_match(&[When]) {
            Some(self.expression()?)
        } else {
            None
        };

        Ok(Pattern {
            name,
            value,
            condition,
        })
    }

    fn block(&mut self) -> ParserResult<Expression> {
        use TokenType::{Dedent, Semicolon};

        let mut expressions = vec![];

        while !self.is_match(&[Dedent]) {
            expressions.push(self.expression()?);

            self.consume(Semicolon, "Endline")?;
        }

        let value = expressions.pop().unwrap();

        if let Some(value) = self.eval_constexpr(&value)? {
            Ok(Expression::Literal(value))
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
        use TokenType::{Arrow, Dedent, Indent, Match, Semicolon};
        if self.is_match(&[Match]) {
            let scrutinee = self.expression()?;

            self.skip_if(&[Semicolon]);

            self.consume(Indent, "Indendation start")?;

            let arms = {
                let mut arms = vec![];

                while !self.is_match(&[Dedent]) {
                    let pattern = self.pattern()?;

                    self.consume(Arrow, "Arrow")?;

                    let expr = if self.is_match(&[Semicolon]) {
                        self.consume(Indent, "Endline")?;
                        self.block()?
                    } else {
                        self.expression()?
                    };

                    arms.push((pattern, expr));
                }

                arms
            };

            Ok(Expression::Match {
                scrutinee: Box::new(scrutinee),
                arms,
            })
        } else {
            self.or()
        }
    }

    fn or(&mut self) -> ParserResult<Expression> {
        use operator::Operator;
        use TokenType::Or;

        let mut left = self.and()?;

        while self.is_match(&[Or]) {
            self.advance();

            let right = self.and()?;

            left = Expression::Binary(Box::new(left), Operator::Or, Box::new(right));

            // if let Some(literal_index) = self.eval_constexpr(&left)? {
            //     left = Expression::Literal(literal_index)
            // }
        }

        Ok(left)
    }

    fn and(&mut self) -> ParserResult<Expression> {
        use TokenType::And;

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
        params: Vec<Pattern>,
        return_type: Option<Expression>,
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

    fn push_literal(&mut self, literal: Literal) -> LiteralIndex {
        self.literals.push(literal);

        LiteralIndex(self.literals.len())
    }

    fn skip_if(&mut self, kinds: &[TokenType]) {
        if self.is_match(kinds) {
            self.advance();
        }
    }

    // TODO: change self.literals as hashmap and remove unused items
    fn eval_constexpr(&mut self, expr: &Expression) -> ParserResult<Option<LiteralIndex>> {
        use operator::*;
        use Expression::*;

        Ok(match expr {
            Literal(literal_index) => Some(*literal_index),
            Grouping(expr) => self.eval_constexpr(expr)?,
            Binary(lhs, op, rhs) => {
                let Some(lhs) = self.eval_constexpr(lhs)? else {
                    return Ok(None);
                };
                let Some(rhs) = self.eval_constexpr(rhs)? else {
                    return Ok(None);
                };
                let lhs = self.get_literal(lhs);
                let rhs = self.get_literal(rhs);
                (match op {
                    Operator::Add => lhs + rhs,
                    Operator::Substract => lhs - rhs,
                    Operator::Multiply => lhs * rhs,
                    Operator::Divide => lhs / rhs,
                    Operator::Modulo => lhs % rhs,
                    Operator::And => lhs.and(rhs),
                    Operator::Or => lhs.or(rhs),
                    Operator::In => lhs.in_operator(rhs),
                    Operator::Equal => lhs.is_equal(rhs),
                    Operator::NotEqual => lhs.is_not_equal(rhs),
                    Operator::Greater => lhs.greater(rhs),
                    Operator::GreaterOrEqual => lhs.greater_equal(rhs),
                    Operator::Smaller => lhs.less(rhs),
                    Operator::SmallerOrEqual => lhs.less_equal(rhs),
                    _ => unreachable!(),
                })
                .map(|literal| self.push_literal(literal))
            }
            Unary(op, expression) => {
                let Some(expr) = self.eval_constexpr(expression)? else {
                    return Ok(None);
                };
                let expr = self.get_literal(expr);
                (match op {
                    Operator::Not => !expr,
                    Operator::Negate => -expr,
                    _ => unreachable!(),
                })
                .map(|literal| self.push_literal(literal))
            }
            IndexOperator(container, index) => {
                let Some(container) = self.eval_constexpr(container)? else {
                    return Ok(None);
                };
                let Some(index) = self.eval_constexpr(index)? else {
                    return Ok(None);
                };
                let container = self.get_literal(container);
                let index = self.get_literal(index);

                container
                    .get(index)
                    .map(|literal| self.push_literal(literal))
            }

            _ => unimplemented!(),
        })
    }
}

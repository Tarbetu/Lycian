mod class;
mod entity;
mod error;
mod expression;
mod function;
mod literal;
mod operator;
mod pattern;
mod statement;

use std::mem::swap;

pub use crate::literal::*;
pub use class::Class;
pub use entity::*;
pub use expression::Expression;
pub use statement::Statement;

pub use function::Function;
pub use pattern::*;

pub use error::ParserError;
pub use error::ParserResult;

use operator::Operator;
use scanner::{Token, TokenType};

use ahash::AHashMap;
use either::Either;
pub use scopeguard::guard;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct LiteralIndex(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ParsingMode {
    /// -> symbol will be parsed as function return type
    /// : symbol will be parsed as start of call block
    Normal,
    // -> symbol will be interpreted as match arm
    // : symbol will be interpreted as:
    //   start of match block,
    //   and name of pattern
    Pattern,
}

pub struct Parser<'a> {
    pub global_entity: Entity,
    pub entities: EntityTable,
    pub literals: AHashMap<LiteralIndex, Literal>,
    pub classes: AHashMap<EntityIndex, Class>,
    lexemes: Vec<&'a str>,
    tokens: Vec<Token>,
    position: usize,
    current_class_index: EntityIndex,
    current_declaration_index: EntityIndex,
    current_local_index: EntityIndex,
    current_methods: AHashMap<EntityIndex, Vec<Function>>,
    current_environment: AHashMap<EntityIndex, Vec<Function>>,
    parsing_mode: ParsingMode,
}

impl<'a> Parser<'a> {
    pub fn new(lexemes: Vec<&'a str>, tokens: Vec<Token>) -> Parser<'a> {
        Parser {
            lexemes,
            tokens,
            position: 0,
            global_entity: Entity::default(),
            entities: EntityTable::new(),
            classes: AHashMap::new(),
            current_class_index: EntityIndex(0),
            current_declaration_index: EntityIndex(0),
            current_local_index: EntityIndex(0),
            current_methods: AHashMap::new(),
            current_environment: AHashMap::new(),
            literals: AHashMap::new(),
            parsing_mode: ParsingMode::Normal,
        }
    }

    pub fn get_entity(&self, index: EntityIndex) -> &Entity {
        self.entities.get(&index).expect("Invalid name index")
    }

    pub fn get_literal(&self, index: LiteralIndex) -> &Literal {
        self.literals.get(&index).expect("Invalid literal index")
    }

    pub fn parse(&mut self) -> ParserResult<()> {
        while self.peek().is_some() {
            let program = self.class();

            match program {
                Ok(class) => {
                    self.classes.insert(class.name, class);
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }

        Ok(())
    }

    pub fn class(&mut self) -> ParserResult<Class> {
        use Statement::*;
        use TokenType::*;

        let decorator = self.consume_decorator()?;

        let name = self.consume_class_name()?;

        self.consume(Colon, "':'")?;
        self.consume_endline()?;
        self.consume(Indent, "Indendation start")?;

        let mut ancestors = Vec::new();

        if self.is_match(&[TokenType::Implementing]) {
            ancestors.push(self.consume_class_name()?);

            while let Some(TokenType::Implementing) = self.peek().map(|t| t.kind) {
                self.advance();
                ancestors.push(self.consume_class_name()?);
            }
        }

        let mut states = vec![];

        while !self.is_match(&[Dedent]) {
            self.skip_while(&[Endline]);
            if let Some(declaration) = self.declaration()? {
                match declaration {
                    state @ ClassState { .. } => states.push(state),
                    Method(_) => {}
                    _ => unreachable!(),
                };
            }
        }
        self.current_class_index = 0.into();
        self.current_declaration_index = 0.into();
        self.current_local_index = 0.into();

        let mut methods = AHashMap::new();
        swap(&mut self.current_methods, &mut methods);

        Ok(Class {
            name,
            ancestors,
            states,
            methods,
            decorator,
        })
    }

    fn declaration(&mut self) -> ParserResult<Option<Statement>> {
        use Statement::{ClassState, Method};
        use TokenType::{Arrow, Endline, Equal, ParenClose, ParenOpen};

        let mut parser = guard(self, |parser| {
            parser.current_declaration_index = 0.into();
            parser.current_local_index = 0.into();
        });

        let decorator = parser.consume_decorator()?;

        let name = parser.consume_declaration_name()?;

        let patterns = if parser.is_match(&[ParenOpen]) {
            parser.pattern_list(ParenClose, "Closing Paranthesis", PatternType::Argument)?
        } else {
            vec![]
        };

        if parser.is_match(&[Endline]) {
            Ok(Some(ClassState { name, patterns }))
        } else {
            let return_type = if parser.is_match(&[Arrow]) {
                Some(parser.or()?)
            } else {
                None
            };

            parser.consume(Equal, "Equal sign")?;

            let body = parser.block(false, Some("Method Definition"))?;

            let mut environment = AHashMap::new();
            swap(&mut parser.current_environment, &mut environment);

            parser.push_method(name, patterns, return_type, body, environment, decorator);
            Ok(Some(Method(name)))
        }
    }

    fn pattern_list(
        &mut self,
        end_with: TokenType,
        expected: &'static str,
        pattern_type: PatternType,
    ) -> ParserResult<Vec<Pattern>> {
        use TokenType::{Comma, Endline};

        let mut patterns = Vec::new();

        patterns.push(self.pattern(pattern_type)?);

        while self.is_match(&[Comma, Endline]) {
            patterns.push(self.pattern(pattern_type)?);
        }

        self.consume(end_with, expected)?;

        if patterns.len() >= 255 {
            return Err(ParserError::PatternListTooLong(self.previous().line));
        }

        Ok(patterns)
    }

    fn pattern(&mut self, pattern_type: PatternType) -> ParserResult<Pattern> {
        use Expression::{Call, ClassSelf};
        use TokenType::*;
        let parsing_mode_before = self.parsing_mode;
        self.parsing_mode = ParsingMode::Pattern;
        let mut parser = guard(self, |parser| parser.parsing_mode = parsing_mode_before);

        let first_expr = parser.expression()?;

        // If it contains : token, this is a name:value pattern
        if parser.is_match(&[Colon]) {
            let name = match first_expr {
                Call { name_id, .. } => PatternName::Name(name_id),
                ClassSelf => PatternName::ClassSelf,
                _ => {
                    return Err(ParserError::UnexpectedToken {
                        expected: "Name",
                        found: parser.previous().kind,
                        line: Some(parser.previous().line),
                    })
                }
            };

            // Parsing value
            let value = Some(parser.expression()?);

            // Parsing condition
            let condition = if parser.is_match(&[When]) {
                Some(parser.or()?)
            } else {
                None
            };

            Ok(Pattern {
                name,
                value,
                condition,
            })
        } else {
            let condition = if parser.is_match(&[When]) {
                Some(parser.or()?)
            } else {
                None
            };

            match (pattern_type, first_expr) {
                (PatternType::Argument, expr) => Ok(Pattern {
                    name: PatternName::NoName,
                    value: Some(expr),
                    condition,
                }),
                (
                    PatternType::Parameter,
                    Call {
                        name_id,
                        caller: None,
                        block: None,
                        args,
                    },
                ) if args.is_empty() => Ok(Pattern {
                    name: PatternName::Name(name_id),
                    value: None,
                    condition,
                }),
                _ => Err(ParserError::UnexpectedToken {
                    expected: "Name",
                    found: parser.previous().kind,
                    line: Some(parser.previous().line),
                }),
            }
        }
    }

    fn block(
        &mut self,
        params_expected: bool,
        context_info: Option<&'static str>,
    ) -> ParserResult<Expression> {
        use TokenType::{Dedent, Endline, Indent, Pipe};

        let params = if self.is_match(&[Pipe]) {
            self.pattern_list(Pipe, "| symbol", PatternType::Parameter)?
        } else {
            vec![]
        };

        // Remove this concept, it's blocks the lambda
        // Or is there ambiguity?
        if !(params_expected || params.is_empty()) {
            return Err(ParserError::UnexpectedBlockParams(
                self.previous().line,
                context_info.unwrap(),
            ));
        }

        self.skip_while(&[Endline]);
        let indented_block = self.is_match(&[Indent]);

        let mut expressions = vec![];

        if indented_block {
            while !self.is_match(&[Dedent]) {
                expressions.push(self.expression()?);

                self.skip_while(&[Endline]);
            }
        } else {
            expressions.push(self.expression()?);

            self.skip_while(&[Endline]);
        }

        let value = expressions.pop().unwrap();

        if let Some(value) = self.eval_constexpr(&value)? {
            Ok(value)
        } else if expressions.is_empty() && params.is_empty() {
            Ok(value)
        } else {
            Ok(Expression::Block {
                expressions,
                value: Box::new(value),
                params,
            })
        }
    }

    fn expression(&mut self) -> ParserResult<Expression> {
        self.function()
    }

    fn function(&mut self) -> ParserResult<Expression> {
        use TokenType::*;
        let mut parser = guard(self, |parser| parser.current_local_index = 0.into());

        let name_token = parser.peek();
        let mut expr = parser.lambda()?;
        let params = if parser.is_match(&[ParenOpen]) {
            parser.pattern_list(ParenClose, "Closing Paranthesis", PatternType::Parameter)?
        } else {
            vec![]
        };

        let return_type = if parser.parsing_mode == ParsingMode::Normal && parser.is_match(&[Arrow])
        {
            Some(parser.or()?)
        } else {
            None
        };

        if parser.is_match(&[Equal]) {
            let value = parser.block(false, Some("Function Definition"))?;

            let Some(name_token) = name_token else {
                return Err(ParserError::UnexpectedToken {
                    expected: "Local Name",
                    found: TokenType::Eof,
                    line: None,
                });
            };
            let name = parser.consume_local_name(name_token)?;

            parser.push_function(name, params, return_type, value, String::new());

            expr = Expression::Function(name);
        }

        Ok(expr)
    }

    fn lambda(&mut self) -> ParserResult<Expression> {
        use TokenType::Pipe;

        if self.is_match(&[Pipe]) {
            let params = self.pattern_list(Pipe, "| symbol", PatternType::Parameter)?;
            let expr = self.expression()?;

            Ok(Expression::Block {
                value: Box::new(expr),
                expressions: vec![],
                params,
            })
        } else {
            self.match_expr()
        }
    }

    fn match_expr(&mut self) -> ParserResult<Expression> {
        use TokenType::{Arrow, Colon, Dedent, Endline, Indent, Match};
        if self.is_match(&[Match]) {
            let parsing_mode_before = self.parsing_mode;
            self.parsing_mode = ParsingMode::Pattern;
            let mut parser = guard(self, |parser| parser.parsing_mode = parsing_mode_before);
            let scrutinee = parser.expression()?;

            parser.consume(Colon, ": symbol")?;
            parser.consume_endline()?;
            parser.consume(Indent, "Indendation start")?;

            let arms = {
                let mut arms = vec![];

                while !parser.is_match(&[Dedent]) {
                    parser.skip_while(&[Endline]);
                    let pattern = parser.pattern(PatternType::Argument)?;

                    parser.consume(Arrow, "Arrow")?;

                    let expr = parser.block(true, None)?;

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
        use TokenType::Or;

        let mut left = self.and()?;

        while self.is_match(&[Or]) {
            let right = self.and()?;

            left = Expression::Binary(Box::new(left), Operator::Or, Box::new(right));
        }

        Ok(left)
    }

    fn and(&mut self) -> ParserResult<Expression> {
        use TokenType::And;

        let mut left = self.equality()?;

        while self.is_match(&[And]) {
            let right = self.equality()?;

            left = Expression::Binary(Box::new(left), Operator::And, Box::new(right));
        }

        Ok(left)
    }

    fn equality(&mut self) -> ParserResult<Expression> {
        use TokenType::{EqualEqual, NotEqual};

        let mut left = self.comparison()?;

        while self.is_match(&[EqualEqual, NotEqual]) {
            let operator = match self.previous().kind {
                EqualEqual => Operator::Equal,
                NotEqual => Operator::NotEqual,
                _ => unreachable!(),
            };

            let right = self.comparison()?;

            left = Expression::Binary(Box::new(left), operator, Box::new(right));
        }

        Ok(left)
    }

    fn comparison(&mut self) -> ParserResult<Expression> {
        use TokenType::{Greater, GreaterEqual, Less, LessEqual};

        let mut left = self.term()?;

        while self.is_match(&[Greater, GreaterEqual, Less, LessEqual]) {
            let operator = match self.previous().kind {
                Greater => Operator::Greater,
                GreaterEqual => Operator::GreaterOrEqual,
                Less => Operator::Smaller,
                LessEqual => Operator::SmallerOrEqual,
                _ => unreachable!(),
            };

            let right = self.term()?;

            left = Expression::Binary(Box::new(left), operator, Box::new(right));
        }

        Ok(left)
    }

    fn term(&mut self) -> ParserResult<Expression> {
        use TokenType::{Minus, Plus};

        let mut left = self.factor()?;

        while self.is_match(&[Plus, Minus]) {
            let operator = match self.previous().kind {
                Plus => Operator::Add,
                Minus => Operator::Substract,
                _ => unreachable!(),
            };

            let right = self.factor()?;

            left = Expression::Binary(Box::new(left), operator, Box::new(right));
        }

        Ok(left)
    }

    fn factor(&mut self) -> ParserResult<Expression> {
        use TokenType::{Percent, Slash, Star};

        let mut left = self.unary()?;

        while self.is_match(&[Star, Slash, Percent]) {
            let operator = match self.previous().kind {
                Star => Operator::Multiply,
                Slash => Operator::Divide,
                Percent => Operator::Modulo,
                _ => unreachable!(),
            };

            let right = self.unary()?;
            left = Expression::Binary(Box::new(left), operator, Box::new(right));
        }

        Ok(left)
    }

    fn unary(&mut self) -> ParserResult<Expression> {
        use TokenType::{Minus, Not};

        if self.is_match(&[Not, Minus]) {
            let operator = match self.previous().kind {
                Not => Operator::Not,
                Minus => Operator::Negate,
                _ => unreachable!(),
            };

            let right = self.unary()?;

            Ok(Expression::Unary(operator, Box::new(right)))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ParserResult<Expression> {
        use Expression::Call;
        use TokenType::{Colon, Dot, ParenClose, ParenOpen};
        let mut expr = self.primary()?;

        loop {
            if self.is_match(&[ParenOpen]) {
                let args = if self.is_match(&[ParenClose]) {
                    vec![]
                } else {
                    self.pattern_list(ParenClose, "Closing Paranthesis", PatternType::Argument)?
                };

                expr = match expr {
                    Call {
                        caller, name_id, ..
                    } => Call {
                        caller,
                        name_id,
                        args,
                        block: None,
                    },
                    _ => {
                        return Err(ParserError::UnexpectedToken {
                            expected: "CallRoot",
                            found: self.peek().map(|t| t.kind).unwrap_or(TokenType::Eof),
                            line: self.peek().map(|t| t.line),
                        })
                    }
                };
            } else if self.is_match(&[Dot]) {
                let Call { name_id, .. } = self.primary()? else {
                    return Err(ParserError::UnexpectedToken {
                        expected: "CallRoot",
                        found: self.peek().map(|t| t.kind).unwrap_or(TokenType::Eof),
                        line: self.peek().map(|t| t.line),
                    });
                };

                expr = Call {
                    caller: Some(Box::new(expr)),
                    name_id,
                    args: vec![],
                    block: None,
                };
            } else {
                if self.parsing_mode == ParsingMode::Normal && self.is_match(&[Colon]) {
                    let block = Some(Box::new(self.block(true, None)?));

                    match expr {
                        Call {
                            caller,
                            name_id,
                            args,
                            block: _,
                        } => {
                            expr = Call {
                                caller,
                                name_id,
                                args,
                                block,
                            }
                        }
                        _ => {
                            return Err(ParserError::UnexpectedBlock(
                                self.previous().line,
                                self.previous().kind,
                            ));
                        }
                    };
                };

                break;
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> ParserResult<Expression> {
        use TokenType::{ClassSelf, Constant, Identifier, ParenClose, ParenOpen, Super, Wildcard};

        Ok(if self.is_match(&[ParenOpen]) {
            let expr = self.expression()?;
            self.consume(ParenClose, "End of Paranthesis")?;
            Expression::Grouping(Box::new(expr))
        } else if self.is_match(&[ClassSelf]) {
            Expression::ClassSelf
        } else if self.is_match(&[Super]) {
            Expression::Super
        } else if self.is_match(&[Constant, Identifier, Wildcard]) {
            Expression::Call {
                // It's expensive to allocate names for every call
                name_id: self.consume_call_name(self.previous())?,
                block: None,
                caller: None,
                args: vec![],
            }
        } else {
            Expression::Literal(self.catch_literal()?)
        })
    }

    fn catch_literal(&mut self) -> ParserResult<LiteralIndex> {
        use TokenType::*;

        let res = if self.is_match(&[True]) {
            Literal::Boolean(true)
        } else if self.is_match(&[False]) {
            Literal::Boolean(false)
        } else if self.is_match(&[Integer, Float]) {
            let token = self.previous();
            debug_assert!(token.kind == TokenType::Integer || token.kind == TokenType::Float);
            let incomplete =
                rug::Float::parse(self.lexemes[token.start..token.end].join("")).unwrap();
            let number = rug::Float::with_val(literal::PRECISION, incomplete);

            match token.kind {
                Float => Literal::Float(number),
                Integer => Literal::Integer(number),
                _ => unreachable!(),
            }
        } else if self.is_match(&[Str]) {
            let token = self.previous();
            debug_assert!(token.kind == TokenType::Str);
            let string = self.lexemes[token.start..token.end].join("");
            Literal::Str(string)
        } else if self.is_match(&[BraceOpen]) {
            unimplemented!()
        } else if self.is_match(&[BracketOpen]) {
            if self.is_match(&[BracketClose]) {
                Literal::LiteralList(vec![])
            } else {
                let mut list = vec![];

                let first_expr = self.expression()?;

                list.push(match self.eval_constexpr(&first_expr)? {
                    Some(Expression::Literal(index)) => {
                        let literal = self.literals.remove(&index).unwrap();
                        Either::Right(literal)
                    }
                    Some(expr) => Either::Left(expr),
                    None => Either::Left(first_expr),
                });

                while self.is_match(&[Comma]) {
                    let expr = self.expression()?;

                    list.push(match self.eval_constexpr(&expr)? {
                        Some(Expression::Literal(index)) => {
                            let literal = self.literals.remove(&index).unwrap();
                            Either::Right(literal)
                        }
                        Some(expr) => Either::Left(expr),
                        None => Either::Left(expr),
                    });
                }

                self.consume(BracketClose, "End of List")?;

                Literal::LiteralList(list)
            }
        } else {
            return Err(ParserError::UnexpectedToken {
                expected: "Expression",
                found: self.peek().unwrap().kind,
                line: self.peek().map(|t| t.line),
            });
        };

        let literal_index = self.next_literal_index();
        self.push_literal(literal_index, res);
        Ok(literal_index)
    }

    fn peek(&self) -> Option<Token> {
        self.look_at(self.position)
    }

    fn previous(&self) -> Token {
        self.look_at(self.position - 1).unwrap()
    }

    fn look_at(&self, position: usize) -> Option<Token> {
        match self.tokens.get(position) {
            Some(token) if token.kind == TokenType::Eof => None,
            Some(token) => Some(*token),
            None => None,
        }
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
            let found = self.peek().map(|t| t.kind).unwrap_or(TokenType::Eof);
            let line = self.peek().map(|t| t.line);

            return Err(ParserError::UnexpectedToken {
                expected,
                found,
                line,
            });
        }

        Ok(())
    }

    fn consume_endline(&mut self) -> ParserResult<()> {
        use TokenType::{Endline, Eof};

        if !(self.is_match(&[Eof, Endline]) || self.peek().is_none()) {
            let found = self.peek().map(|t| t.kind).unwrap_or(TokenType::Eof);
            let line = self.peek().map(|t| t.line);

            return Err(ParserError::UnexpectedToken {
                expected: "End of Line",
                found,
                line,
            });
        }

        Ok(())
    }

    fn consume_class_name(&mut self) -> ParserResult<EntityIndex> {
        self.advance();
        let name = self.create_entity(self.previous(), EntityKind::Class, "Class Name")?;
        let name_index = EntityIndex(self.entities.len());
        self.entities.insert(name_index, name);
        self.global_entity.sub_entities.push(name_index);
        self.current_class_index = name_index;
        Ok(name_index)
    }

    fn consume_declaration_name(&mut self) -> ParserResult<EntityIndex> {
        self.advance();
        let name =
            self.create_entity(self.previous(), EntityKind::Declaration, "Declaration Name")?;
        let name_index = EntityIndex(self.entities.len());
        let Some(class_entity) = self.entities.get_mut(&self.current_class_index) else {
            panic!(
                "Current Class Index {:?} doesn't exist!",
                self.current_class_index
            )
        };
        class_entity.sub_entities.push(name_index);
        self.entities.insert(name_index, name);
        self.current_declaration_index = name_index;
        Ok(name_index)
    }

    fn consume_local_name(&mut self, token: Token) -> ParserResult<EntityIndex> {
        let name = self.create_entity(token, EntityKind::Local, "Local Name")?;
        let name_index = EntityIndex(self.entities.len());
        let Some(declaration_entity) = self.entities.get_mut(&self.current_declaration_index)
        else {
            panic!(
                "Current Declaration Index {:?} doesn't exist!",
                self.current_declaration_index
            )
        };
        declaration_entity.sub_entities.push(name_index);
        self.entities.insert(name_index, name);
        self.current_local_index = name_index;
        Ok(name_index)
    }

    fn consume_call_name(&mut self, token: Token) -> ParserResult<EntityIndex> {
        let name = self.create_entity(token, EntityKind::Call, "CallRoot")?;
        let name_index = EntityIndex(self.entities.len());
        let index = if self.current_local_index == EntityIndex(0) {
            self.current_declaration_index
        } else {
            self.current_local_index
        };
        let Some(entity) = self.entities.get_mut(&index) else {
            panic!("Current Index {:?} doesn't exist!", index)
        };
        entity.sub_entities.push(name_index);
        self.entities.insert(name_index, name);
        Ok(name_index)
    }

    fn consume_decorator(&mut self) -> ParserResult<String> {
        use TokenType::Decorator;

        if self.is_match(&[Decorator]) {
            let token = self.peek().unwrap();

            let res = self.lexemes[token.start..token.end].join("");
            self.advance();
            self.consume_endline()?;
            Ok(res)
        } else {
            Ok(String::new())
        }
    }

    fn create_entity(
        &mut self,
        token: Token,
        kind: EntityKind,
        expected: &'static str,
    ) -> ParserResult<Entity> {
        use ParserError::UnexpectedToken;
        use TokenType::{Constant, Identifier, Wildcard};

        match token.kind {
            Constant | Identifier | Wildcard => {
                let name = self.lexemes[token.start..token.end].join("");
                let visibility = Visibility::from(token.kind);

                Ok(Entity {
                    name,
                    visibility,
                    kind,
                    sub_entities: vec![],
                })
            }
            _ => Err(UnexpectedToken {
                expected,
                found: token.kind,
                line: Some(token.line),
            }),
        }
    }

    fn push_literal(&mut self, index: LiteralIndex, literal: Literal) {
        self.literals.insert(index, literal);
    }

    fn next_literal_index(&self) -> LiteralIndex {
        self.literals
            .keys()
            .max()
            .map(|index| LiteralIndex(index.0 + 1))
            .unwrap_or(LiteralIndex(0))
    }

    fn push_method(
        &mut self,
        name: EntityIndex,
        params: Vec<Pattern>,
        return_type: Option<Expression>,
        body: Expression,
        environment: AHashMap<EntityIndex, Vec<Function>>,
        decorator: String,
    ) {
        let function = Function {
            name,
            params,
            return_type,
            body,
            environment: Some(environment),
            decorator,
        };
        if let Some(functions) = self.current_methods.get_mut(&name) {
            functions.push(function);
        } else {
            self.current_methods.insert(name, vec![function]);
        }
    }

    fn push_function(
        &mut self,
        name: EntityIndex,
        params: Vec<Pattern>,
        return_type: Option<Expression>,
        body: Expression,
        decorator: String,
    ) {
        let function = Function {
            name,
            params,
            return_type,
            body,
            environment: None,
            decorator,
        };
        if let Some(functions) = self.current_environment.get_mut(&name) {
            functions.push(function);
        } else {
            self.current_environment.insert(name, vec![function]);
        }
    }

    fn skip_while(&mut self, kinds: &[TokenType]) {
        while kinds.contains(&self.peek().map(|t| t.kind).unwrap_or(TokenType::Eof)) {
            self.advance();
        }
    }

    fn eval_constexpr(&mut self, expr: &Expression) -> ParserResult<Option<Expression>> {
        use operator::*;
        use Expression::*;

        let next_literal_index = self.next_literal_index();
        Ok(match expr {
            Literal(index) => Some(Expression::Literal(*index)),
            Grouping(expr) => self.eval_constexpr(expr)?,
            Block {
                expressions,
                value,
                params,
            } if expressions.is_empty() && params.is_empty() => self.eval_constexpr(value)?,
            Binary(lhs, op, rhs) => {
                let Some(Literal(lhs_index)) = self.eval_constexpr(lhs)? else {
                    return Ok(None);
                };
                let Some(Literal(rhs_index)) = self.eval_constexpr(rhs)? else {
                    return Ok(None);
                };
                let lhs = self.get_literal(lhs_index);
                let rhs = self.get_literal(rhs_index);
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
                .map(|literal| {
                    self.literals.remove(&lhs_index);
                    self.literals.remove(&rhs_index);
                    self.push_literal(next_literal_index, literal);
                    Literal(next_literal_index)
                })
            }
            Unary(op, expression) => {
                let Some(Literal(expr_index)) = self.eval_constexpr(expression)? else {
                    return Ok(None);
                };
                let expr = self.get_literal(expr_index);
                (match op {
                    Operator::Not => !expr,
                    Operator::Negate => -expr,
                    _ => unreachable!(),
                })
                .map(|literal| {
                    self.literals.remove(&expr_index);
                    self.push_literal(next_literal_index, literal);
                    Literal(next_literal_index)
                })
            }
            IndexOperator(container, search) => {
                let Some(Literal(container_index)) = self.eval_constexpr(container)? else {
                    return Ok(None);
                };
                let Some(Literal(search_index)) = self.eval_constexpr(search)? else {
                    return Ok(None);
                };
                let container = self.get_literal(container_index);
                let search = self.get_literal(search_index);

                container.get(search).map(|literal| {
                    self.literals.remove(&container_index);
                    self.literals.remove(&search_index);
                    match literal {
                        Either::Left(expr) => expr,
                        Either::Right(literal) => {
                            self.push_literal(next_literal_index, literal);
                            Literal(next_literal_index)
                        }
                    }
                })
            }

            _ => None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use scanner::Scanner;
    use unicode_segmentation::UnicodeSegmentation;

    fn initialize_parser(source: &str) -> Parser {
        let graphemes = source.graphemes(true).collect::<Vec<&str>>();
        let tokens = Scanner::new(source, false).scan();
        Parser::new(graphemes, tokens)
    }

    fn parse(source: &str) -> Parser {
        let mut parser = initialize_parser(source);
        parser.parse().unwrap();

        parser
    }

    fn create_number(number: f64) -> rug::Float {
        rug::Float::with_val(literal::PRECISION, number)
    }

    #[test]
    fn parse_simple_class() {
        let source = "
Program:
    Main = 42
";

        let result = parse(source);
        assert_eq!(
            result.get_entity(EntityIndex(1)),
            &Entity::Public("Program".to_string())
        );
        assert_eq!(
            result.get_literal(LiteralIndex(0)),
            &Literal::Integer(create_number(42.0))
        );
        let main_method = result
            .classes
            .get(&EntityIndex(1))
            .unwrap()
            .methods
            .get(&EntityIndex(0))
            .map(|methods| methods.first().unwrap())
            .unwrap();
        assert_eq!(main_method.params, vec![]);
        assert_eq!(main_method.return_type, None);
        assert_eq!(main_method.environment.as_ref().map(|e| e.len()), Some(0));
        assert_eq!(main_method.body, Expression::Literal(LiteralIndex(0)));
        assert!(main_method.decorator.is_empty());
    }

    #[test]
    fn parse_simple_class_with_constexpr() {
        let source = "
Program:
    Main = 420 + 69
";

        let result = parse(source);
        assert_eq!(
            result.get_entity(EntityIndex(1)),
            &Entity::Public("Program".to_string())
        );
        assert_eq!(
            result.get_literal(LiteralIndex(2)),
            &Literal::Integer(create_number(420.0 + 69.0))
        );
        let main_method = result
            .classes
            .get(&EntityIndex(1))
            .unwrap()
            .methods
            .get(&EntityIndex(0))
            .map(|methods| methods.first().unwrap())
            .unwrap();
        assert_eq!(main_method.params, vec![]);
        assert_eq!(main_method.return_type, None);
        assert_eq!(main_method.environment.as_ref().map(|e| e.len()), Some(0));
        assert_eq!(main_method.body, Expression::Literal(LiteralIndex(2)));
        assert!(main_method.decorator.is_empty());
    }

    #[test]
    fn parse_add() {
        let mut parser = initialize_parser("420 + 69");
        let result = parser.term().unwrap();

        assert_eq!(
            parser.get_literal(LiteralIndex(0)),
            &Literal::Integer(create_number(420.0))
        );
        assert_eq!(
            parser.get_literal(LiteralIndex(1)),
            &Literal::Integer(create_number(69.0))
        );

        assert_eq!(
            result,
            Expression::Binary(
                Box::new(Expression::Literal(LiteralIndex(0))),
                Operator::Add,
                Box::new(Expression::Literal(LiteralIndex(1)))
            )
        );

        assert_eq!(
            parser.eval_constexpr(&result).unwrap(),
            Some(Expression::Literal(LiteralIndex(2)))
        );

        assert_eq!(
            parser.get_literal(LiteralIndex(2)),
            &Literal::Integer(create_number(420.0 + 69.0))
        );
    }

    #[test]
    fn parse_minus() {
        let mut parser = initialize_parser("420 - 69");
        let result = parser.term().unwrap();

        assert_eq!(
            parser.get_literal(LiteralIndex(0)),
            &Literal::Integer(create_number(420.0))
        );
        assert_eq!(
            parser.get_literal(LiteralIndex(1)),
            &Literal::Integer(create_number(69.0))
        );

        assert_eq!(
            result,
            Expression::Binary(
                Box::new(Expression::Literal(LiteralIndex(0))),
                Operator::Substract,
                Box::new(Expression::Literal(LiteralIndex(1)))
            )
        );

        assert_eq!(
            parser.eval_constexpr(&result).unwrap(),
            Some(Expression::Literal(LiteralIndex(2)))
        );

        assert_eq!(
            parser.get_literal(LiteralIndex(2)),
            &Literal::Integer(create_number(420.0 - 69.0))
        );
    }

    #[test]
    fn parse_multiply() {
        let mut parser = initialize_parser("420 * 69");
        let result = parser.term().unwrap();

        assert_eq!(
            parser.get_literal(LiteralIndex(0)),
            &Literal::Integer(create_number(420.0))
        );
        assert_eq!(
            parser.get_literal(LiteralIndex(1)),
            &Literal::Integer(create_number(69.0))
        );

        assert_eq!(
            result,
            Expression::Binary(
                Box::new(Expression::Literal(LiteralIndex(0))),
                Operator::Multiply,
                Box::new(Expression::Literal(LiteralIndex(1)))
            )
        );

        assert_eq!(
            parser.eval_constexpr(&result).unwrap(),
            Some(Expression::Literal(LiteralIndex(2)))
        );

        assert_eq!(
            parser.get_literal(LiteralIndex(2)),
            &Literal::Integer(create_number(420.0 * 69.0))
        );
    }

    #[test]
    fn parse_divide() {
        let mut parser = initialize_parser("420 / 69");
        let result = parser.term().unwrap();

        assert_eq!(
            parser.get_literal(LiteralIndex(0)),
            &Literal::Integer(create_number(420.0))
        );
        assert_eq!(
            parser.get_literal(LiteralIndex(1)),
            &Literal::Integer(create_number(69.0))
        );

        assert_eq!(
            result,
            Expression::Binary(
                Box::new(Expression::Literal(LiteralIndex(0))),
                Operator::Divide,
                Box::new(Expression::Literal(LiteralIndex(1)))
            )
        );

        assert_eq!(
            parser.eval_constexpr(&result).unwrap(),
            Some(Expression::Literal(LiteralIndex(2)))
        );

        assert_eq!(
            parser.get_literal(LiteralIndex(2)),
            &Literal::Float(create_number(420.0 / 69.0))
        );
    }

    #[test]
    fn parse_modulo() {
        let mut parser = initialize_parser("420 % 69");
        let result = parser.term().unwrap();

        assert_eq!(
            parser.get_literal(LiteralIndex(0)),
            &Literal::Integer(create_number(420.0))
        );
        assert_eq!(
            parser.get_literal(LiteralIndex(1)),
            &Literal::Integer(create_number(69.0))
        );

        assert_eq!(
            result,
            Expression::Binary(
                Box::new(Expression::Literal(LiteralIndex(0))),
                Operator::Modulo,
                Box::new(Expression::Literal(LiteralIndex(1)))
            )
        );

        assert_eq!(
            parser.eval_constexpr(&result).unwrap(),
            Some(Expression::Literal(LiteralIndex(2)))
        );

        assert_eq!(
            parser.get_literal(LiteralIndex(2)),
            &Literal::Integer(create_number(420.0 % 69.0))
        );
    }

    #[test]
    fn parse_negate() {
        let mut parser = initialize_parser("-31");
        let result = parser.unary().unwrap();

        assert_eq!(
            parser.get_literal(LiteralIndex(0)),
            &Literal::Integer(create_number(31.0))
        );

        assert_eq!(
            result,
            Expression::Unary(
                Operator::Negate,
                Box::new(Expression::Literal(LiteralIndex(0))),
            )
        );
        assert_eq!(
            parser.eval_constexpr(&result).unwrap(),
            Some(Expression::Literal(LiteralIndex(1)))
        );

        assert_eq!(
            parser.get_literal(LiteralIndex(1)),
            &Literal::Integer(create_number(-31.0))
        );
    }

    #[test]
    fn parse_grouping() {
        let mut parser = initialize_parser("(420 + 69) * 2");
        let result = parser.factor().unwrap();

        assert_eq!(
            parser.get_literal(LiteralIndex(0)),
            &Literal::Integer(create_number(420.0))
        );

        assert_eq!(
            parser.get_literal(LiteralIndex(1)),
            &Literal::Integer(create_number(69.0))
        );

        assert_eq!(
            parser.get_literal(LiteralIndex(2)),
            &Literal::Integer(create_number(2.0))
        );

        assert_eq!(
            result,
            Expression::Binary(
                Box::new(Expression::Grouping(Box::new(Expression::Binary(
                    Box::new(Expression::Literal(LiteralIndex(0))),
                    Operator::Add,
                    Box::new(Expression::Literal(LiteralIndex(1)))
                )))),
                Operator::Multiply,
                Box::new(Expression::Literal(LiteralIndex(2))),
            )
        );
        assert_eq!(
            parser.eval_constexpr(&result).unwrap(),
            Some(Expression::Literal(LiteralIndex(3)))
        );

        assert_eq!(
            parser.get_literal(LiteralIndex(3)),
            &Literal::Integer(create_number((420.0 + 69.0) * 2.0))
        );
    }

    #[test]
    fn parse_simply_call() {
        let mut parser = initialize_parser("call");
        let result = parser.call().unwrap();

        assert_eq!(
            result,
            Expression::Call {
                name_id: EntityIndex(1),
                block: None,
                args: vec![],
                caller: None
            }
        )
    }

    #[test]
    fn parse_simply_call_with_redundant_paranthesis() {
        let mut parser = initialize_parser("call()");
        let result = parser.call().unwrap();

        assert_eq!(
            result,
            Expression::Call {
                name_id: EntityIndex(1),
                block: None,
                args: vec![],
                caller: None
            }
        )
    }

    #[test]
    fn parse_call_with_args() {
        let mut parser = initialize_parser("call(5)");
        let result = parser.call().unwrap();

        assert_eq!(
            result,
            Expression::Call {
                name_id: EntityIndex(1),
                block: None,
                args: vec![Pattern {
                    value: Some(Expression::Literal(LiteralIndex(0))),
                    name: PatternName::NoName,
                    condition: None
                }],
                caller: None
            }
        )
    }

    #[test]
    fn parse_simply_call_with_block() {
        use Expression::Call;
        let source = "
call:
    block
";
        let mut parser = initialize_parser(source);
        let result = parser.call().unwrap();

        assert_eq!(
            result,
            Call {
                name_id: EntityIndex(1),
                block: Some(Box::new(Call {
                    name_id: EntityIndex(2),
                    block: None,
                    args: vec![],
                    caller: None
                })),
                args: vec![],
                caller: None
            }
        )
    }

    #[test]
    fn parse_method_call() {
        use Expression::Call;
        let mut parser = initialize_parser("function.method");
        let result = parser.call().unwrap();

        assert_eq!(
            result,
            Call {
                caller: Some(Box::new(Call {
                    name_id: EntityIndex(1),
                    block: None,
                    args: vec![],
                    caller: None
                })),
                name_id: EntityIndex(2),
                args: vec![],
                block: None
            }
        )
    }

    #[test]
    fn parse_method_call_with_empty_paranthesis() {
        use Expression::Call;
        let mut parser = initialize_parser("function().method()");
        let result = parser.call().unwrap();

        assert_eq!(
            result,
            Call {
                caller: Some(Box::new(Call {
                    name_id: EntityIndex(1),
                    block: None,
                    args: vec![],
                    caller: None
                })),
                name_id: EntityIndex(2),
                args: vec![],
                block: None
            }
        )
    }

    #[test]
    fn parse_method_call_with_same_name_with_method() {
        use Expression::Call;
        let mut parser = initialize_parser("call.call");
        let result = parser.call().unwrap();

        assert_eq!(
            result,
            Call {
                caller: Some(Box::new(Call {
                    name_id: EntityIndex(1),
                    block: None,
                    args: vec![],
                    caller: None
                })),
                name_id: EntityIndex(2),
                args: vec![],
                block: None
            }
        )
    }

    #[test]
    fn parse_function_assignment() {
        let mut parser = initialize_parser("result = 0 * 2");
        let result = parser.expression().unwrap();

        assert_eq!(result, Expression::Function(EntityIndex(2)));

        assert_eq!(
            parser
                .current_environment
                .get(&EntityIndex(2))
                .unwrap()
                .first()
                .unwrap(),
            &Function {
                name: EntityIndex(2),
                params: vec![],
                return_type: None,
                environment: None,
                decorator: String::new(),
                body: Expression::Literal(LiteralIndex(2))
            }
        );

        assert_eq!(
            parser.get_literal(LiteralIndex(2)),
            &Literal::Integer(create_number(0.0))
        );
    }

    #[test]
    fn parse_map_example() {
        use Expression::{Block, Call};
        let source = "
result = [1, 2, 3, 4, 5].map: |i|
    i * 2
";
        let mut parser = initialize_parser(source);
        let result = parser.expression().unwrap();

        assert_eq!(result, Expression::Function(EntityIndex(5)));

        assert_eq!(
            parser
                .current_environment
                .get(&EntityIndex(5))
                .unwrap()
                .first()
                .unwrap(),
            &Function {
                name: EntityIndex(5),
                params: vec![],
                return_type: None,
                environment: None,
                decorator: String::new(),
                body: Call {
                    caller: Some(Box::new(Expression::Literal(LiteralIndex(0)))),
                    name_id: EntityIndex(2),
                    args: vec![],
                    block: Some(Box::new(Block {
                        expressions: vec![],
                        value: Box::new(Expression::Binary(
                            Box::new(Call {
                                name_id: EntityIndex(4),
                                caller: None,
                                args: vec![],
                                block: None
                            }),
                            Operator::Multiply,
                            Box::new(Expression::Literal(LiteralIndex(1)))
                        )),
                        params: vec![Pattern {
                            name: PatternName::Name(EntityIndex(3)),
                            value: None,
                            condition: None
                        }]
                    }))
                }
            }
        )
    }

    #[test]
    fn test_class_with_method() {
        let source = "
Program:
    multiply_with_five(x: List(Integer)) -> List(Integer) =
        x.map: |i|
            self.multiply_with_five(i)
";
        let mut parser = initialize_parser(source);
        parser.parse().unwrap();

        let class = parser.classes.get(&EntityIndex(1)).unwrap();

        let mut methods = AHashMap::new();
        methods.insert(
            EntityIndex(2),
            vec![Function {
                name: EntityIndex(2),
                environment: Some(AHashMap::new()),
                decorator: String::new(),
                params: vec![Pattern {
                    name: PatternName::Name(EntityIndex(3)),
                    value: Some(Expression::Call {
                        name_id: EntityIndex(4),
                        block: None,
                        args: vec![Pattern {
                            name: PatternName::NoName,
                            value: Some(Expression::Call {
                                caller: None,
                                name_id: EntityIndex(5),
                                args: vec![],
                                block: None,
                            }),
                            condition: None,
                        }],
                        caller: None,
                    }),
                    condition: None,
                }],
                return_type: Some(Expression::Call {
                    name_id: EntityIndex(6),
                    block: None,
                    args: vec![Pattern {
                        name: PatternName::NoName,
                        value: Some(Expression::Call {
                            caller: None,
                            name_id: EntityIndex(7),
                            args: vec![],
                            block: None,
                        }),
                        condition: None,
                    }],
                    caller: None,
                }),
                body: Expression::Call {
                    name_id: EntityIndex(9),
                    caller: Some(Box::new(Expression::Call {
                        caller: None,
                        name_id: EntityIndex(8),
                        args: vec![],
                        block: None,
                    })),
                    args: vec![],
                    block: Some(Box::new(Expression::Block {
                        expressions: vec![],
                        params: vec![Pattern {
                            name: PatternName::Name(EntityIndex(10)),
                            value: None,
                            condition: None,
                        }],
                        value: Box::new(Expression::Call {
                            caller: Some(Box::new(Expression::ClassSelf)),
                            name_id: EntityIndex(11),
                            block: None,
                            args: vec![Pattern {
                                name: PatternName::NoName,
                                value: Some(Expression::Call {
                                    caller: None,
                                    name_id: EntityIndex(12),
                                    args: vec![],
                                    block: None,
                                }),
                                condition: None,
                            }],
                        }),
                    })),
                },
            }],
        );

        assert_eq!(class.methods, methods)
    }

    #[test]
    fn test_fullfillied_class() {
        let source = "
Program:
    implementing FiftiestFive

    take_five -> Integer = 5

    multiply_with_five(x: Integer) -> Integer =
        x * self.take_five

    multiply_with_five(x: List(Integer)) -> List(Integer) =
        x.map: |i|
            self.multiply_with_five(i)

    Main =
        result = self.multiply_with_five([1, 2, 3, 4, 5])
        IO.print(result)
";

        let mut parser = initialize_parser(source);
        parser.parse().unwrap();

        let class = parser.classes.get(&EntityIndex(1)).unwrap();
        assert_eq!(class.name, EntityIndex(1));
        assert_eq!(class.ancestors, vec![EntityIndex(2)]);
        assert_eq!(class.states, vec![]);
        assert_eq!(class.decorator, String::new());

        let mut methods = AHashMap::new();

        // take_five method
        methods.insert(
            EntityIndex(3),
            vec![Function {
                name: EntityIndex(3),
                params: vec![],
                return_type: Some(Expression::Call {
                    caller: None,
                    name_id: EntityIndex(666),
                    args: vec![],
                    block: None,
                }),
                environment: Some(AHashMap::new()),
                body: Expression::Literal(LiteralIndex(0)),
                decorator: String::new(),
            }],
        );

        // multiply_with_five method
        methods.insert(
            EntityIndex(5),
            vec![
                // multiply_with_five(x: Integer)
                Function {
                    name: EntityIndex(5),
                    params: vec![Pattern {
                        name: PatternName::Name(EntityIndex(6)),
                        value: Some(Expression::Call {
                            caller: None,
                            name_id: EntityIndex(666),
                            args: vec![],
                            block: None,
                        }),
                        condition: None,
                    }],
                    return_type: Some(Expression::Call {
                        caller: None,
                        name_id: EntityIndex(666),
                        args: vec![],
                        block: None,
                    }),
                    environment: Some(AHashMap::new()),
                    body: Expression::Binary(
                        Box::new(Expression::Call {
                            caller: None,
                            name_id: EntityIndex(666),
                            args: vec![],
                            block: None,
                        }),
                        Operator::Multiply,
                        Box::new(Expression::Call {
                            name_id: EntityIndex(3),
                            block: None,
                            args: vec![],
                            caller: Some(Box::new(Expression::ClassSelf)),
                        }),
                    ),
                    decorator: String::new(),
                },
                // multiply_with_five(x: List(Integer))
                Function {
                    name: EntityIndex(5),
                    environment: Some(AHashMap::new()),
                    decorator: String::new(),
                    params: vec![Pattern {
                        name: PatternName::Name(EntityIndex(6)),
                        value: Some(Expression::Call {
                            name_id: EntityIndex(7),
                            block: None,
                            args: vec![Pattern {
                                name: PatternName::NoName,
                                value: Some(Expression::Call {
                                    caller: None,
                                    name_id: EntityIndex(666),
                                    args: vec![],
                                    block: None,
                                }),
                                condition: None,
                            }],
                            caller: None,
                        }),
                        condition: None,
                    }],
                    return_type: Some(Expression::Call {
                        name_id: EntityIndex(7),
                        block: None,
                        args: vec![Pattern {
                            name: PatternName::NoName,
                            value: Some(Expression::Call {
                                caller: None,
                                name_id: EntityIndex(666),
                                args: vec![],
                                block: None,
                            }),
                            condition: None,
                        }],
                        caller: None,
                    }),
                    body: Expression::Call {
                        name_id: EntityIndex(8),
                        block: Some(Box::new(Expression::Block {
                            expressions: vec![],
                            params: vec![Pattern {
                                name: PatternName::Name(EntityIndex(9)),
                                value: None,
                                condition: None,
                            }],
                            value: Box::new(Expression::Call {
                                name_id: EntityIndex(5),
                                block: None,
                                args: vec![Pattern {
                                    name: PatternName::NoName,
                                    value: Some(Expression::Call {
                                        caller: None,
                                        name_id: EntityIndex(666),
                                        args: vec![],
                                        block: None,
                                    }),
                                    condition: None,
                                }],
                                caller: Some(Box::new(Expression::ClassSelf)),
                            }),
                        })),
                        args: vec![],
                        caller: Some(Box::new(Expression::Call {
                            name_id: EntityIndex(6),
                            block: None,
                            args: vec![],
                            caller: None,
                        })),
                    },
                },
            ],
        );

        let mut main_environment = AHashMap::new();
        main_environment.insert(
            EntityIndex(10),
            vec![Function {
                name: EntityIndex(10),
                params: vec![],
                return_type: None,
                environment: None,
                decorator: String::new(),
                body: Expression::Call {
                    name_id: EntityIndex(5),
                    caller: Some(Box::new(Expression::ClassSelf)),
                    args: vec![Pattern {
                        name: PatternName::NoName,
                        value: Some(Expression::Literal(LiteralIndex(1))),
                        condition: None,
                    }],
                    block: None,
                },
            }],
        );

        methods.insert(
            EntityIndex(0),
            vec![Function {
                name: EntityIndex(0),
                params: vec![],
                return_type: None,
                environment: Some(main_environment),
                body: Expression::Block {
                    value: Box::new(Expression::Call {
                        name_id: EntityIndex(12),
                        block: None,
                        args: vec![Pattern {
                            name: PatternName::NoName,
                            value: Some(Expression::Call {
                                caller: None,
                                name_id: EntityIndex(666),
                                args: vec![],
                                block: None,
                            }),
                            condition: None,
                        }],
                        caller: Some(Box::new(Expression::Call {
                            caller: None,
                            name_id: EntityIndex(666),
                            args: vec![],
                            block: None,
                        })),
                    }),
                    expressions: vec![Expression::Function(EntityIndex(10))],
                    params: vec![],
                },
                decorator: String::new(),
            }],
        );
        assert_eq!(class.methods, methods);
    }

    #[test]
    fn test_is_match_with_multiple_kinds() {
        use TokenType::{Equal, EqualEqual};

        let mut parser = initialize_parser("==");

        assert!(parser.is_match(&[Equal, EqualEqual]));
    }

    #[test]
    fn parse_match_expr() {
        use Expression::Match;
        let source = "
match x:
    42 -> 42
    Integer -> 69
";

        let mut parser = initialize_parser(source);
        let result = parser.expression().unwrap();

        assert_eq!(
            result,
            Match {
                scrutinee: Box::new(Expression::Call {
                    caller: None,
                    name_id: EntityIndex(1),
                    args: vec![],
                    block: None
                }),
                arms: vec![
                    (
                        Pattern {
                            name: PatternName::NoName,
                            value: Some(Expression::Literal(LiteralIndex(0))),
                            condition: None,
                        },
                        Expression::Literal(LiteralIndex(1)),
                    ),
                    (
                        Pattern {
                            name: PatternName::NoName,
                            value: Some(Expression::Call {
                                caller: None,
                                name_id: EntityIndex(2),
                                args: vec![],
                                block: None
                            }),
                            condition: None,
                        },
                        Expression::Literal(LiteralIndex(2)),
                    ),
                ],
            },
        )
    }

    #[test]
    fn parse_match_expr_with_condition_and_value() {
        use Expression::{Binary, Match};
        let source = "
match x:
    number: Integer when number > 0 -> 42
    something when something == 0 -> 69
    call_me_maybe: Option.Some(Integer) -> 420
    _ -> 69
";

        let mut parser = initialize_parser(source);
        let result = parser.expression().unwrap();

        assert_eq!(
            result,
            Match {
                scrutinee: Box::new(Expression::Call {
                    caller: None,
                    name_id: EntityIndex(1),
                    args: vec![],
                    block: None
                }),
                arms: vec![
                    (
                        Pattern {
                            name: PatternName::Name(EntityIndex(2)),
                            value: Some(Expression::Call {
                                caller: None,
                                name_id: EntityIndex(3),
                                args: vec![],
                                block: None
                            }),
                            condition: Some(Binary(
                                Box::new(Expression::Call {
                                    caller: None,
                                    name_id: EntityIndex(4),
                                    args: vec![],
                                    block: None
                                }),
                                Operator::Greater,
                                Box::new(Expression::Literal(LiteralIndex(0)))
                            )),
                        },
                        Expression::Literal(LiteralIndex(1))
                    ),
                    (
                        Pattern {
                            name: PatternName::NoName,
                            value: Some(Expression::Call {
                                caller: None,
                                name_id: EntityIndex(5),
                                args: vec![],
                                block: None
                            }),
                            condition: Some(Binary(
                                Box::new(Expression::Call {
                                    caller: None,
                                    name_id: EntityIndex(6),
                                    args: vec![],
                                    block: None
                                }),
                                Operator::Equal,
                                Box::new(Expression::Literal(LiteralIndex(2)))
                            )),
                        },
                        Expression::Literal(LiteralIndex(3))
                    ),
                    (
                        Pattern {
                            name: PatternName::Name(EntityIndex(7)),
                            value: Some(Expression::Call {
                                name_id: EntityIndex(9),
                                block: None,
                                args: vec![Pattern {
                                    // Test fails in here.
                                    // Integer as a param name instead of value? This sound wrong.
                                    // Value should represent the type here.
                                    // Check self.pattern function
                                    name: PatternName::NoName,
                                    value: Some(Expression::Call {
                                        caller: None,
                                        name_id: EntityIndex(10),
                                        args: vec![],
                                        block: None
                                    }),
                                    condition: None,
                                }],
                                caller: Some(Box::new(Expression::Call {
                                    caller: None,
                                    name_id: EntityIndex(8),
                                    args: vec![],
                                    block: None
                                })),
                            }),
                            condition: None,
                        },
                        Expression::Literal(LiteralIndex(4))
                    ),
                    (
                        Pattern {
                            name: PatternName::NoName,
                            value: Some(Expression::Call {
                                caller: None,
                                name_id: EntityIndex(11),
                                args: vec![],
                                block: None
                            }),
                            condition: None
                        },
                        Expression::Literal(LiteralIndex(5))
                    )
                ]
            }
        );
    }

    #[test]
    fn test_missing_expression_after_function() {
        let source = "
Program:
    x =
";
        let mut parser = initialize_parser(source);
        let result = parser.parse().unwrap_err();

        assert_eq!(
            result,
            ParserError::UnexpectedToken {
                expected: "Expression",
                found: TokenType::Dedent,
                line: Some(4)
            }
        )
    }

    #[test]
    fn test_invalid_arrow() {
        let source = "
Program:
    x = ->
";
        let mut parser = initialize_parser(source);
        let result = parser.parse().unwrap_err();

        assert_eq!(
            result,
            ParserError::UnexpectedToken {
                expected: "Expression",
                found: TokenType::Arrow,
                line: Some(3)
            }
        )
    }

    #[test]
    fn test_missing_ancestor_name() {
        let source = "
Program:
    implementing
";
        let mut parser = initialize_parser(source);
        let result = parser.parse().unwrap_err();

        assert_eq!(
            result,
            ParserError::UnexpectedToken {
                expected: "Class Name",
                found: TokenType::Endline,
                line: Some(4)
            }
        )
    }

    #[test]
    fn test_missing_pattern_value() {
        let source = "
Program:
    method(x:) -> Anything =
        x
";

        let mut parser = initialize_parser(source);
        let result = parser.parse().unwrap_err();

        assert_eq!(
            result,
            ParserError::UnexpectedToken {
                expected: "Expression",
                found: TokenType::ParenClose,
                line: Some(3)
            }
        )
    }

    #[test]
    fn test_missing_return_type() {
        let source = "
Program:
    method(x: Anything) -> =
        x
";

        let mut parser = initialize_parser(source);
        let result = parser.parse().unwrap_err();

        assert_eq!(
            result,
            ParserError::UnexpectedToken {
                expected: "Expression",
                found: TokenType::Equal,
                line: Some(3)
            }
        )
    }

    #[test]
    fn test_missing_indent() {
        let source = "
Program:
method() = 8";

        let mut parser = initialize_parser(source);
        let result = parser.parse().unwrap_err();

        assert_eq!(
            result,
            ParserError::UnexpectedToken {
                expected: "Indendation start",
                found: TokenType::Identifier,
                line: Some(3)
            }
        )
    }

    #[test]
    fn test_armless_match_expr() {
        let source = "
match x:
";
        let mut parser = initialize_parser(source);
        let result = parser.expression().unwrap_err();

        assert_eq!(
            result,
            ParserError::UnexpectedToken {
                expected: "Indendation start",
                found: TokenType::Eof,
                line: None
            }
        )
    }

    #[test]
    fn test_patternless_arm_in_match() {
        let source = "
match x:
    -> 666
";
        let mut parser = initialize_parser(source);
        let result = parser.expression().unwrap_err();

        assert_eq!(
            result,
            ParserError::UnexpectedToken {
                expected: "Expression",
                found: TokenType::Arrow,
                line: Some(3)
            }
        )
    }

    #[test]
    fn test_state_without_paran_close() {
        let source = "
Program:
    Connected(
";

        let mut parser = initialize_parser(source);
        let result = parser.expression().unwrap_err();

        assert_eq!(
            result,
            ParserError::UnexpectedToken {
                expected: "Expression",
                found: TokenType::Endline,
                line: Some(4)
            }
        )
    }

    #[test]
    fn test_state_with_missing_value() {
        let source = "
Program:
    Connected(x:)
";

        let mut parser = initialize_parser(source);
        let result = parser.expression().unwrap_err();

        assert_eq!(
            result,
            ParserError::UnexpectedToken {
                expected: "Expression",
                found: TokenType::ParenClose,
                line: Some(3)
            }
        )
    }

    #[test]
    fn test_basic_state_declarations() {
        let source = "
Connection:
    Connected
    Disconnected
    Error(String)
    DetailedError(code: Integer, message: String)
";

        let mut parser = initialize_parser(source);
        parser.parse().unwrap();

        let expected_states = vec![
            Statement::ClassState {
                name: EntityIndex(2), // Connected
                patterns: vec![],
            },
            Statement::ClassState {
                name: EntityIndex(3), // Disconnected
                patterns: vec![],
            },
            Statement::ClassState {
                name: EntityIndex(4), // Error
                patterns: vec![Pattern {
                    name: PatternName::NoName,
                    value: Some(Expression::Call {
                        caller: None,
                        name_id: EntityIndex(5),
                        args: vec![],
                        block: None,
                    }),
                    condition: None,
                }],
            },
            Statement::ClassState {
                name: EntityIndex(6), // DetailedError
                patterns: vec![
                    Pattern {
                        name: PatternName::Name(EntityIndex(7)), // code
                        value: Some(Expression::Call {
                            caller: None,
                            name_id: EntityIndex(8),
                            args: vec![],
                            block: None,
                        }),
                        condition: None,
                    },
                    Pattern {
                        name: PatternName::Name(EntityIndex(9)), // message
                        value: Some(Expression::Call {
                            caller: None,
                            name_id: EntityIndex(10),
                            args: vec![],
                            block: None,
                        }),
                        condition: None,
                    },
                ],
            },
        ];

        assert_eq!(
            parser.classes.get(&EntityIndex(1)).unwrap().states,
            expected_states
        );
    }

    #[test]
    fn test_state_with_methods_and_self_params() {
        use Expression::{Block, Call, ClassSelf, Match};
        let source = r#"
Connection:
    Connected(socket: Socket)
    Disconnected

    disconnect -> Disconnected = 
        match self:
            Connected(socket) ->
                socket.close
                Disconnected
            Disconnected -> Disconnected

    connect(self: Disconnected, socket: Socket) -> Connected =
        Connected(socket)
"#;

        let mut parser = initialize_parser(source);
        parser.parse().unwrap();
        let class = parser.classes.get(&EntityIndex(1)).unwrap();

        let methods = &class.methods;
        let disconnect_method = &methods.get(&EntityIndex(6)).unwrap()[0]; // disconnect
        let connect_method = &methods.get(&EntityIndex(8)).unwrap()[0]; // connect

        assert_eq!(
            disconnect_method.body,
            Match {
                scrutinee: Box::new(ClassSelf),
                arms: vec![
                    (
                        Pattern {
                            name: PatternName::NoName,
                            value: Some(Call {
                                name_id: EntityIndex(2),
                                caller: None,
                                block: None,
                                args: vec![Pattern {
                                    name: PatternName::NoName,
                                    value: Some(Expression::Call {
                                        caller: None,
                                        name_id: EntityIndex(666),
                                        args: vec![],
                                        block: None
                                    }),
                                    condition: None
                                }]
                            }),
                            condition: None
                        },
                        Block {
                            expressions: vec![Call {
                                name_id: EntityIndex(7),
                                caller: Some(Box::new(Expression::Call {
                                    caller: None,
                                    name_id: EntityIndex(666),
                                    args: vec![],
                                    block: None
                                })),
                                args: vec![],
                                block: None
                            }],
                            value: Box::new(Expression::Call {
                                caller: None,
                                name_id: EntityIndex(666),
                                args: vec![],
                                block: None
                            }),
                            params: vec![]
                        }
                    ),
                    (
                        Pattern {
                            name: PatternName::NoName,
                            value: Some(Expression::Call {
                                caller: None,
                                name_id: EntityIndex(666),
                                args: vec![],
                                block: None
                            }),
                            condition: None
                        },
                        Expression::Call {
                            caller: None,
                            name_id: EntityIndex(666),
                            args: vec![],
                            block: None
                        }
                    )
                ]
            }
        );

        assert_eq!(
            connect_method.body,
            Call {
                name_id: EntityIndex(2),
                caller: None,
                block: None,
                args: vec![Pattern {
                    name: PatternName::NoName,
                    value: Some(Expression::Call {
                        caller: None,
                        name_id: EntityIndex(666),
                        args: vec![],
                        block: None
                    }),
                    condition: None
                }]
            }
        );

        assert_eq!(
            connect_method.params[0],
            Pattern {
                name: PatternName::ClassSelf,
                value: Some(Expression::Call {
                    caller: None,
                    name_id: EntityIndex(666),
                    args: vec![],
                    block: None
                }),
                condition: None
            }
        );

        assert_eq!(
            connect_method.params[1],
            Pattern {
                name: PatternName::Name(EntityIndex(3)),
                value: Some(Expression::Call {
                    caller: None,
                    name_id: EntityIndex(666),
                    args: vec![],
                    block: None
                }),
                condition: None
            }
        );

        assert_eq!(
            disconnect_method.return_type,
            Some(Expression::Call {
                caller: None,
                name_id: EntityIndex(666),
                args: vec![],
                block: None
            })
        );

        assert_eq!(
            connect_method.return_type,
            Some(Expression::Call {
                caller: None,
                name_id: EntityIndex(666),
                args: vec![],
                block: None
            })
        );
    }

    #[test]
    fn test_state_with_inline_methods() {
        let source = r#"
Counter:
    Active(count: Integer)
    Inactive

    increment -> Active = Active(self.count + 1)
    reset -> Inactive = Inactive()
    start -> Active = Active(count)
"#;

        let mut parser = initialize_parser(source);
        parser.parse().unwrap();
        let class = parser.classes.get(&EntityIndex(1)).unwrap();

        // Be sure about method definitions are true
        let methods = &class.methods;
        assert!(methods.contains_key(&EntityIndex(6))); // increment
        assert!(methods.contains_key(&EntityIndex(7))); // reset
        assert!(methods.contains_key(&EntityIndex(8))); // start
    }

    #[test]
    fn test_literal_return_types() {
        let source = r#"
Calculator:
    Ready
    
    get_zero -> 0 = 0
    get_empty -> [] = []
    get_entity -> "Calculator" = "Calculator"
"#;

        let mut parser = initialize_parser(source);
        parser.parse().unwrap();
        let class = parser.classes.get(&EntityIndex(1)).unwrap();

        let methods = &class.methods;

        let get_zero = &methods.get(&EntityIndex(3)).unwrap()[0];
        assert_eq!(
            get_zero.return_type,
            Some(Expression::Literal(LiteralIndex(0)))
        );

        let get_empty = &methods.get(&EntityIndex(4)).unwrap()[0];
        assert_eq!(
            get_empty.return_type,
            Some(Expression::Literal(LiteralIndex(2)))
        );

        let get_entity = &methods.get(&EntityIndex(5)).unwrap()[0];
        assert_eq!(
            get_entity.return_type,
            Some(Expression::Literal(LiteralIndex(4)))
        );
    }

    #[test]
    fn test_empty_file() {
        let source = "";
        let mut parser = initialize_parser(source);
        let result = parser.parse();

        assert!(result.is_ok());
        assert!(parser.classes.is_empty());
    }

    #[test]
    fn test_only_comments() {
        let source = "# This is a comment";
        let mut parser = initialize_parser(source);
        let result = parser.parse();

        assert!(result.is_ok());
        assert!(parser.classes.is_empty());
    }
}

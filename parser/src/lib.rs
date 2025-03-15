mod class;
mod entity;
mod error;
mod expression;
mod function;
mod literal;
mod operator;
mod pattern;
mod statement;
mod tests;

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

pub use operator::Operator;
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
            // Line is last token's line, fix it
            line: self.previous().line,
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
        let mut expr = self.primary(true)?;

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
                let Call { name_id, .. } = self.primary(false)? else {
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

    fn primary(&mut self, use_same_call_entity: bool) -> ParserResult<Expression> {
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
                name_id: self.consume_call_name(self.previous(), use_same_call_entity)?,
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
            if self.is_match(&[BraceClose]) {
                Literal::LiteralMap(AHashMap::new())
            } else {
                unimplemented!()
            }
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
        let name = self.allocate_name(self.previous());
        match self
            .global_entity
            .find_sub_entity_by_name(&self.entities, &name)
        {
            Some(Entity { kind, .. }) if kind != &EntityKind::Class => {
                panic!(
                    "{} is already declared as a {:?}, not as a class!",
                    name, kind
                )
            }
            Some(_) => Err(ParserError::DuplicateClass(name, self.previous().line)),
            None => {
                let index = self.next_entity_index();
                let entity = self.create_entity(
                    index,
                    name,
                    self.previous(),
                    EntityKind::Class,
                    "Class Name",
                )?;
                self.entities.insert(index, entity);
                self.global_entity.sub_entities.push(index);
                self.current_class_index = index;
                Ok(index)
            }
        }
    }

    fn consume_declaration_name(&mut self) -> ParserResult<EntityIndex> {
        self.advance();
        let name = self.allocate_name(self.previous());
        match self
            .entities
            .get(&self.current_class_index)
            .and_then(|entity| entity.find_sub_entity_by_name(&self.entities, &name))
        {
            Some(Entity { kind, .. }) if kind != &EntityKind::Declaration => {
                panic!(
                    "{} is already declared as a {:?}, not as a declaration!",
                    name, kind
                )
            }
            Some(Entity { index, .. }) => {
                self.current_declaration_index = *index;
                Ok(*index)
            }
            None => {
                let index = self.next_entity_index();
                let entity = self.create_entity(
                    index,
                    name,
                    self.previous(),
                    EntityKind::Declaration,
                    "Declaration Name",
                )?;
                let Some(class_entity) = self.entities.get_mut(&self.current_class_index) else {
                    panic!(
                        "Current Class Index {:?} doesn't exist!",
                        self.current_class_index
                    )
                };
                class_entity.sub_entities.push(index);
                self.entities.insert(index, entity);
                self.current_declaration_index = index;
                Ok(index)
            }
        }
    }

    fn consume_local_name(&mut self, token: Token) -> ParserResult<EntityIndex> {
        let name = self.allocate_name(self.previous());
        match self
            .entities
            .get(&self.current_declaration_index)
            .and_then(|entity| entity.find_sub_entity_by_name(&self.entities, &name))
        {
            Some(Entity { kind, .. }) if kind != &EntityKind::Local => {
                panic!(
                    "{} is already declared as a {:?}, not as a local!",
                    name, kind
                )
            }
            Some(_) => Err(ParserError::DuplicateLocal(name, self.previous().line)),
            None => {
                let index = self.next_entity_index();
                let entity =
                    self.create_entity(index, name, token, EntityKind::Local, "Local Name")?;
                let Some(declaration_entity) =
                    self.entities.get_mut(&self.current_declaration_index)
                else {
                    panic!(
                        "Current Declaration Index {:?} doesn't exist!",
                        self.current_declaration_index
                    )
                };
                if declaration_entity.kind != EntityKind::Declaration {
                    panic!(
                        "Current Declaration Index {:?} is not a declaration! It is a {:?}",
                        self.current_declaration_index, declaration_entity.kind
                    )
                }
                declaration_entity.sub_entities.push(index);
                self.entities.insert(index, entity);
                self.current_local_index = index;
                Ok(index)
            }
        }
    }

    fn consume_call_name(
        &mut self,
        token: Token,
        use_same_entity: bool,
    ) -> ParserResult<EntityIndex> {
        let name = self.allocate_name(self.previous());
        let parent_index = if self.current_local_index == EntityIndex(0) {
            self.current_declaration_index
        } else {
            self.current_local_index
        };
        match self
            .entities
            .get(&self.current_declaration_index)
            .and_then(|entity| entity.find_sub_entity_by_name(&self.entities, &name))
        {
            Some(Entity { kind, .. }) if kind != &EntityKind::Call => {
                panic!(
                    "{} is already declared as a {:?}, not as a call!",
                    name, kind
                )
            }
            Some(Entity { index, name, .. }) if (use_same_entity || name == "_") => Ok(*index),
            _ => {
                let index = self.next_entity_index();
                let entity =
                    self.create_entity(index, name, token, EntityKind::Call, "CallRoot")?;
                let Some(parent) = self.entities.get_mut(&parent_index) else {
                    panic!("Current Index {:?} doesn't exist!", index)
                };
                parent.sub_entities.push(index);
                self.entities.insert(index, entity);
                Ok(index)
            }
        }
    }

    fn next_entity_index(&self) -> EntityIndex {
        EntityIndex(self.entities.len() + 1)
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
        index: EntityIndex,
        name: String,
        token: Token,
        kind: EntityKind,
        expected: &'static str,
    ) -> ParserResult<Entity> {
        use ParserError::UnexpectedToken;
        use TokenType::{Constant, Identifier, Wildcard};

        match token.kind {
            Constant | Identifier | Wildcard => {
                let visibility = Visibility::from(token.kind);

                Ok(Entity {
                    index,
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

    fn allocate_name(&mut self, token: Token) -> String {
        self.lexemes[token.start..token.end].join("")
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

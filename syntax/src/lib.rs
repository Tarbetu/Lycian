mod class;
mod error;
mod expression;
mod function;
mod literal;
mod operator;
mod pattern;
mod tests;

use crate::expression::CallType;
pub use crate::literal::*;
pub use class::Class;
pub use expression::Expression;
pub use expression::ExpressionKind;

pub use function::Function;
pub use pattern::*;

pub use error::ParserError;
pub use error::ParserResult;

pub use operator::Operator;
use scanner::{Span, Token, TokenType};

use ahash::AHashMap;
use scopeguard::guard;
use std::mem::swap;
use std::rc::Rc;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ParsingMode {
    /// -> symbol will be parsed as function return type
    /// : symbol will be parsed as start of call block
    Normal,
    // -> symbol will be interpreted as match arm
    // : symbol will be interpreted as:
    //   start of match block,
    //   and name of pattern
    NoBlock,
}

pub struct Parser {
    pub classes: AHashMap<Rc<String>, Class>,
    tokens: Vec<Token>,
    position: usize,
    parsing_mode: ParsingMode,
    last_expr_id: usize,
    current_class: Class,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            position: 0,
            classes: AHashMap::new(),
            parsing_mode: ParsingMode::Normal,
            last_expr_id: 1,
            current_class: Class::default(),
        }
    }

    pub fn parse(mut self) -> ParserResult<Parser> {
        while self.peek().is_some() {
            self.class()?;

            let mut class = Class::default();
            swap(&mut class, &mut self.current_class);
            self.classes.insert(class.name.clone(), class);
        }

        Ok(self)
    }

    pub fn class(&mut self) -> ParserResult<()> {
        use TokenType::*;

        self.current_class.decorator = self.consume_decorator()?;

        (self.current_class.span, self.current_class.name) = self.consume_class_name()?;

        self.consume(Colon, "':'")?;
        self.consume_endline()?;
        self.consume(Indent, "Indendation start")?;

        let mut ancestors = vec![];
        while let Some(TokenType::Implementing) = self.peek().map(|t| t.kind) {
            self.advance();
            ancestors.push(self.consume_name()?);
        }
        self.current_class.ancestors = ancestors;

        self.declaration()?;

        while !self.is_match(&[Dedent]) {
            self.skip_while(&[Endline]);

            self.declaration()?;
        }

        Ok(())
    }

    fn declaration(&mut self) -> ParserResult<()> {
        use TokenType::{Arrow, Declaration, Endline, ParenClose, ParenOpen};

        let decorator = self.consume_decorator()?;

        let name = self.consume_name()?;

        let patterns = if self.is_match(&[ParenOpen]) {
            self.pattern_list(ParenClose, "Closing Paranthesis")?
        } else {
            vec![]
        };

        if self.is_match(&[Endline]) {
            self.current_class.constructors.push((name, patterns));
            Ok(())
        } else {
            let return_type = if self.is_match(&[Arrow]) {
                Some(self.or()?)
            } else {
                None
            };

            let token = self.consume(Declaration, "Equal sign")?;

            let body = self.block(false, Some("Method Definition"))?;
            let body = Self::eliminate_expr(body);

            let method = Function {
                name: name.clone(),
                params: patterns,
                return_type,
                body,
                decorator,
                span: token.span,
            };

            self.current_class
                .methods
                .entry(name)
                .or_default()
                .push(method);
            Ok(())
        }
    }

    fn pattern_list(
        &mut self,
        end_with: TokenType,
        expected: &'static str,
    ) -> ParserResult<Vec<Pattern>> {
        use TokenType::{Comma, Endline};

        let mut patterns = Vec::new();

        patterns.push(self.pattern()?);

        while self.is_match(&[Comma, Endline]) {
            patterns.push(self.pattern()?);
        }

        self.consume(end_with, expected)?;

        if patterns.len() >= 50 {
            return Err(ParserError::PatternListTooLong(self.previous().span));
        }

        Ok(patterns)
    }

    fn pattern(&mut self) -> ParserResult<Pattern> {
        use ExpressionKind::{Call, ClassSelf};
        use TokenType::*;
        let parsing_mode_before = self.parsing_mode;
        self.parsing_mode = ParsingMode::NoBlock;
        let mut parser = guard(self, |parser| parser.parsing_mode = parsing_mode_before);

        let expr = parser.expression()?;

        // If it contains : token, this is a name:value pattern
        if parser.is_match(&[Colon]) {
            let name = match *expr.kind {
                Call { caller, .. } => PatternName::Name(caller),
                ClassSelf => PatternName::ClassSelf,
                _ => {
                    return Err(ParserError::UnexpectedToken {
                        expected: "Name",
                        found: parser.previous().kind,
                        span: Some(parser.previous().span),
                    });
                }
            };

            let value = parser.expression()?;

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

            Ok(Pattern {
                name: PatternName::NoName,
                value: expr,
                condition,
            })
        }
    }

    fn block(
        &mut self,
        params_expected: bool,
        context_info: Option<&'static str>,
    ) -> ParserResult<Expression> {
        use TokenType::{Dedent, Endline, Indent, Pipe};

        let params = if self.is_match(&[Pipe]) {
            self.pattern_list(Pipe, "| symbol")?
        } else {
            vec![]
        };

        if !(params_expected || params.is_empty()) {
            return Err(ParserError::UnexpectedBlockParams(
                self.previous().span,
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
        let span = value.span.clone();

        Ok(if expressions.is_empty() && params.is_empty() {
            value
        } else {
            Expression {
                kind: Box::new(ExpressionKind::Block {
                    expressions,
                    value,
                    params,
                }),
                id: self.next_id(),
                span,
            }
        })
    }

    fn expression(&mut self) -> ParserResult<Expression> {
        self.function()
    }

    fn function(&mut self) -> ParserResult<Expression> {
        use TokenType::*;

        let name_token = self.peek();
        let mut expr = self.match_expr()?;
        let params = if self.is_match(&[ParenOpen]) {
            self.pattern_list(ParenClose, "Closing Paranthesis")?
        } else {
            vec![]
        };

        let return_type = if self.is_match(&[Arrow]) {
            Some(self.or()?)
        } else {
            None
        };

        if self.is_match(&[Declaration]) {
            let value = self.block(false, Some("Function Definition"))?;

            let Some(name_token) = name_token else {
                return Err(ParserError::UnexpectedToken {
                    expected: "Local Name",
                    found: TokenType::Eof,
                    span: (name_token.map(|tkn| tkn.span.clone())),
                });
            };
            let name = self.consume_name_from_token(&name_token)?;

            expr = Expression {
                kind: Box::new(ExpressionKind::Function(Function {
                    name,
                    params,
                    return_type,
                    body: value,
                    span: name_token.span.clone(),
                    decorator: Rc::new(String::new()),
                })),
                span: name_token.span,
                id: self.next_id(),
            };
        }

        Ok(expr)
    }

    fn match_expr(&mut self) -> ParserResult<Expression> {
        use TokenType::{Colon, Dedent, Endline, FatArrow, Indent, Match};
        if self.is_match(&[Match]) {
            let parsing_mode_before = self.parsing_mode;
            self.parsing_mode = ParsingMode::NoBlock;
            let mut parser = guard(self, |parser| parser.parsing_mode = parsing_mode_before);

            let span = parser.previous().span;
            let scrutinee = parser.expression()?;

            parser.consume(Colon, ": symbol")?;
            parser.consume_endline()?;
            parser.consume(Indent, "Indendation start")?;

            let arms = {
                let mut arms = vec![];

                while !parser.is_match(&[Dedent]) {
                    parser.skip_while(&[Endline]);
                    let pattern = parser.pattern()?;

                    parser.consume(FatArrow, "The Fat Arrow (=>)")?;

                    let expr = parser.block(true, None)?;

                    arms.push((pattern, expr));
                }

                arms
            };

            Ok(Expression {
                kind: Box::new(ExpressionKind::Match { scrutinee, arms }),
                id: parser.next_id(),
                span,
            })
        } else {
            self.or()
        }
    }

    fn or(&mut self) -> ParserResult<Expression> {
        use TokenType::Or;

        let mut left = self.and()?;

        while self.is_match(&[Or]) {
            let span = self.previous().span;
            let right = self.and()?;

            left = Expression {
                kind: Box::new(ExpressionKind::Binary(left, Operator::Or, right)),
                id: self.next_id(),
                span,
            };
        }

        Ok(left)
    }

    fn and(&mut self) -> ParserResult<Expression> {
        use TokenType::And;

        let mut left = self.equality()?;

        while self.is_match(&[And]) {
            let span = self.previous().span;
            let right = self.equality()?;

            left = Expression {
                kind: Box::new(ExpressionKind::Binary(left, Operator::And, right)),
                id: self.next_id(),
                span,
            };
        }

        Ok(left)
    }

    fn equality(&mut self) -> ParserResult<Expression> {
        use TokenType::{Equal, NotEqual};

        let mut left = self.comparison()?;

        while self.is_match(&[Equal, NotEqual]) {
            let Token { kind, span, .. } = self.previous();
            let operator = match kind {
                Equal => Operator::Equal,
                NotEqual => Operator::NotEqual,
                _ => unreachable!(),
            };

            let right = self.comparison()?;

            left = Expression {
                kind: Box::new(ExpressionKind::Binary(left, operator, right)),
                id: self.next_id(),
                span,
            }
        }

        Ok(left)
    }

    fn comparison(&mut self) -> ParserResult<Expression> {
        use TokenType::{Greater, GreaterEqual, Less, LessEqual};

        let mut left = self.term()?;

        while self.is_match(&[Greater, GreaterEqual, Less, LessEqual]) {
            let Token { kind, span, .. } = self.previous();
            let operator = match kind {
                Greater => Operator::Greater,
                GreaterEqual => Operator::GreaterOrEqual,
                Less => Operator::Smaller,
                LessEqual => Operator::SmallerOrEqual,
                _ => unreachable!(),
            };

            let right = self.term()?;
            left = Expression {
                kind: Box::new(ExpressionKind::Binary(left, operator, right)),
                id: self.next_id(),
                span,
            }
        }

        Ok(left)
    }

    fn term(&mut self) -> ParserResult<Expression> {
        use TokenType::{Minus, Plus};

        let mut left = self.factor()?;

        while self.is_match(&[Plus, Minus]) {
            let Token { kind, span, .. } = self.previous();
            let operator = match kind {
                Plus => Operator::Add,
                Minus => Operator::Substract,
                _ => unreachable!(),
            };

            let right = self.factor()?;

            left = Expression {
                kind: Box::new(ExpressionKind::Binary(left, operator, right)),
                id: self.next_id(),
                span,
            }
        }

        Ok(left)
    }

    fn factor(&mut self) -> ParserResult<Expression> {
        use TokenType::{Percent, Slash, Star};

        let mut left = self.unary()?;

        while self.is_match(&[Star, Slash, Percent]) {
            let Token { kind, span, .. } = self.previous();
            let operator = match kind {
                Star => Operator::Multiply,
                Slash => Operator::Divide,
                Percent => Operator::Modulo,
                _ => unreachable!(),
            };

            let right = self.unary()?;
            left = Expression {
                kind: Box::new(ExpressionKind::Binary(left, operator, right)),
                id: self.next_id(),
                span,
            }
        }

        Ok(left)
    }

    fn unary(&mut self) -> ParserResult<Expression> {
        use TokenType::{Minus, Not};

        if self.is_match(&[Not, Minus]) {
            let Token { kind, span, .. } = self.previous();
            let operator = match kind {
                Not => Operator::Not,
                Minus => Operator::Negate,
                _ => unreachable!(),
            };

            let right = self.unary()?;

            Ok(Expression {
                kind: Box::new(ExpressionKind::Unary(operator, right)),
                id: self.next_id(),
                span,
            })
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ParserResult<Expression> {
        use ExpressionKind::Call;
        use TokenType::{Colon, Dot, Not, ParenClose, ParenOpen};
        let mut expr = self.primary()?;

        loop {
            if self.is_match(&[ParenOpen]) {
                let span = self.previous().span;

                if self.is_match(&[ParenClose]) {
                    continue;
                } else {
                    let Call {
                        caller,
                        callee,
                        block,
                        ..
                    } = *expr.kind
                    else {
                        return Err(ParserError::UnexpectedToken {
                            expected: "Callee",
                            found: self.peek().map(|t| t.kind).unwrap_or(TokenType::Eof),
                            span: self.peek().map(|t| t.span),
                        });
                    };
                    expr = Expression {
                        id: self.next_id(),
                        kind: Box::new(Call {
                            callee,
                            caller,
                            block,
                            args: self.argument_list()?,
                            call_type: CallType::default(),
                        }),
                        span,
                    }
                }
            } else if self.is_match(&[Not]) {
                let span = self.previous().span;

                if self.is_match(&[ParenClose]) {
                    continue;
                } else {
                    let Call {
                        caller,
                        callee,
                        block,
                        ..
                    } = *expr.kind
                    else {
                        return Err(ParserError::UnexpectedToken {
                            expected: "Callee",
                            found: self.peek().map(|t| t.kind).unwrap_or(TokenType::Eof),
                            span: self.peek().map(|t| t.span),
                        });
                    };
                    expr = Expression {
                        id: self.next_id(),
                        kind: Box::new(Call {
                            callee,
                            caller,
                            block,
                            args: self.argument_list()?,
                            call_type: CallType::Strict,
                        }),
                        span,
                    }
                }
            } else if self.is_match(&[Dot]) {
                let span = self.previous().span;
                let name = self.consume_name()?;

                expr = Expression {
                    id: self.next_id(),
                    kind: Box::new(Call {
                        callee: Some(expr),
                        caller: name,
                        args: vec![],
                        block: None,
                        call_type: CallType::default(),
                    }),
                    span,
                }
            } else if let (Some(Colon), ParsingMode::Normal) =
                (self.peek().map(|t| t.kind), self.parsing_mode)
            {
                let Expression { kind, span, id } = expr;
                let ExpressionKind::Call {
                    callee,
                    caller,
                    args,
                    call_type,
                    ..
                } = *kind
                else {
                    panic!("Unexpected expression at call!")
                };

                return Ok(Expression {
                    kind: Box::new(Call {
                        callee,
                        caller,
                        args,
                        call_type,
                        block: Some(self.block(true, Some("Call block"))?),
                    }),
                    span,
                    id,
                });
            } else {
                return Ok(expr);
            }
        }
    }

    fn argument_list(&mut self) -> ParserResult<Vec<Expression>> {
        use TokenType::{Comma, ParenClose};

        let mut list = vec![self.expression()?];

        loop {
            if self.is_match(&[Comma]) {
                list.push(self.expression()?);
            } else {
                self.consume(ParenClose, "closing paranthesis for ending call")?;
                return if list.len() > 50 {
                    Err(ParserError::PatternListTooLong(self.previous().span))
                } else {
                    Ok(list)
                };
            }
        }
    }

    fn primary(&mut self) -> ParserResult<Expression> {
        use TokenType::{
            ClassSelf, Constant, Identifier, ParenClose, ParenOpen, Pass, Super, Wildcard,
        };

        Ok(if self.is_match(&[ParenOpen]) {
            let span = self.previous().span;

            let expr = self.expression()?;
            self.consume(ParenClose, "End of Paranthesis")?;

            Expression {
                kind: Box::new(ExpressionKind::Grouping(expr)),
                span,
                id: self.next_id(),
            }
        } else if self.is_match(&[ClassSelf]) {
            Expression {
                kind: Box::new(ExpressionKind::ClassSelf),
                span: self.previous().span,
                id: self.next_id(),
            }
        } else if self.is_match(&[Super]) {
            Expression {
                kind: Box::new(ExpressionKind::Super),
                span: self.previous().span,
                id: self.next_id(),
            }
        } else if self.is_match(&[Pass]) {
            Expression {
                kind: Box::new(ExpressionKind::Pass),
                span: self.previous().span,
                id: self.next_id(),
            }
        } else if self.is_match(&[Constant, Identifier, Wildcard]) {
            let token = self.previous();
            let name = self.consume_name_from_token(&token)?;
            Expression {
                kind: Box::new(ExpressionKind::Call {
                    caller: name,
                    callee: None,
                    args: vec![],
                    block: None,
                    call_type: CallType::default(),
                }),
                span: token.span,
                id: self.next_id(),
            }
        } else {
            self.catch_literal()?
        })
    }

    fn catch_literal(&mut self) -> ParserResult<Expression> {
        use TokenType::*;

        if self.is_match(&[True]) {
            Ok(Expression {
                kind: Box::new(ExpressionKind::Literal(Rc::new(Literal::Boolean(true)))),
                id: self.next_id(),
                span: self.previous().span,
            })
        } else if self.is_match(&[False]) {
            Ok(Expression {
                kind: Box::new(ExpressionKind::Literal(Rc::new(Literal::Boolean(false)))),
                id: self.next_id(),
                span: self.previous().span,
            })
        } else if self.is_match(&[Integer, Float]) {
            let token = self.previous();
            let incomplete = rug::Float::parse(token.unwrap_literal().as_ref()).unwrap();
            let number = rug::Float::with_val(literal::PRECISION, incomplete);

            let literal = if token.kind == Float {
                Literal::Float(number)
            } else {
                Literal::Integer(number)
            };
            Ok(Expression {
                kind: Box::new(ExpressionKind::Literal(Rc::new(literal))),
                id: self.next_id(),
                span: token.span,
            })
        } else if self.is_match(&[Str]) {
            let token = self.previous();
            Ok(Expression {
                kind: Box::new(ExpressionKind::Literal(Rc::new(Literal::Str(
                    token.unwrap_literal(),
                )))),
                id: self.next_id(),
                span: token.span,
            })
        } else if self.is_match(&[BracketOpen]) {
            if self.is_match(&[BracketClose]) {
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Literal(Rc::new(Literal::LiteralList(
                        vec![],
                    )))),
                    id: self.next_id(),
                    span: self.previous().span,
                })
            } else {
                let mut list = vec![self.expression()?];

                while self.is_match(&[Comma]) {
                    list.push(self.expression()?);
                }

                self.consume(BracketClose, "End of List")?;

                Ok(Expression {
                    kind: Box::new(ExpressionKind::Literal(Rc::new(Literal::LiteralList(list)))),
                    id: self.next_id(),
                    span: self.previous().span,
                })
            }
        } else if self.is_match(&[BraceOpen]) {
            if self.is_match(&[BraceClose]) {
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Literal(Rc::new(Literal::LiteralArray(
                        vec![],
                    )))),
                    id: self.next_id(),
                    span: self.previous().span,
                })
            } else {
                let mut list = vec![self.expression()?];

                while self.is_match(&[Comma]) {
                    list.push(self.expression()?);
                }

                self.consume(BracketClose, "End of List")?;

                Ok(Expression {
                    kind: Box::new(ExpressionKind::Literal(Rc::new(Literal::LiteralArray(
                        list,
                    )))),
                    id: self.next_id(),
                    span: self.previous().span,
                })
            }
        } else {
            Err(ParserError::UnexpectedToken {
                expected: "Expression",
                found: self.peek().unwrap().kind,
                span: self.peek().map(|t| t.span),
            })
        }
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
            Some(token) => Some(token.clone()),
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
            .any(|kind| self.peek().is_some_and(|token| token.kind == *kind))
            .then(|| self.advance())
            .is_some()
    }

    fn consume_one_of_these(
        &mut self,
        kind: &[TokenType],
        expected: &'static str,
    ) -> ParserResult<Token> {
        if self.peek().is_none() {
            Err(ParserError::UnexpectedToken {
                expected,
                found: TokenType::Eof,
                span: None,
            })
        } else if self.is_match(kind) {
            Ok(self.previous())
        } else {
            let Token {
                span, kind: found, ..
            } = self.peek().unwrap();

            Err(ParserError::UnexpectedToken {
                expected,
                found,
                span: Some(span),
            })
        }
    }

    fn consume(&mut self, kind: TokenType, expected: &'static str) -> ParserResult<Token> {
        self.consume_one_of_these(&[kind], expected)
    }

    fn consume_endline(&mut self) -> ParserResult<Token> {
        use TokenType::{Endline, Eof};

        self.consume_one_of_these(&[Eof, Endline], "End of line")
    }

    fn consume_class_name(&mut self) -> ParserResult<(Span, Rc<String>)> {
        let class_token = self.advance();
        let name = class_token.unwrap_literal();

        if self.classes.contains_key(name.as_ref()) {
            Err(ParserError::DuplicateClass(name, class_token.span))
        } else {
            Ok((class_token.span, name))
        }
    }

    fn consume_name(&mut self) -> ParserResult<Rc<String>> {
        let token = self.advance();
        self.consume_name_from_token(&token)
    }

    fn consume_name_from_token(&mut self, token: &Token) -> ParserResult<Rc<String>> {
        let name = token.unwrap_literal();

        if let Some(name) = self
            .current_class
            .methods
            .keys()
            .find(|i| *i == &name)
            .or(self
                .current_class
                .ancestors
                .iter()
                .find(|i| *i == &name)
                .or(self.classes.keys().find(|i| *i == &name)))
        {
            Ok(name.clone())
        } else {
            Ok(name)
        }
    }

    fn consume_decorator(&mut self) -> ParserResult<Rc<String>> {
        use TokenType::Decorator;

        if self.is_match(&[Decorator]) {
            let token = self.peek().unwrap();

            let res = token.unwrap_literal().clone();
            self.advance();
            self.consume_endline()?;
            Ok(res)
        } else {
            Ok(Rc::new(String::new()))
        }
    }

    // fn push_function(
    //     &mut self,
    //     name: EntityIndex,
    //     params: Vec<Pattern>,
    //     return_type: Option<Expression>,
    //     body: Expression,
    //     decorator: String,
    // ) {
    //     let function = Function {
    //         name,
    //         params,
    //         return_type,
    //         body,
    //         environment: None,
    //         decorator,
    //     };
    //     if let Some(functions) = self.current_environment.get_mut(&name) {
    //         functions.push(function);
    //     } else {
    //         self.current_environment.insert(name, vec![function]);
    //     }
    // }

    fn skip_while(&mut self, kinds: &[TokenType]) {
        while kinds.contains(&self.peek().map(|t| t.kind).unwrap_or(TokenType::Eof)) {
            self.advance();
        }
    }

    fn next_id(&mut self) -> usize {
        let id = self.last_expr_id;
        self.last_expr_id += 1;
        id
    }

    fn eliminate_expr(expr: Expression) -> Expression {
        use operator::*;
        use ExpressionKind::*;

        let Expression {
            kind: expr_kind,
            span: expr_span,
            id: expr_id,
        } = expr;

        match *expr_kind {
            Literal(literal) => Expression {
                kind: Box::new(Literal(literal)),
                span: expr_span,
                id: expr_id,
            },
            Grouping(expr) => Self::eliminate_expr(expr),
            Block {
                expressions,
                value,
                params,
            } if expressions.is_empty() && params.is_empty() => Self::eliminate_expr(value),
            Binary(lhs_expr, op, rhs_expr) => {
                if let (Literal(ref lhs), Literal(ref rhs)) =
                    (lhs_expr.kind.as_ref(), rhs_expr.kind.as_ref())
                {
                    let Some(literal) = (match op {
                        Operator::Add => lhs.as_ref() + rhs.as_ref(),
                        Operator::Substract => lhs.as_ref() - rhs.as_ref(),
                        Operator::Multiply => lhs.as_ref() * rhs.as_ref(),
                        Operator::Divide => lhs.as_ref() / rhs.as_ref(),
                        Operator::Modulo => lhs.as_ref() % rhs.as_ref(),
                        Operator::And => lhs.as_ref() & rhs.as_ref(),
                        Operator::Or => lhs.as_ref() | rhs.as_ref(),
                        Operator::In => lhs.as_ref().in_operator(rhs.as_ref()),
                        Operator::Equal => lhs.as_ref().is_equal(rhs.as_ref()),
                        Operator::NotEqual => lhs.as_ref().is_not_equal(rhs.as_ref()),
                        Operator::Greater => lhs.as_ref().greater(rhs.as_ref()),
                        Operator::GreaterOrEqual => lhs.as_ref().greater_equal(rhs.as_ref()),
                        Operator::Smaller => lhs.as_ref().less(rhs.as_ref()),
                        Operator::SmallerOrEqual => lhs.as_ref().less_equal(rhs.as_ref()),
                        _ => unreachable!(),
                    }) else {
                        return Expression {
                            kind: Box::new(Binary(lhs_expr, op, rhs_expr)),
                            span: expr_span,
                            id: expr_id,
                        };
                    };
                    Expression {
                        kind: Box::new(Literal(Rc::new(literal))),
                        id: lhs_expr.id,
                        span: lhs_expr.span,
                    }
                } else {
                    Expression {
                        kind: Box::new(Binary(lhs_expr, op, rhs_expr)),
                        span: expr_span,
                        id: expr_id,
                    }
                }
            }
            Unary(op, expression) => {
                let Expression { kind, span, id } = Self::eliminate_expr(expression);

                if let Literal(ref literal) = kind.as_ref() {
                    let Some(result_literal) = (match op {
                        Operator::Not => !literal.as_ref(),
                        Operator::Negate => -literal.as_ref(),
                        _ => unreachable!(),
                    }) else {
                        return Expression { kind, span, id };
                    };
                    Expression {
                        kind: Box::new(Literal(Rc::new(result_literal))),
                        span,
                        id,
                    }
                } else {
                    Expression { kind, span, id }
                }
            }
            IndexOperator(container, search) => {
                let Expression {
                    kind: container_kind,
                    span: container_span,
                    id: container_id,
                } = Self::eliminate_expr(container);
                let search = Self::eliminate_expr(search);

                if let (Literal(container_literal), Literal(search_literal)) =
                    (container_kind.as_ref(), search.kind.as_ref())
                {
                    match container_literal.get(search_literal) {
                        Some(result_expression) => result_expression,
                        _ => Expression {
                            kind: Box::new(IndexOperator(
                                Expression {
                                    kind: container_kind,
                                    span: container_span.clone(),
                                    id: container_id,
                                },
                                search,
                            )),
                            span: container_span,
                            id: container_id,
                        },
                    }
                } else {
                    Expression {
                        kind: Box::new(IndexOperator(
                            Expression {
                                kind: container_kind,
                                span: container_span.clone(),
                                id: container_id,
                            },
                            search,
                        )),

                        span: expr_span,
                        id: expr_id,
                    }
                }
            }
            Function(function) => {
                let return_type = function.return_type.map(Self::eliminate_expr);
                let body = Self::eliminate_expr(function.body);

                Expression {
                    kind: Box::new(Function(crate::function::Function {
                        return_type,
                        body,
                        ..function
                    })),
                    span: expr_span,
                    id: expr_id,
                }
            }
            Match { scrutinee, arms } => {
                let scrutinee = Self::eliminate_expr(scrutinee);
                let arms = arms
                    .into_iter()
                    .map(|(pattern, expr)| (pattern, Self::eliminate_expr(expr)))
                    .collect();

                Expression {
                    kind: Box::new(Match { scrutinee, arms }),
                    span: expr_span,
                    id: expr_id,
                }
            }
            Call {
                callee,
                args,
                block,
                caller,
                call_type,
            } => {
                let callee = callee.map(Self::eliminate_expr);
                let args = args.into_iter().map(Self::eliminate_expr).collect();
                let block = block.map(Self::eliminate_expr);

                Expression {
                    kind: Box::new(Call {
                        callee,
                        args,
                        block,
                        caller,
                        call_type,
                    }),
                    span: expr_span,
                    id: expr_id,
                }
            }
            kind => Expression {
                kind: Box::new(kind),
                span: expr_span,
                id: expr_id,
            },
        }
    }
}

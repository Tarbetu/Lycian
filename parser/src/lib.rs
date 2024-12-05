mod class;
mod error;
mod expression;
mod function;
mod literal;
mod newtypes;
mod operator;
mod pattern;
mod statement;

use std::mem::swap;

pub use crate::literal::*;
use class::Class;
pub use expression::Expression;
pub use statement::Statement;

pub use function::Function;
pub use pattern::Pattern;

pub use newtypes::*;

pub use error::ParserError;
pub use error::ParserResult;

use operator::Operator;
use scanner::{Token, TokenType};

use ahash::AHashMap;
use bimap::BiHashMap;
use either::Either;

pub struct Parser<'a> {
    pub names: BiHashMap<NameIndex, Name>,
    pub literals: AHashMap<LiteralIndex, Literal>,
    pub classes: AHashMap<NameIndex, Class>,
    lexemes: Vec<&'a str>,
    tokens: Vec<Token>,
    position: usize,
    current_methods: AHashMap<NameIndex, Vec<Function>>,
    current_environment: AHashMap<NameIndex, Vec<Function>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexemes: Vec<&'a str>, tokens: Vec<Token>) -> Parser<'a> {
        Parser {
            lexemes,
            tokens,
            position: 0,
            names: BiHashMap::new(),
            classes: AHashMap::new(),
            current_methods: AHashMap::new(),
            current_environment: AHashMap::new(),
            literals: AHashMap::new(),
        }
    }

    pub fn get_name(&self, index: NameIndex) -> &Name {
        self.names.get_by_left(&index).expect("Invalid name index")
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
        self.consume(Endline, "End of line")?;
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

        while self.peek().is_some() {
            if let Some(declaration) = self.declaration()? {
                match declaration {
                    state @ ClassState { .. } => states.push(state),
                    Method(_) => {}
                    _ => unreachable!(),
                };
            }
        }

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
        use TokenType::{Arrow, Dedent, Endline, Equal};

        let decorator = self.consume_decorator()?;

        let name = self.consume_name()?;

        let patterns = self.pattern_list()?;

        if self.is_match(&[Endline]) {
            Ok(Some(ClassState { name, patterns }))
        } else if self.is_match(&[Dedent]) {
            Ok(None)
        } else {
            let return_type = if self.is_match(&[Arrow]) {
                Some(self.expression()?)
            } else {
                None
            };

            self.consume(Equal, "Equal sign")?;

            let body = self.block(false, Some("Method"))?;

            let mut environment = AHashMap::new();
            swap(&mut self.current_environment, &mut environment);

            self.push_method(name, patterns, return_type, body, environment, decorator);
            Ok(Some(Method(name)))
        }
    }

    fn pattern_list(&mut self) -> ParserResult<Vec<Pattern>> {
        use TokenType::{Comma, Endline, ParenClose, ParenOpen};

        let mut patterns = Vec::new();

        if self.is_match(&[ParenOpen]) {
            self.skip_while(&[Endline]);

            patterns.push(self.pattern()?);

            while self.is_match(&[Comma, Endline]) {
                self.skip_while(&[Endline]);

                patterns.push(self.pattern()?);
            }

            self.consume(ParenClose, "End of Paranthesis")?;
        }

        if patterns.len() >= 255 {
            return Err(ParserError::PatternListTooLong(self.previous().line));
        }

        Ok(patterns)
    }

    fn pattern(&mut self) -> ParserResult<Pattern> {
        use TokenType::*;

        let (name, value) = if self.is_match(&[Wildcard, Identifier, Constant]) {
            let name = self.push_name(&self.peek().unwrap());

            if self.is_match(&[Colon]) {
                let value = self.expression()?;

                (Some(name), Some(value))
            } else {
                (Some(name), None)
            }
        } else {
            (None, Some(self.expression()?))
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

    fn block(
        &mut self,
        params_expected: bool,
        context_info: Option<&'static str>,
    ) -> ParserResult<Expression> {
        use TokenType::{Dedent, Endline, Pipe};

        let params = if self.is_match(&[Pipe]) {
            let result = self.pattern_list()?;
            self.consume(Pipe, "| symbol")?;
            result
        } else {
            vec![]
        };

        if params.is_empty() && params_expected {
            return Err(ParserError::UnexpectedBlockParams(
                self.previous().line,
                context_info.unwrap(),
            ));
        }

        self.skip_while(&[Endline]);

        let mut expressions = vec![];

        while !self.is_match(&[Dedent]) {
            expressions.push(self.expression()?);

            self.consume(Endline, "Endline")?;
        }

        let value = expressions.pop().unwrap();

        if let Some(value) = self.eval_constexpr(&value)? {
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

        let expr = self.lambda()?;
        let token = self.peek();

        if self.is_match(&[ParenOpen]) {
            let params = self.pattern_list()?;

            let return_type = if self.is_match(&[Arrow]) {
                Some(self.expression()?)
            } else {
                None
            };

            self.consume(Equal, "Equal sign")?;

            let value = self.block(false, Some("Function"))?;

            let name = self.consume_name_from_token(token, "Function Name")?;

            self.push_function(name, params, return_type, value, String::new());
            Ok(Expression::Function(name))
        } else {
            Ok(expr)
        }
    }

    fn lambda(&mut self) -> ParserResult<Expression> {
        use TokenType::Pipe;

        if self.is_match(&[Pipe]) {
            let params = self.pattern_list()?;
            self.consume(Pipe, "| symbol")?;
            let expr = self.expression()?;

            Ok(Expression::Lambda {
                expression: Box::new(expr),
                params,
            })
        } else {
            self.match_expr()
        }
    }

    fn match_expr(&mut self) -> ParserResult<Expression> {
        use TokenType::{Arrow, Dedent, Endline, Indent, Match};
        if self.is_match(&[Match]) {
            let scrutinee = self.expression()?;

            self.skip_while(&[Endline]);

            self.consume(Indent, "Indendation start")?;

            let arms = {
                let mut arms = vec![];

                while !self.is_match(&[Dedent]) {
                    let pattern = self.pattern()?;

                    self.consume(Arrow, "Arrow")?;

                    let expr = if self.is_match(&[Endline]) {
                        self.skip_while(&[Endline]);
                        self.consume(Indent, "Endline")?;
                        self.block(true, None)?
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
        use TokenType::{Slash, Star};

        let mut left = self.unary()?;

        while self.is_match(&[Star, Slash]) {
            let operator = match self.previous().kind {
                Star => Operator::Multiply,
                Slash => Operator::Divide,
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
        use Expression::{Call, MethodCall};
        use TokenType::{Colon, Dot, Endline, ParenClose, ParenOpen};
        let mut expr = self.primary()?;

        loop {
            let callee_token = self.peek();

            if self.is_match(&[ParenOpen]) {
                self.skip_while(&[Endline]);

                let args = if self.is_match(&[ParenClose]) {
                    vec![]
                } else {
                    self.arguments()?
                };
                let block = if self.is_match(&[Colon]) {
                    Some(Box::new(self.block(true, None)?))
                } else {
                    None
                };
                expr = Call {
                    callee: Box::new(expr),
                    function_id: self.consume_name_from_token(callee_token, "Callee")?,
                    args,
                    block,
                };
            } else if self.is_match(&[Dot]) {
                expr = MethodCall {
                    inner_call: Box::new(expr),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn arguments(&mut self) -> ParserResult<Vec<Expression>> {
        use TokenType::{Comma, Endline, ParenClose};

        self.skip_while(&[Endline]);

        let mut arguments = vec![];
        arguments.push(self.expression()?);

        while self.is_match(&[Comma, Endline]) {
            arguments.push(self.expression()?);
        }

        self.consume(ParenClose, "")?;

        Ok(arguments)
    }

    fn primary(&mut self) -> ParserResult<Expression> {
        use TokenType::{ClassSelf, ParenClose, ParenOpen, Super};

        Ok(if self.is_match(&[ParenOpen]) {
            let expr = self.expression()?;
            self.consume(ParenClose, "End of Paranthesis")?;
            Expression::Grouping(Box::new(expr))
        } else if self.is_match(&[ClassSelf]) {
            Expression::ClassSelf
        } else if self.is_match(&[Super]) {
            Expression::Super
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
            let token = self.peek().unwrap();
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
                    self.skip_while(&[Endline]);

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

                Literal::LiteralList(list)
            }
        } else {
            return Err(ParserError::UnexpectedToken {
                expected: "Expression",
                found: self.peek().unwrap().kind,
                line: self.peek().map(|t| t.line),
            });
        };

        Ok(self.push_literal(res))
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
            let token = self.peek().unwrap();

            return Err(ParserError::UnexpectedToken {
                expected,
                found: token.kind,
                line: Some(token.line),
            });
        }

        Ok(())
    }

    fn consume_class_name(&mut self) -> ParserResult<NameIndex> {
        self.consume_name_from_token(self.peek(), "Class Name")
    }

    fn consume_name(&mut self) -> ParserResult<NameIndex> {
        self.consume_name_from_token(self.peek(), "Name")
    }

    fn consume_name_from_token(
        &mut self,
        token: Option<Token>,
        expected: &'static str,
    ) -> ParserResult<NameIndex> {
        use ParserError::UnexpectedToken;
        use TokenType::{Constant, Identifier, Wildcard};

        match token {
            Some(token)
                if token.kind == Constant || token.kind == Identifier || token.kind == Wildcard =>
            {
                self.advance();
                Ok(self.push_name(&token))
            }
            Some(token) => Err(UnexpectedToken {
                expected,
                found: token.kind,
                line: Some(token.line),
            }),
            None => Err(UnexpectedToken {
                expected,
                found: TokenType::Eof,
                line: None,
            }),
        }
    }

    fn consume_decorator(&mut self) -> ParserResult<String> {
        use TokenType::{Decorator, Endline};

        if self.is_match(&[Decorator]) {
            let token = self.peek().unwrap();

            let res = self.lexemes[token.start..token.end].join("");
            self.advance();
            self.consume(Endline, "Endline")?;
            Ok(res)
        } else {
            Ok(String::new())
        }
    }

    fn push_name(&mut self, token: &Token) -> NameIndex {
        use TokenType::{Constant, Identifier, Wildcard};

        let string = self.lexemes[token.start..token.end].join("");

        let name = match token.kind {
            Constant => Name::Public(string),
            Identifier => Name::Protected(string),
            Wildcard => Name::Private(string),
            _ => unreachable!(),
        };

        if let Some(index) = self.names.get_by_right(&name) {
            *index
        } else if name.as_ref() == "main" {
            NameIndex(0)
        } else {
            let next_index = *self.names.left_values().max().unwrap_or(&NameIndex(1));

            self.names.insert(next_index, name);

            next_index
        }
    }

    fn push_literal(&mut self, literal: Literal) -> LiteralIndex {
        let next_index = *self.literals.keys().max().unwrap_or(&LiteralIndex(0));

        self.literals.insert(next_index, literal);

        next_index
    }

    fn push_method(
        &mut self,
        name: NameIndex,
        params: Vec<Pattern>,
        return_type: Option<Expression>,
        body: Expression,
        environment: AHashMap<NameIndex, Vec<Function>>,
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
        name: NameIndex,
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
            self.current_methods.insert(name, vec![function]);
        }
    }

    fn skip_while(&mut self, kinds: &[TokenType]) {
        while self.is_match(kinds) {
            self.advance();
        }
    }

    fn eval_constexpr(&mut self, expr: &Expression) -> ParserResult<Option<Expression>> {
        use operator::*;
        use Expression::*;

        Ok(match expr {
            Literal(index) => Some(Expression::Literal(*index)),
            Grouping(expr) => self.eval_constexpr(expr)?,
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
                    Literal(self.push_literal(literal))
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
                    Literal(self.push_literal(literal))
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
                        Either::Right(literal) => Literal(self.push_literal(literal)),
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

    fn parse(source: &str) -> Parser {
        let tokens = Scanner::new(source, false).scan();

        let graphemes = source.graphemes(true).collect::<Vec<&str>>();
        let mut parser = Parser::new(graphemes, tokens);
        parser.parse().unwrap();

        parser
    }

    #[test]
    fn parse_simple_method() {
        let source = "
Program:
    main = 42
";

        let result = parse(source);
        assert_eq!(
            result.get_name(NameIndex(1)),
            &Name::Public("Program".to_string())
        );
        assert_eq!(
            result.get_literal(LiteralIndex(0)),
            &Literal::Integer(rug::Float::with_val(literal::PRECISION, 42))
        );
        let main_method = result
            .classes
            .get(&NameIndex(1))
            .unwrap()
            .methods
            .get(&NameIndex(0))
            .map(|methods| methods.first().unwrap())
            .unwrap();
        assert_eq!(main_method.params, vec![]);
        assert_eq!(main_method.return_type, None);
        assert_eq!(main_method.environment.as_ref().map(|e| e.len()), Some(0));
        assert_eq!(main_method.body, Expression::Literal(LiteralIndex(0)));
        assert!(main_method.decorator.is_empty());
    }
}

use scanner::Span;
use scanner::TokenType;
use std::rc::Rc;
use std::{error::Error, fmt::Display};

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug, PartialEq)]
pub enum ParserError {
    ErrorToken(TokenType, Option<Span>),
    UnexpectedToken {
        expected: &'static str,
        found: TokenType,
        span: Option<Span>,
    },
    InvalidAssignmentTarget(Span),
    PatternListTooLong(Span),
    ErrorWithMessage(Box<ParserError>, &'static str),
    UnexpectedBlockParams(Span, &'static str),
    UnexpectedBlock(Span, TokenType),
    DuplicateClass(Rc<String>, Span),
    DuplicateLocal(Rc<String>, Span),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ParserError::*;

        match self {
            ErrorToken(token, Some(span)) => {
                write!(f, "[{}] Found {}", span, token)
            }
            ErrorToken(token, None) => {
                write!(f, "[at end] Found {}", token)
            }
            UnexpectedToken {
                expected,
                found,
                span: Some(span),
            } => {
                write!(f, "[{}] Expected {} but found {}", span, expected, found)
            }
            UnexpectedToken {
                expected,
                found,
                span: None,
            } => {
                write!(f, "[at end] Expected {} but found {}", expected, found)
            }
            InvalidAssignmentTarget(span) => {
                write!(f, "[{}] Invalid assignment target", span)
            }
            PatternListTooLong(span) => {
                write!(f, "[{}] Too many patterns in pattern list", span)
            }
            ErrorWithMessage(err, msg) => {
                write!(f, "{}\n{}", err, msg)
            }
            UnexpectedBlockParams(span, context) => {
                write!(
                    f,
                    "[{}] Block parameters is not allowed in {}",
                    span, context
                )
            }
            UnexpectedBlock(span, token_type) => {
                write!(f, "[{}] Unexpected block for {}", span, token_type)
            }
            DuplicateClass(class_name, span) => {
                write!(f, "[{}] Class {} already defined", span, class_name)
            }
            DuplicateLocal(class_name, span) => {
                write!(
                    f,
                    "[{}] {} already defined, you can not redefine or overload locals",
                    span, class_name
                )
            }
        }
    }
}

impl Error for ParserError {}

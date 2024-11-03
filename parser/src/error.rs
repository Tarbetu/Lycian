use scanner::TokenType;
use std::{error::Error, fmt::Display};

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    ErrorToken(TokenType, Option<usize>),
    UnexpectedToken {
        expected: &'static str,
        found: TokenType,
        line: Option<usize>,
    },
    MissingReturnType {
        function: String,
        line: usize,
    },
    InvalidAssignmentTarget(usize),
    ErrorWithMessage(Box<ParserError>, &'static str),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ParserError::*;

        match self {
            ErrorToken(token, Some(line)) => {
                write!(f, "[{}] Found {}", line, token)
            }
            ErrorToken(token, None) => {
                write!(f, "[at end] Found {}", token)
            }
            UnexpectedToken {
                expected,
                found,
                line: Some(line),
            } => {
                write!(f, "[{}] Expected {} but found {}", line, expected, found)
            }
            UnexpectedToken {
                expected,
                found,
                line: None,
            } => {
                write!(f, "[at end] Expected {} but found {}", expected, found)
            }
            MissingReturnType { function, line } => {
                write!(
                    f,
                    "[{}] Can not infer return type for function {}. Please add a return type",
                    line, function
                )
            }
            InvalidAssignmentTarget(line) => {
                write!(f, "[{}] Invalid assignment target", line)
            }
            ErrorWithMessage(err, msg) => {
                write!(f, "{}\n{}", err, msg)
            }
        }
    }
}

impl Error for ParserError {}

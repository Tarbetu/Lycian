use scanner::TokenType;
use std::{error::Error, fmt::Display};

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    ErrorToken(TokenType),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::ErrorToken(token) => {
                write!(f, "Error at token: {}", token)
            }
        }
    }
}

impl Error for ParserError {}

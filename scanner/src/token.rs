use crate::TokenType;

#[derive(Debug)]
pub struct Token {
    pub kind: TokenType,
    pub start: usize,
    pub end: usize,
    pub line: usize,
}

impl Token {
    pub fn new(kind: TokenType, start: usize, end: usize, line: usize) -> Token {
        Token {
            kind,
            start,
            end,
            line,
        }
    }
}

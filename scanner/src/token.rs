use crate::span::Span;
use crate::TokenType;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenType,
    literal: TokenLiteral,
    pub span: Span,
}

#[derive(Debug, Clone)]
enum TokenLiteral {
    Keyword,
    Variable(Rc<String>),
}

impl Token {
    pub fn new(kind: TokenType, span: Span, literal: Option<String>) -> Self {
        Token {
            kind,
            span,
            literal: if let Some(literal) = literal {
                TokenLiteral::Variable(Rc::new(literal))
            } else {
                TokenLiteral::Keyword
            },
        }
    }

    pub fn unwrap_literal(&self) -> Rc<String> {
        if let TokenLiteral::Variable(ref literal) = self.literal {
            literal.clone()
        } else {
            panic!("Tried to unwrap a non-variable token literal");
        }
    }

    pub fn paren_open(span: Span) -> Self {
        Token::new(TokenType::ParenOpen, span, None)
    }

    pub fn paren_close(span: Span) -> Self {
        Token::new(TokenType::ParenClose, span, None)
    }

    pub fn brace_open(span: Span) -> Self {
        Token::new(TokenType::BraceOpen, span, None)
    }
    pub fn brace_close(span: Span) -> Self {
        Token::new(TokenType::BraceClose, span, None)
    }
    pub fn bracket_open(span: Span) -> Self {
        Token::new(TokenType::BracketOpen, span, None)
    }
    pub fn bracket_close(span: Span) -> Self {
        Token::new(TokenType::BracketClose, span, None)
    }
    pub fn comma(span: Span) -> Self {
        Token::new(TokenType::Comma, span, None)
    }
    pub fn dot(span: Span) -> Self {
        Token::new(TokenType::Dot, span, None)
    }

    pub fn colon(span: Span) -> Self {
        Token::new(TokenType::Colon, span, None)
    }
    pub fn range(span: Span) -> Self {
        Token::new(TokenType::Range, span, None)
    }
    pub fn range_equal(span: Span) -> Self {
        Token::new(TokenType::RangeEqual, span, None)
    }
    pub fn not(span: Span) -> Self {
        Token::new(TokenType::Not, span, None)
    }
    pub fn not_equal(span: Span) -> Self {
        Token::new(TokenType::NotEqual, span, None)
    }
    pub fn equal(span: Span) -> Self {
        Token::new(TokenType::Equal, span, None)
    }
    pub fn declaration(span: Span) -> Self {
        Token::new(TokenType::Declaration, span, None)
    }
    pub fn greater(span: Span) -> Self {
        Token::new(TokenType::Greater, span, None)
    }
    pub fn less(span: Span) -> Self {
        Token::new(TokenType::Less, span, None)
    }
    pub fn greater_equal(span: Span) -> Self {
        Token::new(TokenType::GreaterEqual, span, None)
    }
    pub fn less_equal(span: Span) -> Self {
        Token::new(TokenType::LessEqual, span, None)
    }
    pub fn minus(span: Span) -> Self {
        Token::new(TokenType::Minus, span, None)
    }
    pub fn plus(span: Span) -> Self {
        Token::new(TokenType::Plus, span, None)
    }
    pub fn slash(span: Span) -> Self {
        Token::new(TokenType::Slash, span, None)
    }
    pub fn star(span: Span) -> Self {
        Token::new(TokenType::Star, span, None)
    }

    pub fn percent(span: Span) -> Self {
        Token::new(TokenType::Percent, span, None)
    }
    pub fn pipe(span: Span) -> Self {
        Token::new(TokenType::Pipe, span, None)
    }

    pub fn arrow(span: Span) -> Self {
        Token::new(TokenType::Arrow, span, None)
    }

    pub fn fat_arrow(span: Span) -> Self {
        Token::new(TokenType::FatArrow, span, None)
    }

    pub fn identifier(span: Span, literal: String) -> Self {
        Token::new(TokenType::Identifier, span, Some(literal))
    }

    pub fn constant(span: Span, literal: String) -> Self {
        Token::new(TokenType::Constant, span, Some(literal))
    }

    pub fn wildcard(span: Span, literal: String) -> Self {
        Token::new(TokenType::Wildcard, span, Some(literal))
    }

    pub fn char_literal(span: Span, literal: String) -> Self {
        Token::new(TokenType::CharLiteral, span, Some(literal))
    }

    pub fn str(span: Span, literal: String) -> Self {
        Token::new(TokenType::Str, span, Some(literal))
    }

    pub fn integer(span: Span, literal: String) -> Self {
        Token::new(TokenType::Integer, span, Some(literal))
    }

    pub fn float(span: Span, literal: String) -> Self {
        Token::new(TokenType::Float, span, Some(literal))
    }

    pub fn and(span: Span) -> Self {
        Token::new(TokenType::And, span, None)
    }

    pub fn or(span: Span) -> Self {
        Token::new(TokenType::Or, span, None)
    }

    pub fn true_keyword(span: Span) -> Self {
        Token::new(TokenType::True, span, None)
    }

    pub fn false_keyword(span: Span) -> Self {
        Token::new(TokenType::False, span, None)
    }

    pub fn use_keyword(span: Span) -> Self {
        Token::new(TokenType::Use, span, None)
    }

    pub fn match_keyword(span: Span) -> Self {
        Token::new(TokenType::Match, span, None)
    }

    pub fn in_keyword(span: Span) -> Self {
        Token::new(TokenType::In, span, None)
    }

    pub fn when(span: Span) -> Self {
        Token::new(TokenType::When, span, None)
    }

    pub fn pass(span: Span) -> Self {
        Token::new(TokenType::Pass, span, None)
    }

    pub fn super_keyword(span: Span) -> Self {
        Token::new(TokenType::Super, span, None)
    }

    pub fn implementing(span: Span) -> Self {
        Token::new(TokenType::Implementing, span, None)
    }

    pub fn class_self(span: Span) -> Self {
        Token::new(TokenType::ClassSelf, span, None)
    }

    pub fn decorator(span: Span, literal: String) -> Self {
        Token::new(TokenType::Decorator, span, Some(literal))
    }

    pub fn space(span: Span) -> Self {
        Token::new(TokenType::Space, span, None)
    }

    pub fn comment(span: Span, literal: String) -> Self {
        Token::new(TokenType::Comment, span, Some(literal))
    }

    pub fn documentation(span: Span, literal: String) -> Self {
        Token::new(TokenType::Documentation, span, Some(literal))
    }

    pub fn endline(span: Span) -> Self {
        Token::new(TokenType::Endline, span, None)
    }

    pub fn eof(span: Span) -> Self {
        Token::new(TokenType::Eof, span, None)
    }

    pub fn unexpected_character(span: Span, literal: String) -> Self {
        Token::new(TokenType::UnexpectedCharacter, span, Some(literal))
    }

    pub fn unterminated_char(span: Span, literal: String) -> Self {
        Token::new(TokenType::UnterminatedChar, span, Some(literal))
    }

    pub fn empty_char(span: Span) -> Self {
        Token::new(TokenType::EmptyChar, span, None)
    }

    pub fn unterminated_string(span: Span, literal: String) -> Self {
        Token::new(TokenType::UnterminatedString, span, Some(literal))
    }

    pub fn invalid_indentation(span: Span) -> Self {
        Token::new(TokenType::InvalidIndentation, span, None)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenLiteral::*;

        match self.literal {
            Variable(ref string) => {
                write!(f, "{string}")
            }
            Keyword => write!(f, "{}", self.kind),
        }
    }
}

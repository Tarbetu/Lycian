mod token;
mod token_type;
use std::cmp::Ordering;

use std::{
    collections::VecDeque,
    iter::{Enumerate, Peekable},
    str::Chars,
};

pub use token::Token;
pub use token_type::TokenType;

pub struct Scanner<'a> {
    chars: Peekable<Enumerate<Chars<'a>>>,
    start: usize,
    line: usize,
    tokens: VecDeque<Token>,
    line_buffer: Vec<TokenType>,
    first_indentation: usize,
    current_indentation: usize,
    max: usize,
    catch_comments: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str, catch_comments: bool) -> Scanner {
        Scanner {
            chars: source.chars().enumerate().peekable(),
            tokens: VecDeque::new(),
            start: 0,
            line: 1,
            line_buffer: Vec::new(),
            first_indentation: 0,
            current_indentation: 0,
            max: source.len(),
            catch_comments,
        }
    }

    pub fn scan(mut self) -> VecDeque<Token> {
        use TokenType::*;

        if self.is_at_end() {
            self.line_buffer.clear();
            self.push(self.max, Eof);
            return self.tokens;
        }

        if let Some((current, char)) = self.chars.next() {
            self.start = current;

            let kind = match char {
                '(' => Some((current + 1, ParenOpen)),
                ')' => Some((current + 1, ParenClose)),
                '{' => Some((current + 1, BraceOpen)),
                '}' => Some((current + 1, BraceClose)),
                '[' => Some((current + 1, BracketOpen)),
                ']' => Some((current + 1, BracketClose)),
                ',' => Some((current + 1, Comma)),
                ':' => Some((current + 1, Colon)),
                ';' => Some((current + 1, Semicolon)),
                '+' => Some((current + 1, Plus)),
                '*' => Some((current + 1, Star)),
                '/' => Some((current + 1, Slash)),
                '%' => Some((current + 1, Percent)),
                '|' => Some((current + 1, Pipe)),
                '-' => {
                    if let Some(&(_, '>')) = self.chars.peek() {
                        self.chars.next();
                        Some((current + 2, Arrow))
                    } else {
                        Some((current + 1, Minus))
                    }
                }
                '.' => {
                    if let Some(&(_, '.')) = self.chars.peek() {
                        self.chars.next();
                        if let Some(&(_, '=')) = self.chars.peek() {
                            self.chars.next();
                            Some((current + 3, RangeEqual))
                        } else {
                            Some((current + 2, Range))
                        }
                    } else {
                        Some((current + 1, Dot))
                    }
                }
                '!' => {
                    if let Some(&(_, '=')) = self.chars.peek() {
                        self.chars.next();
                        Some((current + 2, NotEqual))
                    } else {
                        Some((current + 1, Not))
                    }
                }
                '=' => {
                    if let Some(&(_, '=')) = self.chars.peek() {
                        self.chars.next();
                        Some((current + 2, EqualEqual))
                    } else {
                        Some((current + 1, Equal))
                    }
                }
                '<' => {
                    if let Some(&(_, '=')) = self.chars.peek() {
                        self.chars.next();
                        Some((current + 2, LessEqual))
                    } else {
                        Some((current + 1, Less))
                    }
                }
                '>' => {
                    if let Some(&(_, '=')) = self.chars.peek() {
                        self.chars.next();
                        Some((current + 2, GreaterEqual))
                    } else {
                        Some((current + 1, Greater))
                    }
                }
                '\'' => Some(self.char()),
                '"' => Some(self.string()),
                '_' => Some(self.wildcard()),
                '@' => Some(self.catch_until_line_end(Decorator)),
                '#' => {
                    if let Some(&(_, '#')) = self.chars.peek() {
                        self.chars.next();
                        let token = self.catch_until_line_end(Documentation);

                        if self.catch_comments {
                            Some(token)
                        } else {
                            None
                        }
                    } else {
                        let token = self.catch_until_line_end(Comment);

                        if self.catch_comments {
                            Some(token)
                        } else {
                            None
                        }
                    }
                }
                digit if digit.is_ascii_digit() => Some(self.number()),
                space if space.is_whitespace() => self
                    .handle_whitespace(space)
                    .map(|kind| (current + 1, kind)),
                char if char.is_ascii_lowercase() => Some(self.identifier(char)),
                char if char.is_ascii_uppercase() => Some(self.constant()),
                char => Some((current + 1, UnexpectedCharacter(char))),
            };

            if let Some((current, kind)) = kind {
                self.push(current, kind);
            }
        }

        self.scan()
    }

    fn is_at_end(&mut self) -> bool {
        self.chars.peek().is_none()
    }

    fn push(&mut self, current: usize, kind: TokenType) {
        use TokenType::{Dedent, Endline, Indent, InvalidIndentation, Semicolon, Space};
        if !(kind == Endline) {
            if matches!(self.line_buffer.last(), Some(Space) | None) {
                self.line_buffer.push(kind);
            }

            if !matches!(self.line_buffer.last(), Some(Space) | None) {
                let indentation = self.line_buffer.len() - 1;

                match indentation.cmp(&self.current_indentation) {
                    Ordering::Less => {
                        for _ in
                            0..((self.current_indentation - indentation) / self.first_indentation)
                        {
                            self.tokens
                                .push_back(Token::new(Dedent, self.start, current, self.line));
                        }

                        if self.current_indentation == 0 {
                            self.first_indentation = 0;
                        }
                    }
                    Ordering::Equal => {}
                    Ordering::Greater => {
                        if self.first_indentation == 0 {
                            self.first_indentation = indentation;
                        } else if self.current_indentation % self.first_indentation != 0 {
                            self.tokens.push_back(Token::new(
                                InvalidIndentation,
                                current,
                                current,
                                self.line,
                            ));
                        }

                        self.tokens
                            .push_back(Token::new(Indent, self.start, current, self.line));
                    }
                }
                self.current_indentation = indentation;
            }

            self.tokens
                .push_back(Token::new(kind, self.start, current, self.line));
        } else {
            self.tokens
                .push_back(Token::new(Semicolon, self.start, current, self.line))
        }
    }

    fn handle_whitespace(&mut self, current_char: char) -> Option<TokenType> {
        use TokenType::*;

        match current_char {
            '\n' => {
                self.line += 1;
                let res = if !self.line_buffer.is_empty() {
                    Some(Endline)
                } else {
                    None
                };
                self.line_buffer.clear();
                res
            }
            ' ' | '\t' => {
                if matches!(self.line_buffer.last(), Some(TokenType::Space) | None) {
                    self.line_buffer.push(Space);
                }
                None
            }
            _ if current_char.is_whitespace() => None,
            _ => unreachable!(),
        }
    }

    fn char(&mut self) -> (usize, TokenType) {
        use TokenType::{CharLiteral, EmptyChar, UnterminatedChar};

        let mut char_encountered = false;
        loop {
            match self.chars.peek() {
                Some(&(current, '\'')) => {
                    self.chars.next();
                    return if char_encountered {
                        (current + 1, CharLiteral)
                    } else {
                        (current + 1, EmptyChar)
                    };
                }
                Some(&(current, _)) if char_encountered => return (current, UnterminatedChar),
                None => return (self.max, UnterminatedChar),
                Some(_) => {
                    char_encountered = true;
                    self.chars.next();
                }
            }
        }
    }

    fn string(&mut self) -> (usize, TokenType) {
        use TokenType::{BasicString, InterpolatedString, UnterminatedString};

        let mut interpolated = false;
        loop {
            if let Some(&(_, char)) = self.chars.peek() {
                match char {
                    '#' => {
                        self.chars.next();
                        if let Some(&(_, '{')) = self.chars.peek() {
                            interpolated = true;
                        }
                    }
                    '"' => {
                        let current = self.chars.next().map(|(i, _)| i).unwrap_or(self.max) + 1;
                        return if interpolated {
                            (current, InterpolatedString)
                        } else {
                            (current, BasicString)
                        };
                    }
                    '\n' => {
                        self.line += 1;
                        self.chars.next();
                    }
                    _ => {
                        self.chars.next();
                    }
                }
            } else {
                return (self.max, UnterminatedString);
            }
        }
    }

    fn number(&mut self) -> (usize, TokenType) {
        use TokenType::{Float, Integer};

        let mut is_integer = true;
        loop {
            match self.chars.peek() {
                Some(&(_, char)) if char.is_ascii_digit() => {
                    self.chars.next();
                }
                Some(&(_, '.')) => {
                    self.chars.next();
                    is_integer = false;
                }
                Some(&(index, _)) => return (index, if is_integer { Integer } else { Float }),
                None => return (self.max, if is_integer { Integer } else { Float }),
            }
        }
    }

    fn wildcard(&mut self) -> (usize, TokenType) {
        use TokenType::Wildcard;

        loop {
            match self.chars.peek() {
                Some(&(_, char)) if char.is_ascii_alphanumeric() || char == '_' => {
                    self.chars.next();
                }
                Some(&(index, _)) => return (index, Wildcard),
                None => return (self.max, Wildcard),
            }
        }
    }

    fn identifier(&mut self, first_char: char) -> (usize, TokenType) {
        use TokenType::*;

        let mut buffer = String::from(first_char);
        let mut current = self.max;
        loop {
            match self.chars.peek() {
                Some(&(_, char)) if char.is_ascii_alphanumeric() || char == '_' => {
                    self.chars.next();
                    buffer.push(char);
                }
                Some(&(index, _)) => {
                    current = index;
                    break;
                }
                _ => break,
            }
        }

        let kind = match buffer.as_str() {
            "and" => And,
            "or" => Or,
            "implementing" => Implementing,
            "self" => ClassSelf,
            "super" => Super,
            "true" => True,
            "false" => False,
            "use" => Use,
            "match" => Match,
            "in" => In,
            "when" => When,
            "pass" => Pass,
            _ => Identifier,
        };

        (current, kind)
    }

    fn constant(&mut self) -> (usize, TokenType) {
        use TokenType::*;

        loop {
            match self.chars.peek() {
                Some(&(_, char)) if char.is_ascii_alphanumeric() => {
                    self.chars.next();
                }
                Some(&(index, _)) => return (index, Constant),
                None => return (self.max, Constant),
            }
        }
    }

    fn catch_until_line_end(&mut self, kind: TokenType) -> (usize, TokenType) {
        loop {
            match self.chars.peek() {
                Some((index, '\n')) => {
                    return (index + 1, kind);
                }
                None => return (self.max, kind),
                _ => {
                    self.chars.next();
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenType::*;

    fn tokenize(scanner: Scanner) -> Vec<Token> {
        scanner.scan().into_iter().collect()
    }

    fn get_token_kinds(tokens: &[Token]) -> Vec<TokenType> {
        tokens
            .iter()
            .filter_map(|token| {
                if token.kind == Eof {
                    None
                } else {
                    Some(token.kind)
                }
            })
            .collect()
    }

    fn assert_for_single_token(source: &str, kind: TokenType) {
        use unicode_segmentation::UnicodeSegmentation;

        let scanner = Scanner::new(source, true);
        let tokens = tokenize(scanner);
        let token = tokens.first().unwrap();
        let source_graphemes = source.graphemes(true).collect::<Vec<&str>>();
        assert_eq!(source, source_graphemes[token.start..token.end].join(""));

        let result = get_token_kinds(&tokens);
        assert_eq!(&result, &[kind]);
    }

    fn assert_for_tokens(source: &str, kinds: &[TokenType]) {
        let scanner = Scanner::new(source, true);

        let result = get_token_kinds(&tokenize(scanner));
        assert_eq!(&result, kinds);
    }

    #[test]
    fn tokenize_paren_open() {
        assert_for_single_token("(", ParenOpen);
    }

    #[test]
    fn tokenize_paren_close() {
        assert_for_single_token(")", ParenClose);
    }

    #[test]
    fn tokenize_brace_open() {
        assert_for_single_token("{", BraceOpen);
    }

    #[test]
    fn tokenize_brace_close() {
        assert_for_single_token("}", BraceClose);
    }

    #[test]
    fn tokenize_bracket_open() {
        assert_for_single_token("[", BracketOpen);
    }

    #[test]
    fn tokenize_bracket_close() {
        assert_for_single_token("]", BracketClose);
    }

    #[test]
    fn tokenize_comma() {
        assert_for_single_token(",", Comma);
    }

    #[test]
    fn tokenize_colon() {
        assert_for_single_token(":", Colon);
    }

    #[test]
    fn tokenize_semicolon() {
        assert_for_single_token(";", Semicolon);
    }

    #[test]
    fn tokenize_plus() {
        assert_for_single_token("+", Plus);
    }

    #[test]
    fn tokenize_star() {
        assert_for_single_token("*", Star);
    }

    #[test]
    fn tokenize_slash() {
        assert_for_single_token("/", Slash);
    }

    #[test]
    fn tokenize_percent() {
        assert_for_single_token("%", Percent);
    }

    #[test]
    fn tokenize_minus() {
        assert_for_single_token("-", Minus);
    }

    #[test]
    fn tokenize_arrow() {
        assert_for_single_token("->", Arrow);
    }

    #[test]
    fn tokenize_dot() {
        assert_for_single_token(".", Dot);
    }

    #[test]
    fn tokenize_range() {
        assert_for_single_token("..", Range);
    }

    #[test]
    fn tokenize_rangeequal() {
        assert_for_single_token("..=", RangeEqual);
    }

    #[test]
    fn tokenize_not() {
        assert_for_single_token("!", Not);
    }

    #[test]
    fn tokenize_notequal() {
        assert_for_single_token("!=", NotEqual);
    }

    #[test]
    fn tokenize_equal() {
        assert_for_single_token("=", Equal);
    }

    #[test]
    fn tokenize_equalequal() {
        assert_for_single_token("==", EqualEqual);
    }

    #[test]
    fn tokenize_greater() {
        assert_for_single_token(">", Greater);
    }

    #[test]
    fn tokenize_greaterequal() {
        assert_for_single_token(">=", GreaterEqual);
    }

    #[test]
    fn tokenize_less() {
        assert_for_single_token("<", Less);
    }

    #[test]
    fn tokenize_lessequal() {
        assert_for_single_token("<=", LessEqual);
    }

    #[test]
    fn tokenize_charliteral() {
        assert_for_single_token("'g'", CharLiteral);
    }

    #[test]
    fn tokenize_non_ascii_charliteral() {
        assert_for_single_token("'ğ'", CharLiteral);
    }

    #[test]
    fn tokenize_empty_charliteral() {
        assert_for_single_token("''", EmptyChar);
    }

    #[test]
    fn tokenize_empty_string() {
        assert_for_single_token("\"\"", BasicString);
    }

    #[test]
    fn tokenize_basicstring() {
        assert_for_single_token("\"I shall became manifest!\"", BasicString);
    }

    #[test]
    fn tokenize_non_ascii_basic_string() {
        assert_for_single_token("\"şuşter\"", BasicString);
    }

    #[test]
    fn tokenize_interpolatedstring() {
        assert_for_single_token("\"I shall became #{manifest}!\"", InterpolatedString);
    }

    #[test]
    fn tokenize_non_ascii_interpolatedstring() {
        assert_for_single_token("\"#{şuşter}\"", InterpolatedString);
    }

    #[test]
    fn tokenize_true() {
        assert_for_single_token("true", True)
    }

    #[test]
    fn tokenize_false() {
        assert_for_single_token("false", False)
    }

    #[test]
    fn tokenize_and() {
        assert_for_single_token("and", And)
    }

    #[test]
    fn tokenize_or() {
        assert_for_single_token("or", Or)
    }

    #[test]
    fn tokenize_super() {
        assert_for_single_token("super", Super)
    }

    #[test]
    fn tokenize_implementing() {
        assert_for_single_token("implementing", Implementing)
    }

    #[test]
    fn tokenize_self() {
        assert_for_single_token("self", ClassSelf)
    }

    #[test]
    fn tokenize_use() {
        assert_for_single_token("use", Use)
    }

    #[test]
    fn tokenize_match() {
        assert_for_single_token("match", Match)
    }

    #[test]
    fn tokenize_in() {
        assert_for_single_token("in", In)
    }

    #[test]
    fn tokenize_when() {
        assert_for_single_token("when", When)
    }

    #[test]
    fn tokenize_pass() {
        assert_for_single_token("pass", Pass)
    }

    #[test]
    fn tokenize_integer() {
        assert_for_single_token("1907", Integer);
    }

    #[test]
    fn tokenize_float() {
        assert_for_single_token("19.07", Float);
    }

    #[test]
    fn tokenize_float_without_fractional_part() {
        assert_for_single_token("42.", Float);
    }

    #[test]
    fn tokenize_constant() {
        assert_for_single_token("MindInABox", Constant);
    }

    #[test]
    fn tokenize_constant_with_number() {
        assert_for_single_token("M1ndIn4B0x", Constant);
    }

    #[test]
    fn tokenize_identifier() {
        assert_for_single_token("mind_in_a_box", Identifier);
    }

    #[test]
    fn tokenize_identifier_with_number() {
        assert_for_single_token("m1nd_1n_4_b0x", Identifier);
    }

    #[test]
    fn tokenize_decorator() {
        assert_for_single_token("@Hey!", Decorator);
    }

    #[test]
    fn tokenize_comment() {
        assert_for_single_token("# This is a comment!", Comment);
    }

    #[test]
    fn tokenize_documentation() {
        assert_for_single_token("## This is a documentation!", Documentation);
    }

    #[test]
    fn tokenize_pipe() {
        assert_for_single_token("|", Pipe);
    }

    #[test]
    fn tokenize_simple_wildcard() {
        assert_for_single_token("_", Wildcard);
    }

    #[test]
    fn tokenize_wildcard_with_name() {
        assert_for_single_token("_mind_in_a_box", Wildcard);
    }

    #[test]
    fn tokenize_binary() {
        assert_for_tokens("2 + 2", &[Integer, Plus, Integer]);
    }

    #[test]
    fn tokenize_binary_with_floats() {
        assert_for_tokens("2.5 + 2.5", &[Float, Plus, Float]);
    }

    #[test]
    fn tokenize_match_expression() {
        let source = "
match Condition:
    5 -> five()
    6 -> six()

    7 -> seven()

true
";

        assert_for_tokens(
            source,
            &[
                Match, Constant, Colon, Semicolon, Indent, Integer, Arrow, Identifier, ParenOpen,
                ParenClose, Semicolon, Integer, Arrow, Identifier, ParenOpen, ParenClose,
                Semicolon, Integer, Arrow, Identifier, ParenOpen, ParenClose, Semicolon, Dedent,
                True, Semicolon,
            ],
        )
    }

    #[test]
    fn tokenize_nested_match_expression() {
        let source = "
match cond:
    5 -> five()
    6 ->
        six()
";

        assert_for_tokens(
            source,
            &[
                Match, Identifier, Colon, Semicolon, Indent, Integer, Arrow, Identifier, ParenOpen,
                ParenClose, Semicolon, Integer, Arrow, Semicolon, Indent, Identifier, ParenOpen,
                ParenClose, Semicolon, Dedent, Dedent,
            ],
        )
    }

    #[test]
    fn tokenize_type_definition_with_expression() {
        let source = "
Constant:
    function =
        match cond:
            5 -> five()
            6 ->
                six()

    function2 = \"Hello from Constant Type\"

IO.puts(Constant().function)
";

        assert_for_tokens(
            source,
            &[
                Constant,
                Colon,
                Semicolon,
                Indent,
                Identifier,
                Equal,
                Semicolon,
                Indent,
                Match,
                Identifier,
                Colon,
                Semicolon,
                Indent,
                Integer,
                Arrow,
                Identifier,
                ParenOpen,
                ParenClose,
                Semicolon,
                Integer,
                Arrow,
                Semicolon,
                Indent,
                Identifier,
                ParenOpen,
                ParenClose,
                Semicolon,
                Dedent,
                Dedent,
                Dedent,
                Identifier,
                Equal,
                BasicString,
                Semicolon,
                Dedent,
                Constant,
                Dot,
                Identifier,
                ParenOpen,
                Constant,
                ParenOpen,
                ParenClose,
                Dot,
                Identifier,
                ParenClose,
                Semicolon,
            ],
        )
    }
}

mod span;
mod token;
mod token_type;
use std::rc::Rc;

use std::iter::{Enumerate, Peekable};

pub use span::Span;
pub use token::Token;
pub use token_type::TokenType;
use unicode_segmentation::{Graphemes, UnicodeSegmentation};

pub struct Scanner<'a> {
    chars: Peekable<Enumerate<Graphemes<'a>>>,
    line: usize,
    file: Rc<String>,
    position: usize,
    tokens: Vec<Token>,
    unit_indentation: usize,
    previous_indentation: usize,
    current_indentation: usize,
    is_beginning_of_line: bool,
    max: usize,
    catch_comments: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str, file: String, catch_comments: bool) -> Scanner<'a> {
        // Duplicate computation, fix it later
        let max = source.graphemes(true).count();
        Scanner {
            chars: source.graphemes(true).enumerate().peekable(),
            tokens: vec![],
            file: Rc::new(file),
            position: 0,
            line: 1,
            unit_indentation: 0,
            previous_indentation: 0,
            current_indentation: 0,
            is_beginning_of_line: true,
            max: max,
            catch_comments,
        }
    }

    pub fn scan(mut self) -> Vec<Token> {
        if self.is_at_end() {
            self.push(Token::eof(Span::from_scanner(&self, self.position + 1)));
            return self.tokens;
        }

        if let Some((current, char)) = self.chars.next() {
            let span = Span::from_scanner(&self, current + 1);

            match char {
                "(" => self.push(Token::paren_open(span)),
                ")" => self.push(Token::paren_close(span)),
                "{" => self.push(Token::brace_open(span)),
                "}" => self.push(Token::brace_close(span)),
                "[" => self.push(Token::bracket_open(span)),
                "]" => self.push(Token::bracket_close(span)),
                "," => self.push(Token::comma(span)),
                ":" => self.push(Token::colon(span)),
                "+" => self.push(Token::plus(span)),
                "*" => self.push(Token::star(span)),
                "/" => self.push(Token::slash(span)),
                "%" => self.push(Token::percent(span)),
                "|" => self.push(Token::pipe(span)),
                "-" => {
                    if let Some(&(_, ">")) = self.chars.peek() {
                        self.chars.next();
                        self.push(Token::arrow(span.two_char()))
                    } else {
                        self.push(Token::minus(span))
                    }
                }
                "." => {
                    if let Some(&(_, ".")) = self.chars.peek() {
                        self.chars.next();
                        if let Some(&(_, "=")) = self.chars.peek() {
                            self.chars.next();
                            self.push(Token::range_equal(span.three_char()))
                        } else {
                            self.push(Token::range(span.two_char()))
                        }
                    } else {
                        self.push(Token::dot(span))
                    }
                }
                "!" => {
                    if let Some(&(_, "=")) = self.chars.peek() {
                        self.chars.next();
                        self.push(Token::not_equal(span.two_char()))
                    } else {
                        self.push(Token::not(span))
                    }
                }
                "=" => match self.chars.peek() {
                    Some(&(_, "=")) => {
                        self.chars.next();
                        self.push(Token::equal(span.two_char()))
                    }
                    Some(&(_, ">")) => {
                        self.chars.next();
                        self.push(Token::fat_arrow(span.two_char()))
                    }
                    _ => self.push(Token::declaration(span)),
                },
                "<" => {
                    if let Some(&(_, "=")) = self.chars.peek() {
                        self.chars.next();
                        self.push(Token::less_equal(span.two_char()))
                    } else {
                        self.push(Token::less(span))
                    }
                }
                ">" => {
                    if let Some(&(_, "=")) = self.chars.peek() {
                        self.chars.next();
                        self.push(Token::greater_equal(span.two_char()))
                    } else {
                        self.push(Token::greater(span))
                    }
                }
                "'" => self.char(),
                "\"" => self.string(),
                "_" => self.wildcard(),
                "@" => self.decorator(),
                "#" => self.comment(),
                digit if digit.chars().all(|c| c.is_ascii_digit()) => self.number(digit),
                space if space.chars().all(|c| c.is_whitespace()) => {
                    self.handle_whitespace(space, span)
                }
                char if char.chars().all(|c| c.is_ascii_lowercase()) => self.identifier(char),
                char if char.chars().all(|c| c.is_ascii_uppercase()) => self.constant(char),
                char => self.push(Token::unexpected_character(span, char.to_owned())),
            };
        }

        self.scan()
    }

    fn push(&mut self, token: Token) {
        use TokenType::*;

        match token.kind {
            Endline if self.is_beginning_of_line => {
                self.position += 1;
                self.line += 1;
                self.current_indentation = 0;
            }
            Endline => {
                self.tokens.push(Token::new(
                    TokenType::Endline,
                    Span::from_scanner(self, self.position),
                    None,
                ));
                self.line += 1;
                self.position += 1;
                self.is_beginning_of_line = true;
                self.previous_indentation = self.current_indentation;
                self.current_indentation = 0;
            }
            Space if self.is_beginning_of_line => {
                self.position += 1;
                self.current_indentation += 1;
            }
            _ if self.is_beginning_of_line
                && self.current_indentation != 0
                && self.unit_indentation == 0 =>
            {
                self.tokens.push(Token::new(
                    TokenType::Indent,
                    Span::from_scanner(self, self.position),
                    None,
                ));
                self.unit_indentation = self.current_indentation;
                self.is_beginning_of_line = false;
                self.push(token);
            }
            _ if self.is_beginning_of_line
                && self.current_indentation == self.previous_indentation =>
            {
                self.is_beginning_of_line = false;
                self.push(token);
            }
            _ if self.is_beginning_of_line
                && self.previous_indentation > self.current_indentation =>
            {
                self.is_beginning_of_line = false;
                for _ in 1..=((self.previous_indentation - self.current_indentation)
                    / self.unit_indentation)
                {
                    self.tokens.push(Token::new(
                        TokenType::Dedent,
                        Span::from_scanner(self, self.position),
                        None,
                    ))
                }
                self.push(token);
            }
            _ if self.is_beginning_of_line
                && self.current_indentation - self.previous_indentation
                    != self.unit_indentation =>
            {
                self.is_beginning_of_line = false;
                self.tokens.push(Token::new(
                    TokenType::InvalidIndentation,
                    Span::from_scanner(self, self.position),
                    None,
                ));
                self.push(token);
            }
            _ if self.is_beginning_of_line
                && self.current_indentation > self.previous_indentation =>
            {
                self.is_beginning_of_line = false;
                self.tokens.push(Token::new(
                    TokenType::Indent,
                    Span::from_scanner(self, self.position),
                    None,
                ));
                self.push(token);
            }
            _ if self.is_beginning_of_line => {
                self.is_beginning_of_line = false;
                self.push(token);
            }
            Identifier | Constant | Wildcard | CharLiteral => {
                self.position = token.span.position;
                self.tokens.push(token)
            }
            _ => {
                self.position = token.span.position;
                self.tokens.push(token)
            }
        }
    }

    fn handle_whitespace(&mut self, current_str: &str, span: Span) {
        if self.is_beginning_of_line && current_str == "\n" {
            self.position += 1;
            self.line += 1;
        } else if current_str == "\n" {
            self.push(Token::endline(span))
        } else if self.is_beginning_of_line && (current_str == " " || current_str == "\t") {
            self.push(Token::space(span))
        } else {
            self.position += 1
        }
    }

    fn char(&mut self) {
        let mut char_encountered = String::from("'");
        loop {
            match self.chars.peek() {
                Some(&(current, "'")) => {
                    self.chars.next();
                    return if char_encountered != "'" {
                        char_encountered.push_str("'");
                        self.push(Token::char_literal(
                            Span::from_scanner(self, current + 1),
                            char_encountered,
                        ))
                    } else {
                        self.push(Token::empty_char(Span::from_scanner(self, current + 1)))
                    };
                }
                Some(&(current, _)) if char_encountered != "'" => {
                    return self.push(Token::unterminated_char(
                        Span::from_scanner(self, current),
                        char_encountered.to_string(),
                    ))
                }
                None => {
                    return self.push(Token::unterminated_char(
                        Span::from_scanner(self, self.max),
                        char_encountered.to_string(),
                    ))
                }
                Some(&(_, character)) => {
                    char_encountered.push_str(character);
                    self.chars.next();
                }
            }
        }
    }

    fn string(&mut self) {
        let mut content = String::from('"');

        loop {
            if let Some(&(_, char)) = self.chars.peek() {
                match char {
                    "\"" => {
                        let current = self.chars.next().map(|(i, _)| i).unwrap_or(self.max) + 1;
                        content.push('"');
                        return self.push(Token::str(Span::from_scanner(self, current), content));
                    }
                    "\n" => {
                        self.line += 1;
                        content.push_str("\n");
                        self.chars.next();
                    }
                    character => {
                        content.push_str(character);
                        self.chars.next();
                    }
                }
            } else {
                return self.push(Token::unterminated_string(
                    Span::from_scanner(self, self.max),
                    content,
                ));
            }
        }
    }

    fn number(&mut self, first_char: &str) {
        let mut is_integer = true;
        let mut content = String::from(first_char);
        loop {
            match self.chars.peek() {
                Some(&(_, char)) if char.chars().all(|c| c.is_ascii_digit()) => {
                    content.push_str(char);
                    self.chars.next();
                }
                Some(&(_, ".")) => {
                    content.push_str(".");
                    is_integer = false;
                    self.chars.next();
                }
                Some(&(current, _)) => {
                    return if is_integer {
                        self.push(Token::integer(Span::from_scanner(self, current), content))
                    } else {
                        self.push(Token::float(Span::from_scanner(self, current), content))
                    }
                }
                None => {
                    return if is_integer {
                        self.push(Token::integer(Span::from_scanner(self, self.max), content))
                    } else {
                        self.push(Token::float(Span::from_scanner(self, self.max), content))
                    }
                }
            }
        }
    }

    fn wildcard(&mut self) {
        let mut content = String::from("_");

        loop {
            match self.chars.peek() {
                Some(&(_, char))
                    if char.chars().all(|c| c.is_ascii_alphanumeric()) || char == "_" =>
                {
                    content.push_str(char);
                    self.chars.next();
                }
                Some(&(current, _)) => {
                    return self.push(Token::wildcard(Span::from_scanner(self, current), content))
                }
                None => {
                    return self.push(Token::wildcard(Span::from_scanner(self, self.max), content));
                }
            }
        }
    }

    fn identifier(&mut self, first_char: &str) {
        let mut content = String::from(first_char);
        let mut current = self.max;
        loop {
            match self.chars.peek() {
                Some(&(_, char))
                    if char.chars().all(|c| c.is_ascii_alphanumeric()) || char == "_" =>
                {
                    content.push_str(char);
                    self.chars.next();
                }
                Some(&(index, _)) => {
                    current = index;
                    break;
                }
                _ => break,
            }
        }

        self.push(match content.as_str() {
            "and" => Token::and(Span::from_scanner(self, current)),
            "or" => Token::or(Span::from_scanner(self, current)),
            "implementing" => Token::implementing(Span::from_scanner(self, current)),
            "self" => Token::class_self(Span::from_scanner(self, current)),
            "super" => Token::super_keyword(Span::from_scanner(self, current)),
            "true" => Token::true_keyword(Span::from_scanner(self, current)),
            "false" => Token::false_keyword(Span::from_scanner(self, current)),
            "use" => Token::use_keyword(Span::from_scanner(self, current)),
            "match" => Token::match_keyword(Span::from_scanner(self, current)),
            "in" => Token::in_keyword(Span::from_scanner(self, current)),
            "when" => Token::when(Span::from_scanner(self, current)),
            "pass" => Token::pass(Span::from_scanner(self, current)),
            _ => Token::identifier(Span::from_scanner(self, current), content),
        })
    }

    fn constant(&mut self, first_char: &str) {
        let mut content = String::from(first_char);

        loop {
            match self.chars.peek() {
                Some(&(_, char)) if char.chars().all(|c| c.is_ascii_alphanumeric()) => {
                    content.push_str(char);
                    self.chars.next();
                }
                Some(&(index, _)) => {
                    return self.push(Token::constant(
                        Span::from_scanner(self, index - 1),
                        content,
                    ));
                }
                None => {
                    return self.push(Token::constant(Span::from_scanner(self, self.max), content));
                }
            }
        }
    }

    fn comment(&mut self) {
        if self.catch_comments {
            let mut content = String::from("#");

            let is_documentation = if let Some(&(_, "#")) = self.chars.peek() {
                self.chars.next();
                content.push_str("#");
                true
            } else {
                false
            };

            loop {
                match self.chars.peek() {
                    Some(&(index, "\n")) => {
                        self.chars.peek();

                        if let Some(&(_, "#")) = self.chars.peek() {
                            self.chars.next();
                            content.push_str("\n");
                        } else {
                            return self.push(if is_documentation {
                                Token::documentation(Span::from_scanner(self, index), content)
                            } else {
                                Token::comment(Span::from_scanner(self, index), content)
                            });
                        }
                    }
                    Some(&(_, character)) => {
                        self.chars.next();
                        content.push_str(character)
                    }
                    None => {
                        return self.push(if is_documentation {
                            Token::documentation(Span::from_scanner(self, self.max), content)
                        } else {
                            Token::comment(Span::from_scanner(self, self.max), content)
                        });
                    }
                }
            }
        } else {
            loop {
                match self.chars.peek() {
                    Some(&(index, "\n")) => {
                        self.chars.next();
                        return self.position = index + 1;
                    }
                    Some(_) => {
                        self.chars.next();
                    }
                    None => return self.position = self.max,
                }
            }
        }
        // let mut content
    }

    fn decorator(&mut self) {
        let mut content = String::from("@");

        loop {
            match self.chars.peek() {
                Some(&(index, "\n")) => {
                    self.chars.next();

                    return self.push(Token::decorator(
                        Span::from_scanner(self, index + 1),
                        content,
                    ));
                }
                None => {
                    return self.push(Token::decorator(
                        Span::from_scanner(self, self.max),
                        content,
                    ))
                }
                Some(&(_, character)) => {
                    content.push_str(character);
                    self.chars.next();
                }
            }
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.chars.peek().is_none()
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
        let scanner = Scanner::new(source, String::from("test.lyc"), true);
        let tokens = tokenize(scanner);
        let token = tokens.first().unwrap();
        assert_eq!(source, token.to_string());

        let result = get_token_kinds(&tokens);
        assert_eq!(&result, &[kind]);
    }

    fn assert_for_tokens(source: &str, kinds: &[TokenType]) {
        let scanner = Scanner::new(source, String::from("test.lyc"), true);

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
        assert_for_single_token("=", Declaration);
    }

    #[test]
    fn tokenize_equalequal() {
        assert_for_single_token("==", Equal);
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
        assert_for_single_token("\"\"", Str);
    }

    #[test]
    fn tokenize_basicstring() {
        assert_for_single_token("\"I shall became manifest!\"", Str);
    }

    #[test]
    fn tokenize_non_ascii_basic_string() {
        assert_for_single_token("\"şuşter\"", Str);
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
    5 => five()
    6 => six()

    7 => seven()

true
";

        assert_for_tokens(
            source,
            &[
                Match, Constant, Colon, Endline, Indent, Integer, FatArrow, Identifier, ParenOpen,
                ParenClose, Endline, Integer, FatArrow, Identifier, ParenOpen, ParenClose, Endline,
                Integer, FatArrow, Identifier, ParenOpen, ParenClose, Endline, Dedent, True,
                Endline,
            ],
        )
    }

    #[test]
    fn tokenize_nested_match_expression() {
        let source = "
match cond:
    5 => five()
    6 =>
        six()
";

        assert_for_tokens(
            source,
            &[
                Match, Identifier, Colon, Endline, Indent, Integer, FatArrow, Identifier,
                ParenOpen, ParenClose, Endline, Integer, FatArrow, Endline, Indent, Identifier,
                ParenOpen, ParenClose, Endline, Dedent, Dedent,
            ],
        )
    }

    #[test]
    fn tokenize_type_definition_with_expression() {
        let source = "
Constant:
    function =
        match cond:
            5 => five()
            6 =>
                six()

    function2 = \"Hello from Constant Type\"

IO.puts(Constant().function)
";

        assert_for_tokens(
            source,
            &[
                Constant,
                Colon,
                Endline,
                Indent,
                Identifier,
                Declaration,
                Endline,
                Indent,
                Match,
                Identifier,
                Colon,
                Endline,
                Indent,
                Integer,
                FatArrow,
                Identifier,
                ParenOpen,
                ParenClose,
                Endline,
                Integer,
                FatArrow,
                Endline,
                Indent,
                Identifier,
                ParenOpen,
                ParenClose,
                Endline,
                Dedent,
                Dedent,
                Dedent,
                Identifier,
                Declaration,
                Str,
                Endline,
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
                Endline,
            ],
        )
    }
}

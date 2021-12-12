//! Utilities for lexing source code into tokens.

#[cfg(test)]
mod tests;

use std::{iter::Peekable, ops::Range, str::CharIndices};

/// The type of a given token, with additional information included for tokens
/// that need it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    String,
    Number,

    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,

    /// Indicates an error during lexing, including the error message.
    Error(String),
}

/// A token produced by lexing a string of source code.
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    span: Range<usize>,
}

impl Token {
    pub fn new(token_type: TokenType, span: Range<usize>) -> Self {
        Token { token_type, span }
    }

    pub fn line_number(&self, source_code: &str) -> usize {
        let mut newline_buffer = [0; 1];
        '\n'.encode_utf8(&mut newline_buffer);
        let newline_byte = newline_buffer[0];

        let mut line_number = 1;
        for (i, byte) in source_code.bytes().enumerate() {
            if i == self.span.start {
                return line_number;
            } else if byte == newline_byte {
                line_number += 1;
            }
        }

        // EOF spans start past the end of the source code string.
        if self.span.start == source_code.len() {
            debug_assert_eq!(self.token_type, TokenType::Eof);
            debug_assert_eq!(self.span.start, self.span.end);
            line_number
        } else {
            panic!(
                "Span ({}..{}) does not exist in the given string.",
                self.span.start, self.span.end
            );
        }
    }

    pub fn token_string<'c>(&self, source_code: &'c str) -> &'c str {
        &source_code[self.span.clone()]
    }
}

/// An iterator over Lox tokens for a source code string.
pub struct Lexer<'a> {
    source_code: &'a str,
    scanner: Peekable<CharIndices<'a>>,
    token_start_index: usize,
    current_index: usize,
    emitted_eof: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            source_code,
            scanner: source_code.char_indices().peekable(),
            token_start_index: 0,
            current_index: 0,
            emitted_eof: false,
        }
    }

    pub fn source_code(&self) -> &str {
        self.source_code
    }

    /// Get the source code string representing our current token span.
    fn current_token_string(&self) -> &str {
        &self.source_code[self.token_start_index..self.current_index]
    }

    /// Skip over whitespace and comments in the source code, incrementing our
    /// line counter on newlines.
    fn skip_whitespace(&mut self) {
        loop {
            match self.scanner_peek() {
                Some(' ') | Some('\r') | Some('\n') | Some('\t') => {
                    self.scanner_next();
                }
                Some('/') => match self.scanner_peek_next() {
                    // If there's a second slash, we're in a comment and should
                    // dump all of it. Otherwise we aren't on whitespace, so we
                    // should exit this function.
                    Some('/') => loop {
                        let peeked = self.scanner_peek();
                        if peeked.is_none() || peeked == Some('\n') {
                            break;
                        }
                        self.scanner_next();
                    },
                    _ => return,
                },
                _ => return,
            }
        }
    }

    /// Lex the next characters in the scanner as a string. The leading
    /// quotation mark is assumed to have already been consumed. An error token
    /// type is potentially returned.
    fn lex_string(&mut self) -> Token {
        loop {
            match self.scanner_peek() {
                None | Some('"') => break,
                Some(_) => {
                    self.scanner_next();
                }
            }
        }

        // If we're at the end of the source, the string was unterminated.
        if self.scanner_peek().is_none() {
            return self.create_token(TokenType::Error("Unterminated string.".to_owned()));
        }

        // If we're here we must have encountered the closing quote, so eat it.
        self.scanner_next();

        self.create_token(TokenType::String)
    }

    /// Lex the next characters in the scanner as a number. The first digit has
    /// already been consumed, but since we're not actually constructing the
    /// runtime value at the lexing stage we don't need to worry about that.
    fn lex_number(&mut self) -> Token {
        // Lex any numbers before an optional decimal point.
        self.lex_digits();

        // Look for a decimal point. We do not allow decimal points at the end
        // of numbers, so we also require a digit to be after the decimal
        // point.
        if self.scanner_peek() == Some('.')
            && self
                .scanner_peek_next()
                .map_or(false, |c| c.is_ascii_digit())
        {
            // Eat the decimal point.
            self.scanner_next();

            // Lex the fractional portion of the number.
            self.lex_digits();
        }

        self.create_token(TokenType::Number)
    }

    /// Lex the next characters in the scanner into an identifier. The first
    /// character of the identifier has already been consumed, but since we're
    /// not actually constructing the runtime value at the lexing stage we
    /// don't need to worry about that.
    fn lex_identifier(&mut self) -> Token {
        loop {
            match self.scanner_peek() {
                Some(c) if Self::is_identifier_character(c) => {
                    self.scanner_next();
                }
                _ => break,
            }
        }

        self.create_token(Self::token_type_from_identifier_string(
            self.current_token_string(),
        ))
    }

    /// Get the token type from an identifier string. This uses a trie-like
    /// technique to efficiently match any keywords one character at a time. If
    /// the identifier matches a keyword, we return that keyword's TokenType.
    /// Otherwise, we just return TokenType::Identifier.
    fn token_type_from_identifier_string(s: &str) -> TokenType {
        let bytes = s.as_bytes();
        // We know that the identifier has at least one character, so its sound
        // to call get_unchecked here. We also know all keywords have only
        // ASCII characters, meaning checking individual bytes works.
        match unsafe { bytes.get_unchecked(0) } {
            b'a' => Self::maybe_lex_keyword(&bytes[1..], "nd".as_bytes(), TokenType::And),
            b'c' => Self::maybe_lex_keyword(&bytes[1..], "lass".as_bytes(), TokenType::Class),
            b'e' => Self::maybe_lex_keyword(&bytes[1..], "lse".as_bytes(), TokenType::Else),
            b'f' => {
                if let Some(second_byte) = bytes.get(1) {
                    match second_byte {
                        b'a' => {
                            Self::maybe_lex_keyword(&bytes[2..], "lse".as_bytes(), TokenType::False)
                        }
                        b'o' => {
                            Self::maybe_lex_keyword(&bytes[2..], "r".as_bytes(), TokenType::For)
                        }
                        b'u' => {
                            Self::maybe_lex_keyword(&bytes[2..], "n".as_bytes(), TokenType::Fun)
                        }
                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            b'i' => Self::maybe_lex_keyword(&bytes[1..], "f".as_bytes(), TokenType::If),
            b'n' => Self::maybe_lex_keyword(&bytes[1..], "il".as_bytes(), TokenType::Nil),
            b'o' => Self::maybe_lex_keyword(&bytes[1..], "r".as_bytes(), TokenType::Or),
            b'p' => Self::maybe_lex_keyword(&bytes[1..], "rint".as_bytes(), TokenType::Print),
            b'r' => Self::maybe_lex_keyword(&bytes[1..], "eturn".as_bytes(), TokenType::Return),
            b's' => Self::maybe_lex_keyword(&bytes[1..], "uper".as_bytes(), TokenType::Super),
            b't' => {
                if let Some(second_byte) = bytes.get(1) {
                    match second_byte {
                        b'h' => {
                            Self::maybe_lex_keyword(&bytes[2..], "is".as_bytes(), TokenType::This)
                        }
                        b'r' => {
                            Self::maybe_lex_keyword(&bytes[2..], "ue".as_bytes(), TokenType::True)
                        }
                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            b'v' => Self::maybe_lex_keyword(&bytes[1..], "ar".as_bytes(), TokenType::Var),
            b'w' => Self::maybe_lex_keyword(&bytes[1..], "hile".as_bytes(), TokenType::While),

            _ => TokenType::Identifier,
        }
    }

    /// Try to lex the remainder of an identifier as a keyword. The remaining
    /// bytes of an identifier are passed in, plus what bytes they should match
    /// to be of the specified token type. If they match, the token type is
    /// returned. If not, it's just a regular identifier and
    /// TokenType::Identifier is returned.
    fn maybe_lex_keyword(
        remaining_identifier: &[u8],
        bytes_to_match: &[u8],
        keyword_token_type: TokenType,
    ) -> TokenType {
        if remaining_identifier == bytes_to_match {
            keyword_token_type
        } else {
            TokenType::Identifier
        }
    }

    /// Scan through as long as we're at an ASCII digit. This is used a couple
    /// times to lex a number, one for each side of the decimal point.
    fn lex_digits(&mut self) {
        loop {
            match self.scanner_peek() {
                Some(c) if c.is_ascii_digit() => {
                    self.scanner_next();
                }
                _ => break,
            }
        }
    }

    /// Determines if the given character can start an identifier. This
    /// includes alphabetic unicode characters (as defined by
    /// https://doc.rust-lang.org/std/primitive.char.html#method.is_alphabetic)
    /// and underscores. While technically for lexing purposes any identifier
    /// that isn't an ASCII digit and doesn't have some other function in the
    /// language could be used, I decided to constrain it here to characters
    /// that are culturally considered alphabetic, and underscores.
    fn is_identifier_leading_character(c: char) -> bool {
        c == '_' || c.is_alphabetic()
    }

    /// Determines if the given character can be a part of an identifier. This
    /// includes all characters that can start an identifier (see
    /// is_identifier_leading_character), as well as numeric unicode characters
    /// (as defined by
    /// https://doc.rust-lang.org/std/primitive.char.html#method.is_numeric).
    /// Other characters like unicode symbols are not included.
    fn is_identifier_character(c: char) -> bool {
        Self::is_identifier_leading_character(c) || c.is_numeric()
    }

    /// Return the next character from the scanner. We also update
    /// self.current_index to point at the index of the next character in the
    /// source string after this one was consumed.
    fn scanner_next(&mut self) -> Option<char> {
        let (_, c) = self.scanner.next()?;

        // The current index needs to point at the index of the next character.
        self.current_index = match self.scanner.peek() {
            None => self.source_code.len(),
            Some((index, _)) => *index,
        };

        Some(c)
    }

    fn scanner_next_if_eq(&mut self, expected_char: char) -> Option<char> {
        if self.scanner_peek()? == expected_char {
            self.scanner_next()
        } else {
            None
        }
    }

    fn scanner_peek(&mut self) -> Option<char> {
        let (_, c) = self.scanner.peek()?;
        Some(*c)
    }

    /// Peek one after next. This is done by cloning the scanner iterator and
    /// advancing it. Cloning is fairly cheap, and this function is needed
    /// rarely enough that caching the second peeked value doesn't seem
    /// necessary.
    fn scanner_peek_next(&self) -> Option<char> {
        let mut cloned = self.scanner.clone();
        cloned.next();
        let (_, c) = cloned.next()?;
        Some(c)
    }

    fn create_token(&self, token_type: TokenType) -> Token {
        Token::new(token_type, self.token_start_index..self.current_index)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        self.token_start_index = self.current_index;

        match self.scanner_next() {
            None if !self.emitted_eof => {
                self.emitted_eof = true;
                Some(self.create_token(TokenType::Eof))
            }
            None => None,

            Some('(') => Some(self.create_token(TokenType::LeftParen)),
            Some(')') => Some(self.create_token(TokenType::RightParen)),
            Some('{') => Some(self.create_token(TokenType::LeftBrace)),
            Some('}') => Some(self.create_token(TokenType::RightBrace)),
            Some(';') => Some(self.create_token(TokenType::Semicolon)),
            Some(',') => Some(self.create_token(TokenType::Comma)),
            Some('.') => Some(self.create_token(TokenType::Dot)),
            Some('-') => Some(self.create_token(TokenType::Minus)),
            Some('+') => Some(self.create_token(TokenType::Plus)),
            Some('/') => Some(self.create_token(TokenType::Slash)),
            Some('*') => Some(self.create_token(TokenType::Star)),

            Some('!') => Some(if self.scanner_next_if_eq('=').is_some() {
                self.create_token(TokenType::BangEqual)
            } else {
                self.create_token(TokenType::Bang)
            }),
            Some('=') => Some(if self.scanner_next_if_eq('=').is_some() {
                self.create_token(TokenType::EqualEqual)
            } else {
                self.create_token(TokenType::Equal)
            }),
            Some('<') => Some(if self.scanner_next_if_eq('=').is_some() {
                self.create_token(TokenType::LessEqual)
            } else {
                self.create_token(TokenType::Less)
            }),
            Some('>') => Some(if self.scanner_next_if_eq('=').is_some() {
                self.create_token(TokenType::GreaterEqual)
            } else {
                self.create_token(TokenType::Greater)
            }),

            Some('"') => Some(self.lex_string()),
            Some(c) if c.is_ascii_digit() => Some(self.lex_number()),

            Some(c) if Self::is_identifier_leading_character(c) => Some(self.lex_identifier()),

            Some(c) => {
                Some(self.create_token(TokenType::Error(format!("Unexpected character '{}'.", c))))
            }
        }
    }
}

use std::{
    iter::Peekable,
    str::{Chars, FromStr},
};

/// The type of a given token, with additional information included for tokens
/// that need it.
#[derive(Clone, Debug, PartialEq)]
enum TokenType {
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

    Identifier(String),
    String(String),
    Number(f64),

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

    /// Indicates an error during lexing, including the error message.
    Error(String),

    Eof,
}

/// A token produced by lexing a string of source code.
#[derive(Debug, PartialEq)]
pub struct Token {
    token_type: TokenType,
    line_number: usize,
}

impl Token {
    fn new(token_type: TokenType, line_number: usize) -> Self {
        Token {
            token_type,
            line_number,
        }
    }
}

/// An iterator over Lox tokens for a source code string.
pub struct Lexer<'a> {
    scanner: Peekable<Chars<'a>>,
    current_line: usize,
    emitted_eof: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            scanner: source_code.chars().peekable(),
            current_line: 1,
            emitted_eof: false,
        }
    }

    /// Skip over whitespace and comments in the source code, incrementing our
    /// line counter on newlines.
    fn skip_whitespace(&mut self) {
        loop {
            match self.scanner.peek() {
                Some(' ') | Some('\r') | Some('\t') => {
                    self.scanner.next();
                }
                Some('\n') => {
                    self.current_line += 1;
                    self.scanner.next();
                }
                Some('/') => match self.scanner_peek_next() {
                    // If there's a second slash, we're in a comment and should
                    // dump all of it. Otherwise we aren't on whitespace, so we
                    // should exit this function.
                    Some('/') => loop {
                        let peeked = self.scanner.peek();
                        if peeked.is_none() || peeked == Some(&'\n') {
                            break;
                        }
                        self.scanner.next();
                    },
                    _ => return,
                },
                _ => return,
            }
        }
    }

    /// Lex the next characters in the scanner into a string. The leading
    /// quotation mark is assumed to have already been consumed. An error token
    /// type is potentially returned.
    fn lex_string(&mut self) -> Token {
        let mut s = String::new();

        loop {
            match self.scanner.peek() {
                None | Some('"') => break,
                Some(c) => {
                    if *c == '\n' {
                        self.current_line += 1;
                    }

                    s.push(*c);
                    self.scanner.next();
                }
            }
        }

        // If we're at the end of the source, the string was unterminated.
        if self.scanner.peek().is_none() {
            return self.create_token(TokenType::Error(format!("Unterminated string '{}'.", s)));
        }

        // If we're here we must have encountered the closing quote, so eat it.
        self.scanner.next();

        self.create_token(TokenType::String(s))
    }

    /// Lex the next characters in the scanner into a string. Since the first
    /// digit has already been consumed, pass that digit's character in.
    fn lex_number(&mut self, first_digit: char) -> f64 {
        let mut number_string = first_digit.to_string();

        // Add any numbers before an optional decimal point.
        self.add_digits_to_string(&mut number_string);

        // Look for a decimal point. We do not allow decimal points at the end
        // of numbers, so we also require a digit to be after the decimal
        // point.
        if self.scanner.peek() == Some(&'.')
            && self
                .scanner_peek_next()
                .map_or(false, |c| c.is_ascii_digit())
        {
            // Add the decimal point to the string.
            number_string.push(self.scanner.next().unwrap());

            // Add the fractional portion of the number to the string.
            self.add_digits_to_string(&mut number_string);
        }

        f64::from_str(&number_string).unwrap()
    }

    /// Lex the next characters in the scanner into an identifier. Since the
    /// first character of the identifier has already been consumed, pass it in
    /// to the function.
    fn lex_identifier(&mut self, first_char: char) -> TokenType {
        let mut identifier_string = first_char.to_string();
        loop {
            match self.scanner.peek() {
                Some(c) if Self::is_identifier_character(*c) => {
                    identifier_string.push(*c);
                    self.scanner.next();
                }
                _ => break,
            }
        }
        Self::token_type_from_identifier_string(identifier_string)
    }

    /// Get the token type from an identifier string. This uses a trie-like
    /// technique to efficiently match any keywords one character at a time. If
    /// the identifier matches a keyword, we return that keyword's TokenType.
    /// Otherwise, we just return TokenType::Identifier.
    fn token_type_from_identifier_string(s: String) -> TokenType {
        let bytes = s.as_bytes();
        // We know that the identifier has at least one character, so its sound
        // to call get_unchecked here. We also know all keywords have only
        // ASCII characters, meaning checking individual bytes works.
        match unsafe { bytes.get_unchecked(0) } {
            /*a*/
            97 => Self::maybe_lex_keyword(&s, &bytes[1..], "nd".as_bytes(), TokenType::And),
            /*c*/
            99 => Self::maybe_lex_keyword(&s, &bytes[1..], "lass".as_bytes(), TokenType::Class),
            /*e*/
            101 => Self::maybe_lex_keyword(&s, &bytes[1..], "lse".as_bytes(), TokenType::Else),
            /*f*/
            102 => {
                if let Some(second_byte) = bytes.get(1) {
                    match second_byte {
                        /*a*/
                        97 => Self::maybe_lex_keyword(
                            &s,
                            &bytes[2..],
                            "lse".as_bytes(),
                            TokenType::False,
                        ),
                        /*o*/
                        111 => {
                            Self::maybe_lex_keyword(&s, &bytes[2..], "r".as_bytes(), TokenType::For)
                        }
                        /*u*/
                        117 => {
                            Self::maybe_lex_keyword(&s, &bytes[2..], "n".as_bytes(), TokenType::Fun)
                        }
                        _ => TokenType::Identifier(s),
                    }
                } else {
                    TokenType::Identifier(s)
                }
            }
            /*i*/
            105 => Self::maybe_lex_keyword(&s, &bytes[1..], "f".as_bytes(), TokenType::If),
            /*n*/
            110 => Self::maybe_lex_keyword(&s, &bytes[1..], "il".as_bytes(), TokenType::Nil),
            /*o*/
            111 => Self::maybe_lex_keyword(&s, &bytes[1..], "r".as_bytes(), TokenType::Or),
            /*p*/
            112 => Self::maybe_lex_keyword(&s, &bytes[1..], "rint".as_bytes(), TokenType::Print),
            /*r*/
            114 => Self::maybe_lex_keyword(&s, &bytes[1..], "eturn".as_bytes(), TokenType::Return),
            /*s*/
            115 => Self::maybe_lex_keyword(&s, &bytes[1..], "uper".as_bytes(), TokenType::Super),
            /*t*/
            116 => {
                if let Some(second_byte) = bytes.get(1) {
                    match second_byte {
                        /*h*/
                        104 => Self::maybe_lex_keyword(
                            &s,
                            &bytes[2..],
                            "is".as_bytes(),
                            TokenType::This,
                        ),
                        /*r*/
                        114 => Self::maybe_lex_keyword(
                            &s,
                            &bytes[2..],
                            "ue".as_bytes(),
                            TokenType::True,
                        ),
                        _ => TokenType::Identifier(s),
                    }
                } else {
                    TokenType::Identifier(s)
                }
            }
            /*v*/
            118 => Self::maybe_lex_keyword(&s, &bytes[1..], "ar".as_bytes(), TokenType::Var),
            /*w*/
            119 => Self::maybe_lex_keyword(&s, &bytes[1..], "hile".as_bytes(), TokenType::While),

            _ => TokenType::Identifier(s),
        }
    }

    /// Try to lex the remainder of an identifier as a keyword. The remaining
    /// bytes of an identifier are passed in, plus what bytes they should match
    /// to be of the specified token type. If they match, the token type is
    /// returned. If not, it's just a regular identifier and
    /// TokenType::Identifier is returned.
    fn maybe_lex_keyword(
        identifier_string: &str,
        remaining_identifier: &[u8],
        bytes_to_match: &[u8],
        keyword_token_type: TokenType,
    ) -> TokenType {
        if remaining_identifier == bytes_to_match {
            keyword_token_type
        } else {
            TokenType::Identifier(identifier_string.to_owned())
        }
    }

    /// Add digits in the scanner to the given string. This is used a couple
    /// times to lex a number, one for each side of the decimal point.
    fn add_digits_to_string(&mut self, s: &mut String) {
        loop {
            match self.scanner.peek() {
                Some(c) if c.is_ascii_digit() => {
                    s.push(*c);
                    self.scanner.next();
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

    /// Peek one after next. This is done by cloning the scanner iterator and
    /// advancing it. Cloning is fairly cheap, and this function is needed
    /// rarely enough that caching the second peeked value doesn't seem
    /// necessary.
    fn scanner_peek_next(&self) -> Option<char> {
        let mut cloned = self.scanner.clone();
        cloned.next();
        cloned.peek().cloned()
    }

    fn create_token(&self, token_type: TokenType) -> Token {
        Token::new(token_type, self.current_line)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        match self.scanner.next() {
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

            Some('!') => Some(if self.scanner.next_if_eq(&'=').is_some() {
                self.create_token(TokenType::BangEqual)
            } else {
                self.create_token(TokenType::Bang)
            }),
            Some('=') => Some(if self.scanner.next_if_eq(&'=').is_some() {
                self.create_token(TokenType::EqualEqual)
            } else {
                self.create_token(TokenType::Equal)
            }),
            Some('<') => Some(if self.scanner.next_if_eq(&'=').is_some() {
                self.create_token(TokenType::LessEqual)
            } else {
                self.create_token(TokenType::Less)
            }),
            Some('>') => Some(if self.scanner.next_if_eq(&'=').is_some() {
                self.create_token(TokenType::GreaterEqual)
            } else {
                self.create_token(TokenType::Greater)
            }),

            Some('"') => Some(self.lex_string()),
            Some(c) if c.is_ascii_digit() => {
                let token_type = TokenType::Number(self.lex_number(c));
                Some(self.create_token(token_type))
            }

            Some(c) if Self::is_identifier_leading_character(c) => {
                let token_type = self.lex_identifier(c);
                Some(self.create_token(token_type))
            }

            Some(c) => {
                Some(self.create_token(TokenType::Error(format!("Unexpected character '{}'.", c))))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        rand::{distributions, Rng},
        std::iter,
    };

    fn run_lexer(source_string: &str) -> Vec<Token> {
        Lexer::new(source_string).collect()
    }

    fn generate_token_string(token_type: &TokenType) -> String {
        match token_type {
            TokenType::LeftParen => "(".to_owned(),
            TokenType::RightParen => ")".to_owned(),
            TokenType::LeftBrace => "{".to_owned(),
            TokenType::RightBrace => "}".to_owned(),
            TokenType::Comma => ",".to_owned(),
            TokenType::Dot => ".".to_owned(),
            TokenType::Minus => "-".to_owned(),
            TokenType::Plus => "+".to_owned(),
            TokenType::Semicolon => ";".to_owned(),
            TokenType::Slash => "/".to_owned(),
            TokenType::Star => "*".to_owned(),
            TokenType::Bang => "!".to_owned(),
            TokenType::BangEqual => "!=".to_owned(),
            TokenType::Equal => "=".to_owned(),
            TokenType::EqualEqual => "==".to_owned(),
            TokenType::Greater => ">".to_owned(),
            TokenType::GreaterEqual => ">=".to_owned(),
            TokenType::Less => "<".to_owned(),
            TokenType::LessEqual => "<=".to_owned(),
            TokenType::Identifier(s) => s.clone(),
            TokenType::String(s) => format!("\"{}\"", s),
            TokenType::Number(f) => f.to_string(),
            TokenType::And => "and".to_owned(),
            TokenType::Class => "class".to_owned(),
            TokenType::Else => "else".to_owned(),
            TokenType::False => "false".to_owned(),
            TokenType::For => "for".to_owned(),
            TokenType::Fun => "fun".to_owned(),
            TokenType::If => "if".to_owned(),
            TokenType::Nil => "nil".to_owned(),
            TokenType::Or => "or".to_owned(),
            TokenType::Print => "print".to_owned(),
            TokenType::Return => "return".to_owned(),
            TokenType::Super => "super".to_owned(),
            TokenType::This => "this".to_owned(),
            TokenType::True => "true".to_owned(),
            TokenType::Var => "var".to_owned(),
            TokenType::While => "while".to_owned(),
            TokenType::Error(_) => unreachable!(),
            TokenType::Eof => unreachable!(),
        }
    }

    fn create_random_whitespace(rng: &mut impl Rng) -> String {
        let num_whitespace = rng.gen_range(1..30);
        rng.sample_iter(&distributions::Slice::new(&[" ", "\r", "\t"]).unwrap())
            .take(num_whitespace)
            .cloned()
            .collect::<Vec<_>>()
            .join("")
    }

    fn add_whitespace_to_token_strings(
        rng: &mut impl Rng,
        token_strings: impl Iterator<Item = String>,
    ) -> String {
        let mut result_string = String::new();

        result_string.push_str(&create_random_whitespace(rng));

        for s in token_strings {
            result_string.push_str(&s);
            result_string.push_str(&create_random_whitespace(rng));
        }

        result_string
    }

    fn create_random_source_line(
        rng: &mut impl Rng,
        token_distribution: &distributions::Slice<TokenType>,
    ) -> (Vec<TokenType>, String) {
        let num_tokens = rng.gen_range(1..1000);
        let token_types = rng
            .sample_iter(token_distribution)
            .take(num_tokens)
            .cloned()
            .collect::<Vec<_>>();

        let token_string = add_whitespace_to_token_strings(
            rng,
            token_types.iter().map(|tt| generate_token_string(tt)),
        );

        (token_types, token_string)
    }

    fn test_random_source(token_types: &[TokenType]) {
        let mut rng = rand::thread_rng();
        let token_distribution = distributions::Slice::new(token_types).unwrap();

        for _ in 0..10 {
            let mut expected_tokens = Vec::new();

            let num_lines = rng.gen_range(1..5);
            let mut lines = Vec::with_capacity(num_lines);
            for i in 0..num_lines {
                let line_number = i + 1;
                let (line_token_types, line_source) =
                    create_random_source_line(&mut rng, &token_distribution);

                expected_tokens.extend(
                    line_token_types
                        .into_iter()
                        .map(|tt| Token::new(tt, line_number)),
                );
                lines.push(line_source);
            }

            expected_tokens.push(Token::new(TokenType::Eof, num_lines));

            let line_separator = if rng.gen() { "\n" } else { "\r\n" };
            let source_string = lines.join(line_separator);

            let output = run_lexer(&source_string);
            if output != expected_tokens {
                println!(
                    "Running lexer on '{}', expecting {:?}.",
                    source_string, expected_tokens
                );
            }
            assert_eq!(run_lexer(&source_string), expected_tokens);
        }
    }

    fn test_single_token(source_string: &str, expected_token_type: TokenType) {
        assert_eq!(
            run_lexer(source_string),
            vec![
                Token::new(expected_token_type, 1),
                Token::new(TokenType::Eof, 1)
            ]
        );
    }

    fn test_tokens(source_string: &str, expected_token_types: Vec<TokenType>) {
        let expected_tokens = expected_token_types
            .into_iter()
            .map(|tt| Token::new(tt, 1))
            .chain(iter::once(Token::new(TokenType::Eof, 1)))
            .collect::<Vec<_>>();
        assert_eq!(run_lexer(source_string), expected_tokens);
    }

    fn test_is_error(source_string: &str, error_string: &str) {
        test_single_token(source_string, TokenType::Error(error_string.to_owned()));
    }

    #[test]
    fn no_tokens_on_empty_string() {
        test_tokens("", vec![]);
        test_tokens(" ", vec![]);
        test_tokens("\t", vec![]);
        test_tokens("\r", vec![]);
        assert_eq!(run_lexer("\n"), vec![Token::new(TokenType::Eof, 2)]);
        assert_eq!(run_lexer("\r\n"), vec![Token::new(TokenType::Eof, 2)]);

        test_tokens("       ", vec![]);
        assert_eq!(
            run_lexer("  \t\r  \n "),
            vec![Token::new(TokenType::Eof, 2)]
        );
    }

    #[test]
    fn single_character_tokens() {
        let tests = [
            ("(", TokenType::LeftParen),
            (")", TokenType::RightParen),
            ("{", TokenType::LeftBrace),
            ("}", TokenType::RightBrace),
            (",", TokenType::Comma),
            (".", TokenType::Dot),
            ("-", TokenType::Minus),
            ("+", TokenType::Plus),
            (";", TokenType::Semicolon),
            ("/", TokenType::Slash),
            ("*", TokenType::Star),
            ("!", TokenType::Bang),
            ("=", TokenType::Equal),
            (">", TokenType::Greater),
            ("<", TokenType::Less),
            ("a", TokenType::Identifier("a".to_owned())),
            ("_", TokenType::Identifier("_".to_owned())),
            ("京", TokenType::Identifier("京".to_owned())),
            ("0", TokenType::Number(0.0)),
            ("1", TokenType::Number(1.0)),
        ];

        tests.iter().for_each(|(source_string, token_type)| {
            test_single_token(source_string, token_type.clone());
        });

        let token_types = tests
            .iter()
            .map(|(_, token_type)| token_type.clone())
            .collect::<Vec<_>>();
        test_random_source(&token_types);
    }

    #[test]
    fn multi_character_tokens() {
        let tests = [
            ("!=", TokenType::BangEqual),
            ("==", TokenType::EqualEqual),
            (">=", TokenType::GreaterEqual),
            ("<=", TokenType::LessEqual),
            ("_abc8902tt", TokenType::Identifier("_abc8902tt".to_owned())),
            ("abc0123", TokenType::Identifier("abc0123".to_owned())),
            ("京中7δ", TokenType::Identifier("京中7δ".to_owned())),
            (
                r#"" @ abcde-**f""#,
                TokenType::String(" @ abcde-**f".to_owned()),
            ),
            ("1234", TokenType::Number(1234.0)),
            ("8.72", TokenType::Number(8.72)),
            ("and", TokenType::And),
            ("class", TokenType::Class),
            ("else", TokenType::Else),
            ("false", TokenType::False),
            ("for", TokenType::For),
            ("fun", TokenType::Fun),
            ("if", TokenType::If),
            ("nil", TokenType::Nil),
            ("or", TokenType::Or),
            ("print", TokenType::Print),
            ("return", TokenType::Return),
            ("super", TokenType::Super),
            ("this", TokenType::This),
            ("true", TokenType::True),
            ("var", TokenType::Var),
            ("while", TokenType::While),
        ];

        tests.iter().for_each(|(source_string, token_type)| {
            test_single_token(source_string, token_type.clone());
        });

        let token_types = tests
            .iter()
            .map(|(_, token_type)| token_type.clone())
            .collect::<Vec<_>>();
        test_random_source(&token_types);
    }

    #[test]
    fn identifiers() {
        let tests = [
            ("a", TokenType::Identifier("a".to_owned())),
            ("_", TokenType::Identifier("_".to_owned())),
            ("a1", TokenType::Identifier("a1".to_owned())),
            ("_1", TokenType::Identifier("_1".to_owned())),
            ("abc0123", TokenType::Identifier("abc0123".to_owned())),
            ("camelCase", TokenType::Identifier("camelCase".to_owned())),
            (
                "CapsCamelCase",
                TokenType::Identifier("CapsCamelCase".to_owned()),
            ),
            ("snake_case", TokenType::Identifier("snake_case".to_owned())),
            (
                "LOUD_SNAKE_CASE",
                TokenType::Identifier("LOUD_SNAKE_CASE".to_owned()),
            ),
            ("_京中7δ", TokenType::Identifier("_京中7δ".to_owned())),
            ("_1", TokenType::Identifier("_1".to_owned())),
            ("_1_", TokenType::Identifier("_1_".to_owned())),
            ("a①", TokenType::Identifier("a①".to_owned())),
            ("京৬", TokenType::Identifier("京৬".to_owned())),
        ];

        tests.iter().for_each(|(source_string, token_type)| {
            test_single_token(source_string, token_type.clone());
        });

        let token_types = tests
            .iter()
            .map(|(_, token_type)| token_type.clone())
            .collect::<Vec<_>>();
        test_random_source(&token_types);

        // Identifiers shouldn't start with numbers.
        test_tokens(
            "1o",
            vec![
                TokenType::Number(1.0),
                TokenType::Identifier("o".to_owned()),
            ],
        );
        test_tokens(
            "123.456abcdef",
            vec![
                TokenType::Number(123.456),
                TokenType::Identifier("abcdef".to_owned()),
            ],
        );
        test_tokens(
            "①o",
            vec![
                TokenType::Error("Unexpected character '①'.".to_owned()),
                TokenType::Identifier("o".to_owned()),
            ],
        );
        test_tokens(
            "a\u{A0}",
            vec![
                TokenType::Identifier("a".to_owned()),
                TokenType::Error("Unexpected character '\u{A0}'.".to_owned()),
            ],
        );
        test_is_error("\u{A0}", "Unexpected character '\u{A0}'.");
    }

    #[test]
    fn strings() {
        let tests = [
            (r#""""#, TokenType::String("".to_owned())),
            (r#""a""#, TokenType::String("a".to_owned())),
            (r#""1""#, TokenType::String("1".to_owned())),
            (r#""aaaaaa""#, TokenType::String("aaaaaa".to_owned())),
            (r#""11111""#, TokenType::String("11111".to_owned())),
            (
                r#"" @ abcde-**f""#,
                TokenType::String(" @ abcde-**f".to_owned()),
            ),
            (r#""京""#, TokenType::String("京".to_owned())),
            // There are no escapes inside strings.
            (r#""\""#, TokenType::String("\\".to_owned())),
            (r#""\\""#, TokenType::String("\\\\".to_owned())),
            (r#""\t""#, TokenType::String("\\t".to_owned())),
            (r#""\n""#, TokenType::String("\\n".to_owned())),
            (r#""\r""#, TokenType::String("\\r".to_owned())),
            (r#""\a""#, TokenType::String("\\a".to_owned())),
        ];

        tests.iter().for_each(|(source_string, token_type)| {
            test_single_token(source_string, token_type.clone());
        });

        let token_types = tests
            .iter()
            .map(|(_, token_type)| token_type.clone())
            .collect::<Vec<_>>();
        test_random_source(&token_types);

        test_is_error(
            r#""This is the beginning of a string"#,
            "Unterminated string 'This is the beginning of a string'.",
        );

        test_tokens(
            r#"""""#,
            vec![
                TokenType::String("".to_owned()),
                TokenType::Error("Unterminated string ''.".to_owned()),
            ],
        );
        test_tokens(
            r#""\"""#,
            vec![
                TokenType::String("\\".to_owned()),
                TokenType::Error("Unterminated string ''.".to_owned()),
            ],
        );

        // Test multi-line string.
        assert_eq!(
            run_lexer("\"this is the first line\nand this is the second line\""),
            vec![
                Token::new(
                    TokenType::String(
                        "this is the first line\nand this is the second line".to_owned(),
                    ),
                    2
                ),
                Token::new(TokenType::Eof, 2)
            ],
        );
        assert_eq!(
            run_lexer("\"this is the first line with a space \n and this is the second line\""),
            vec![
                Token::new(
                    TokenType::String(
                        "this is the first line with a space \n and this is the second line"
                            .to_owned(),
                    ),
                    2
                ),
                Token::new(TokenType::Eof, 2)
            ],
        );
    }

    #[test]
    fn numbers() {
        let tests = [
            ("0", TokenType::Number(0.0)),
            ("0.0", TokenType::Number(0.0)),
            ("0.0000", TokenType::Number(0.0)),
            ("1", TokenType::Number(1.0)),
            ("1.0", TokenType::Number(1.0)),
            ("1.000", TokenType::Number(1.0)),
            ("123456", TokenType::Number(123456.0)),
            ("123456.0", TokenType::Number(123456.0)),
            ("123456.789", TokenType::Number(123456.789)),
            ("123456.7890", TokenType::Number(123456.789)),
            ("123456.089", TokenType::Number(123456.089)),
            ("123456.0890", TokenType::Number(123456.089)),
        ];

        tests.iter().for_each(|(source_string, token_type)| {
            test_single_token(source_string, token_type.clone());
        });

        let token_types = tests
            .iter()
            .map(|(_, token_type)| token_type.clone())
            .collect::<Vec<_>>();
        test_random_source(&token_types);

        test_tokens("1.", vec![TokenType::Number(1.0), TokenType::Dot]);
        test_tokens("1. ", vec![TokenType::Number(1.0), TokenType::Dot]);
        test_tokens("1234.", vec![TokenType::Number(1234.0), TokenType::Dot]);
        test_tokens("1234. ", vec![TokenType::Number(1234.0), TokenType::Dot]);

        test_tokens(".1", vec![TokenType::Dot, TokenType::Number(1.0)]);
        test_tokens(" .1", vec![TokenType::Dot, TokenType::Number(1.0)]);
        test_tokens(".1234", vec![TokenType::Dot, TokenType::Number(1234.0)]);
        test_tokens(" .1234", vec![TokenType::Dot, TokenType::Number(1234.0)]);

        test_tokens(
            "1 . 2",
            vec![
                TokenType::Number(1.0),
                TokenType::Dot,
                TokenType::Number(2.0),
            ],
        );
        test_tokens(
            "1. 2",
            vec![
                TokenType::Number(1.0),
                TokenType::Dot,
                TokenType::Number(2.0),
            ],
        );
        test_tokens(
            "1 .2",
            vec![
                TokenType::Number(1.0),
                TokenType::Dot,
                TokenType::Number(2.0),
            ],
        );
    }

    #[test]
    fn keywords() {
        let tests = [
            ("and", TokenType::And),
            ("class", TokenType::Class),
            ("else", TokenType::Else),
            ("false", TokenType::False),
            ("for", TokenType::For),
            ("fun", TokenType::Fun),
            ("if", TokenType::If),
            ("nil", TokenType::Nil),
            ("or", TokenType::Or),
            ("print", TokenType::Print),
            ("return", TokenType::Return),
            ("super", TokenType::Super),
            ("this", TokenType::This),
            ("true", TokenType::True),
            ("var", TokenType::Var),
            ("while", TokenType::While),
            // Test that variations on keywords such as changing capitalization
            // and adding and removing characters lex properly.
            ("And", TokenType::Identifier("And".to_owned())),
            ("clAss", TokenType::Identifier("clAss".to_owned())),
            ("elsE", TokenType::Identifier("elsE".to_owned())),
            ("fals", TokenType::Identifier("fals".to_owned())),
            ("f", TokenType::Identifier("f".to_owned())),
            ("fune", TokenType::Identifier("fune".to_owned())),
            ("nl", TokenType::Identifier("nl".to_owned())),
        ];

        tests.iter().for_each(|(source_string, token_type)| {
            test_single_token(source_string, token_type.clone());
        });

        let token_types = tests
            .iter()
            .map(|(_, token_type)| token_type.clone())
            .collect::<Vec<_>>();
        test_random_source(&token_types);
    }

    #[test]
    fn invalid_characters() {
        test_single_token(
            "^",
            TokenType::Error("Unexpected character '^'.".to_owned()),
        );
        test_single_token(
            "&",
            TokenType::Error("Unexpected character '&'.".to_owned()),
        );
        test_single_token(
            "|",
            TokenType::Error("Unexpected character '|'.".to_owned()),
        );
        test_single_token(
            "#",
            TokenType::Error("Unexpected character '#'.".to_owned()),
        );
        test_single_token(
            "①",
            TokenType::Error("Unexpected character '①'.".to_owned()),
        );
    }

    #[test]
    fn all_token_types() {
        let tests = [
            ("(", TokenType::LeftParen),
            (")", TokenType::RightParen),
            ("{", TokenType::LeftBrace),
            ("}", TokenType::RightBrace),
            (",", TokenType::Comma),
            (".", TokenType::Dot),
            ("-", TokenType::Minus),
            ("+", TokenType::Plus),
            (";", TokenType::Semicolon),
            ("/", TokenType::Slash),
            ("*", TokenType::Star),
            ("!", TokenType::Bang),
            ("!=", TokenType::BangEqual),
            ("=", TokenType::Equal),
            ("==", TokenType::EqualEqual),
            (">", TokenType::Greater),
            (">=", TokenType::GreaterEqual),
            ("<", TokenType::Less),
            ("<=", TokenType::LessEqual),
            ("a", TokenType::Identifier("a".to_owned())),
            ("_", TokenType::Identifier("_".to_owned())),
            ("a1", TokenType::Identifier("a1".to_owned())),
            ("_1", TokenType::Identifier("_1".to_owned())),
            ("abc0123", TokenType::Identifier("abc0123".to_owned())),
            ("camelCase", TokenType::Identifier("camelCase".to_owned())),
            (
                "CapsCamelCase",
                TokenType::Identifier("CapsCamelCase".to_owned()),
            ),
            ("snake_case", TokenType::Identifier("snake_case".to_owned())),
            (
                "LOUD_SNAKE_CASE",
                TokenType::Identifier("LOUD_SNAKE_CASE".to_owned()),
            ),
            ("_京中7δ", TokenType::Identifier("_京中7δ".to_owned())),
            ("_1", TokenType::Identifier("_1".to_owned())),
            ("_1_", TokenType::Identifier("_1_".to_owned())),
            ("a①", TokenType::Identifier("a①".to_owned())),
            ("京৬", TokenType::Identifier("京৬".to_owned())),
            (r#""""#, TokenType::String("".to_owned())),
            (r#""a""#, TokenType::String("a".to_owned())),
            (r#""1""#, TokenType::String("1".to_owned())),
            (r#""aaaaaa""#, TokenType::String("aaaaaa".to_owned())),
            (r#""11111""#, TokenType::String("11111".to_owned())),
            (
                r#"" @ abcde-**f""#,
                TokenType::String(" @ abcde-**f".to_owned()),
            ),
            (r#""京""#, TokenType::String("京".to_owned())),
            (r#""\""#, TokenType::String("\\".to_owned())),
            (r#""\\""#, TokenType::String("\\\\".to_owned())),
            (r#""\t""#, TokenType::String("\\t".to_owned())),
            (r#""\n""#, TokenType::String("\\n".to_owned())),
            (r#""\r""#, TokenType::String("\\r".to_owned())),
            (r#""\a""#, TokenType::String("\\a".to_owned())),
            ("0", TokenType::Number(0.0)),
            ("0.0", TokenType::Number(0.0)),
            ("0.0000", TokenType::Number(0.0)),
            ("1", TokenType::Number(1.0)),
            ("1.0", TokenType::Number(1.0)),
            ("1.000", TokenType::Number(1.0)),
            ("123456", TokenType::Number(123456.0)),
            ("123456.0", TokenType::Number(123456.0)),
            ("123456.789", TokenType::Number(123456.789)),
            ("123456.7890", TokenType::Number(123456.789)),
            ("123456.089", TokenType::Number(123456.089)),
            ("123456.0890", TokenType::Number(123456.089)),
            ("and", TokenType::And),
            ("class", TokenType::Class),
            ("else", TokenType::Else),
            ("false", TokenType::False),
            ("for", TokenType::For),
            ("fun", TokenType::Fun),
            ("if", TokenType::If),
            ("nil", TokenType::Nil),
            ("or", TokenType::Or),
            ("print", TokenType::Print),
            ("return", TokenType::Return),
            ("super", TokenType::Super),
            ("this", TokenType::This),
            ("true", TokenType::True),
            ("var", TokenType::Var),
            ("while", TokenType::While),
        ];

        tests.iter().for_each(|(source_string, token_type)| {
            test_single_token(source_string, token_type.clone());
        });

        let token_types = tests
            .iter()
            .map(|(_, token_type)| token_type.clone())
            .collect::<Vec<_>>();
        test_random_source(&token_types);
    }

    #[test]
    fn comments() {
        assert_eq!(
            run_lexer("//a comment without a space"),
            vec![Token::new(TokenType::Eof, 1)]
        );
        assert_eq!(
            run_lexer("// a comment with a space"),
            vec![Token::new(TokenType::Eof, 1)]
        );
        assert_eq!(
            run_lexer("// a comment with a 京 character"),
            vec![Token::new(TokenType::Eof, 1)]
        );
        assert_eq!(
            run_lexer("and\n// a comment ending the source"),
            vec![Token::new(TokenType::And, 1), Token::new(TokenType::Eof, 2)]
        );
        assert_eq!(
            run_lexer("and\n// a comment\nor\n// another comment"),
            vec![
                Token::new(TokenType::And, 1),
                Token::new(TokenType::Or, 3),
                Token::new(TokenType::Eof, 4)
            ]
        );
    }

    #[test]
    fn multiple_lines() {
        assert_eq!(
            run_lexer("first line\nsecond line\nthird line"),
            vec![
                Token::new(TokenType::Identifier("first".to_owned()), 1),
                Token::new(TokenType::Identifier("line".to_owned()), 1),
                Token::new(TokenType::Identifier("second".to_owned()), 2),
                Token::new(TokenType::Identifier("line".to_owned()), 2),
                Token::new(TokenType::Identifier("third".to_owned()), 3),
                Token::new(TokenType::Identifier("line".to_owned()), 3),
                Token::new(TokenType::Eof, 3),
            ]
        );
    }

    #[test]
    fn eof() {
        assert_eq!(run_lexer("\n\n\n\n"), vec![Token::new(TokenType::Eof, 5)]);
        assert_eq!(
            run_lexer("\n\n\n\n// a comment"),
            vec![Token::new(TokenType::Eof, 5)]
        );
        assert_eq!(
            run_lexer("\n\n\n\n// a comment\n"),
            vec![Token::new(TokenType::Eof, 6)]
        );
    }

    #[test]
    fn valid_lox_code() {
        assert_eq!(
            run_lexer(
                r#"
            var a = "this is a string";
            var b1 = 1;
            var b2 = 8.2;
            // This is not a useful loop.
            while (b1 < 10 and false) {
                print b1;
                b1 = b1 + 1;
            }
        "#
            ),
            vec![
                Token::new(TokenType::Var, 2),
                Token::new(TokenType::Identifier("a".to_owned()), 2),
                Token::new(TokenType::Equal, 2),
                Token::new(TokenType::String("this is a string".to_owned()), 2),
                Token::new(TokenType::Semicolon, 2),
                Token::new(TokenType::Var, 3),
                Token::new(TokenType::Identifier("b1".to_owned()), 3),
                Token::new(TokenType::Equal, 3),
                Token::new(TokenType::Number(1.0), 3),
                Token::new(TokenType::Semicolon, 3),
                Token::new(TokenType::Var, 4),
                Token::new(TokenType::Identifier("b2".to_owned()), 4),
                Token::new(TokenType::Equal, 4),
                Token::new(TokenType::Number(8.2), 4),
                Token::new(TokenType::Semicolon, 4),
                Token::new(TokenType::While, 6),
                Token::new(TokenType::LeftParen, 6),
                Token::new(TokenType::Identifier("b1".to_owned()), 6),
                Token::new(TokenType::Less, 6),
                Token::new(TokenType::Number(10.0), 6),
                Token::new(TokenType::And, 6),
                Token::new(TokenType::False, 6),
                Token::new(TokenType::RightParen, 6),
                Token::new(TokenType::LeftBrace, 6),
                Token::new(TokenType::Print, 7),
                Token::new(TokenType::Identifier("b1".to_owned()), 7),
                Token::new(TokenType::Semicolon, 7),
                Token::new(TokenType::Identifier("b1".to_owned()), 8),
                Token::new(TokenType::Equal, 8),
                Token::new(TokenType::Identifier("b1".to_owned()), 8),
                Token::new(TokenType::Plus, 8),
                Token::new(TokenType::Number(1.0), 8),
                Token::new(TokenType::Semicolon, 8),
                Token::new(TokenType::RightBrace, 9),
                Token::new(TokenType::Eof, 10),
            ]
        );
    }
}

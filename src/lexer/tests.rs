use {
    super::*,
    rand::{distributions, Rng},
    std::panic::catch_unwind,
};

fn run_lexer(source_string: &str) -> Vec<Token> {
    Lexer::new(source_string).collect()
}

fn create_random_whitespace(rng: &mut impl Rng) -> String {
    let num_whitespace = rng.gen_range(1..10);
    rng.sample_iter(&distributions::Slice::new(&[" ", "\r", "\t"]).unwrap())
        .take(num_whitespace)
        .cloned()
        .collect::<Vec<_>>()
        .join("")
}

fn create_random_source_line(
    rng: &mut impl Rng,
    token_descriptor_distribution: &distributions::Slice<(&str, TokenType)>,
    index: &mut usize,
) -> (Vec<Token>, String) {
    let num_tokens = rng.gen_range(1..30);
    let token_descriptors = rng
        .sample_iter(token_descriptor_distribution)
        .take(num_tokens)
        .collect::<Vec<_>>();

    let mut tokens = Vec::new();
    let mut result_string = String::new();

    let whitespace = create_random_whitespace(rng);
    result_string.push_str(&whitespace);
    *index += whitespace.len();

    for (token_string, token_type) in token_descriptors {
        tokens.push(Token::new(
            token_type.clone(),
            *index..*index + token_string.len(),
        ));

        result_string.push_str(token_string);
        *index += token_string.len();

        let whitespace = create_random_whitespace(rng);
        result_string.push_str(&whitespace);
        *index += whitespace.len();
    }

    (tokens, result_string)
}

fn test_random_source(token_descriptors: &[(&str, TokenType)]) {
    let mut rng = rand::thread_rng();
    let token_descriptor_distribution = distributions::Slice::new(token_descriptors).unwrap();

    for _ in 0..100 {
        let mut expected_tokens = Vec::new();
        let mut source_string = String::new();
        let mut index = 0;

        let num_lines = rng.gen_range(1..5);
        for i in 0..num_lines {
            if i != 0 {
                let line_separator = if rng.gen() { "\n" } else { "\r\n" };
                source_string.push_str(line_separator);
                index += line_separator.len();
            }

            let (line_tokens, line_source) =
                create_random_source_line(&mut rng, &token_descriptor_distribution, &mut index);
            source_string.push_str(&line_source);

            for token in &line_tokens {
                assert_eq!(token.line_number(&source_string), i + 1);
            }

            expected_tokens.extend(line_tokens);
        }

        expected_tokens.push(Token::new(TokenType::Eof, index..index));

        assert_eq!(run_lexer(&source_string), expected_tokens);
    }
}

fn test_single_token(source_string: &str, expected_token_type: TokenType) {
    assert_eq!(
        run_lexer(source_string),
        vec![
            Token::new(expected_token_type, 0..source_string.len()),
            Token::new(TokenType::Eof, source_string.len()..source_string.len())
        ]
    );
}

fn test_tokens(source_string: &str, mut expected_tokens: Vec<Token>) {
    expected_tokens.push(Token::new(
        TokenType::Eof,
        source_string.len()..source_string.len(),
    ));
    assert_eq!(run_lexer(source_string), expected_tokens);
}

fn test_is_error(source_string: &str, error_string: &str) {
    test_single_token(source_string, TokenType::Error(error_string.to_owned()));
}

/// Test that the tokens from lexing the given source string have the given
/// sequence of line numbers. The last line number passed in should be for
/// the EOF token.
fn test_line_numbers(source_string: &str, expected_line_numbers: Vec<usize>) {
    let actual_line_numbers = run_lexer(source_string)
        .into_iter()
        .map(|token| token.line_number(source_string))
        .collect::<Vec<_>>();
    assert_eq!(actual_line_numbers, expected_line_numbers);
}

#[test]
fn no_tokens_on_empty_string() {
    test_tokens("", vec![]);
    test_tokens(" ", vec![]);
    test_tokens("\t", vec![]);
    test_tokens("\r", vec![]);
    test_tokens("\n", vec![]);
    test_tokens("\r\n", vec![]);

    test_tokens("       ", vec![]);
    test_tokens("  \t\r  \n ", vec![]);
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
        ("a", TokenType::Identifier),
        ("_", TokenType::Identifier),
        ("京", TokenType::Identifier),
        ("0", TokenType::Number),
        ("1", TokenType::Number),
    ];

    tests.iter().for_each(|(source_string, token_type)| {
        test_single_token(source_string, token_type.clone());
    });
    test_random_source(&tests);
}

#[test]
fn multi_character_tokens() {
    let tests = [
        ("!=", TokenType::BangEqual),
        ("==", TokenType::EqualEqual),
        (">=", TokenType::GreaterEqual),
        ("<=", TokenType::LessEqual),
        ("_abc8902tt", TokenType::Identifier),
        ("abc0123", TokenType::Identifier),
        ("京中7δ", TokenType::Identifier),
        (r#"" @ abcde-**f""#, TokenType::String),
        ("1234", TokenType::Number),
        ("8.72", TokenType::Number),
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
    test_random_source(&tests);
}

#[test]
fn identifiers() {
    let tests = [
        ("a", TokenType::Identifier),
        ("_", TokenType::Identifier),
        ("a1", TokenType::Identifier),
        ("_1", TokenType::Identifier),
        ("abc0123", TokenType::Identifier),
        ("camelCase", TokenType::Identifier),
        ("CapsCamelCase", TokenType::Identifier),
        ("snake_case", TokenType::Identifier),
        ("LOUD_SNAKE_CASE", TokenType::Identifier),
        ("_京中7δ", TokenType::Identifier),
        ("_1", TokenType::Identifier),
        ("_1_", TokenType::Identifier),
        ("a①", TokenType::Identifier),
        ("京৬", TokenType::Identifier),
    ];

    tests.iter().for_each(|(source_string, token_type)| {
        test_single_token(source_string, token_type.clone());
    });
    test_random_source(&tests);

    // Identifiers shouldn't start with numbers.
    test_tokens(
        "1o",
        vec![
            Token::new(TokenType::Number, 0..1),
            Token::new(TokenType::Identifier, 1..2),
        ],
    );
    test_tokens(
        "123.456abcdef",
        vec![
            Token::new(TokenType::Number, 0..7),
            Token::new(TokenType::Identifier, 7..13),
        ],
    );
    test_tokens(
        "①o",
        vec![
            Token::new(
                TokenType::Error("Unexpected character '①'.".to_owned()),
                0..3,
            ),
            Token::new(TokenType::Identifier, 3..4),
        ],
    );
    test_tokens(
        "a\u{A0}",
        vec![
            Token::new(TokenType::Identifier, 0..1),
            Token::new(
                TokenType::Error("Unexpected character '\u{A0}'.".to_owned()),
                1..3,
            ),
        ],
    );
    test_is_error("\u{A0}", "Unexpected character '\u{A0}'.");
}

#[test]
fn strings() {
    let tests = [
        (r#""""#, TokenType::String),
        (r#""a""#, TokenType::String),
        (r#""1""#, TokenType::String),
        (r#""aaaaaa""#, TokenType::String),
        (r#""11111""#, TokenType::String),
        (r#"" @ abcde-**f""#, TokenType::String),
        (r#""京""#, TokenType::String),
        // There are no escapes inside strings.
        (r#""\""#, TokenType::String),
        (r#""\\""#, TokenType::String),
        (r#""\t""#, TokenType::String),
        (r#""\n""#, TokenType::String),
        (r#""\r""#, TokenType::String),
        (r#""\a""#, TokenType::String),
    ];

    tests.iter().for_each(|(source_string, token_type)| {
        test_single_token(source_string, token_type.clone());
    });
    test_random_source(&tests);

    // Test multi-line strings.
    test_single_token(
        "\"this is the first line\nand this is the second line\"",
        TokenType::String,
    );
    test_single_token(
        "\"this is the first line with a space \n and this is the second line\"",
        TokenType::String,
    );
    test_tokens(
        "token1 \"line1\nline2\" token2",
        vec![
            Token::new(TokenType::Identifier, 0..6),
            Token::new(TokenType::String, 7..20),
            Token::new(TokenType::Identifier, 21..27),
        ],
    );

    test_is_error(
        r#""This is the beginning of a string"#,
        "Unterminated string.",
    );

    test_tokens(
        r#"""""#,
        vec![
            Token::new(TokenType::String, 0..2),
            Token::new(TokenType::Error("Unterminated string.".to_owned()), 2..3),
        ],
    );
    test_tokens(
        r#""\"""#,
        vec![
            Token::new(TokenType::String, 0..3),
            Token::new(TokenType::Error("Unterminated string.".to_owned()), 3..4),
        ],
    );
}

#[test]
fn numbers() {
    let tests = [
        ("0", TokenType::Number),
        ("0.0", TokenType::Number),
        ("0.0000", TokenType::Number),
        ("1", TokenType::Number),
        ("1.0", TokenType::Number),
        ("1.000", TokenType::Number),
        ("123456", TokenType::Number),
        ("123456.0", TokenType::Number),
        ("123456.789", TokenType::Number),
        ("123456.7890", TokenType::Number),
        ("123456.089", TokenType::Number),
        ("123456.0890", TokenType::Number),
    ];

    tests.iter().for_each(|(source_string, token_type)| {
        test_single_token(source_string, token_type.clone());
    });
    test_random_source(&tests);

    test_tokens(
        "1.",
        vec![
            Token::new(TokenType::Number, 0..1),
            Token::new(TokenType::Dot, 1..2),
        ],
    );
    test_tokens(
        "1. ",
        vec![
            Token::new(TokenType::Number, 0..1),
            Token::new(TokenType::Dot, 1..2),
        ],
    );
    test_tokens(
        "1234.",
        vec![
            Token::new(TokenType::Number, 0..4),
            Token::new(TokenType::Dot, 4..5),
        ],
    );
    test_tokens(
        "1234. ",
        vec![
            Token::new(TokenType::Number, 0..4),
            Token::new(TokenType::Dot, 4..5),
        ],
    );

    test_tokens(
        ".1",
        vec![
            Token::new(TokenType::Dot, 0..1),
            Token::new(TokenType::Number, 1..2),
        ],
    );
    test_tokens(
        " .1",
        vec![
            Token::new(TokenType::Dot, 1..2),
            Token::new(TokenType::Number, 2..3),
        ],
    );
    test_tokens(
        ".1234",
        vec![
            Token::new(TokenType::Dot, 0..1),
            Token::new(TokenType::Number, 1..5),
        ],
    );
    test_tokens(
        " .1234",
        vec![
            Token::new(TokenType::Dot, 1..2),
            Token::new(TokenType::Number, 2..6),
        ],
    );

    test_tokens(
        "1 . 2",
        vec![
            Token::new(TokenType::Number, 0..1),
            Token::new(TokenType::Dot, 2..3),
            Token::new(TokenType::Number, 4..5),
        ],
    );
    test_tokens(
        "1. 2",
        vec![
            Token::new(TokenType::Number, 0..1),
            Token::new(TokenType::Dot, 1..2),
            Token::new(TokenType::Number, 3..4),
        ],
    );
    test_tokens(
        "1 .2",
        vec![
            Token::new(TokenType::Number, 0..1),
            Token::new(TokenType::Dot, 2..3),
            Token::new(TokenType::Number, 3..4),
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
        ("And", TokenType::Identifier),
        ("clAss", TokenType::Identifier),
        ("elsE", TokenType::Identifier),
        ("fals", TokenType::Identifier),
        ("f", TokenType::Identifier),
        ("fune", TokenType::Identifier),
        ("nl", TokenType::Identifier),
    ];

    tests.iter().for_each(|(source_string, token_type)| {
        test_single_token(source_string, token_type.clone());
    });
    test_random_source(&tests);
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
        ("a", TokenType::Identifier),
        ("_", TokenType::Identifier),
        ("a1", TokenType::Identifier),
        ("_1", TokenType::Identifier),
        ("abc0123", TokenType::Identifier),
        ("camelCase", TokenType::Identifier),
        ("CapsCamelCase", TokenType::Identifier),
        ("snake_case", TokenType::Identifier),
        ("LOUD_SNAKE_CASE", TokenType::Identifier),
        ("_京中7δ", TokenType::Identifier),
        ("_1", TokenType::Identifier),
        ("_1_", TokenType::Identifier),
        ("a①", TokenType::Identifier),
        ("京৬", TokenType::Identifier),
        (r#""""#, TokenType::String),
        (r#""a""#, TokenType::String),
        (r#""1""#, TokenType::String),
        (r#""aaaaaa""#, TokenType::String),
        (r#""11111""#, TokenType::String),
        (r#"" @ abcde-**f""#, TokenType::String),
        (r#""京""#, TokenType::String),
        (r#""\""#, TokenType::String),
        (r#""\\""#, TokenType::String),
        (r#""\t""#, TokenType::String),
        (r#""\n""#, TokenType::String),
        (r#""\r""#, TokenType::String),
        (r#""\a""#, TokenType::String),
        ("0", TokenType::Number),
        ("0.0", TokenType::Number),
        ("0.0000", TokenType::Number),
        ("1", TokenType::Number),
        ("1.0", TokenType::Number),
        ("1.000", TokenType::Number),
        ("123456", TokenType::Number),
        ("123456.0", TokenType::Number),
        ("123456.789", TokenType::Number),
        ("123456.7890", TokenType::Number),
        ("123456.089", TokenType::Number),
        ("123456.0890", TokenType::Number),
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
    test_random_source(&tests);
}

#[test]
fn comments() {
    test_tokens("//a comment without a space", vec![]);
    test_tokens("// a comment with a space", vec![]);
    test_tokens("// a comment with a 京 character", vec![]);
    test_tokens(
        "and\n// a comment ending the source",
        vec![Token::new(TokenType::And, 0..3)],
    );
    test_tokens(
        "and\n// a comment\nor\n// another comment",
        vec![
            Token::new(TokenType::And, 0..3),
            Token::new(TokenType::Or, 17..19),
        ],
    );

    test_tokens(
        "/ not a comment",
        vec![
            Token::new(TokenType::Slash, 0..1),
            Token::new(TokenType::Identifier, 2..5),
            Token::new(TokenType::Identifier, 6..7),
            Token::new(TokenType::Identifier, 8..15),
        ],
    );
    test_tokens(
        " / not a comment",
        vec![
            Token::new(TokenType::Slash, 1..2),
            Token::new(TokenType::Identifier, 3..6),
            Token::new(TokenType::Identifier, 7..8),
            Token::new(TokenType::Identifier, 9..16),
        ],
    );
    test_tokens(
        "/not a comment",
        vec![
            Token::new(TokenType::Slash, 0..1),
            Token::new(TokenType::Identifier, 1..4),
            Token::new(TokenType::Identifier, 5..6),
            Token::new(TokenType::Identifier, 7..14),
        ],
    );
    test_tokens(
        " /not a comment",
        vec![
            Token::new(TokenType::Slash, 1..2),
            Token::new(TokenType::Identifier, 2..5),
            Token::new(TokenType::Identifier, 6..7),
            Token::new(TokenType::Identifier, 8..15),
        ],
    );
}

#[test]
fn multiple_lines() {
    test_tokens(
        "first line\nsecond line\nthird line",
        vec![
            Token::new(TokenType::Identifier, 0..5),
            Token::new(TokenType::Identifier, 6..10),
            Token::new(TokenType::Identifier, 11..17),
            Token::new(TokenType::Identifier, 18..22),
            Token::new(TokenType::Identifier, 23..28),
            Token::new(TokenType::Identifier, 29..33),
        ],
    );
}

#[test]
fn line_numbers() {
    test_line_numbers("identifier", vec![1, 1]);
    test_line_numbers(
        "first line\nsecond line\nthird line",
        vec![1, 1, 2, 2, 3, 3, 3],
    );
    test_line_numbers(
        "first line\nsecond line\nthird line\n",
        vec![1, 1, 2, 2, 3, 3, 4],
    );

    test_line_numbers("\"abc\"", vec![1, 1]);
    test_line_numbers("\"abc\ndef\"", vec![1, 2]);
    test_line_numbers("\n\"abc\ndef\"", vec![2, 3]);

    test_line_numbers("token1\n// this is a comment\ntoken2", vec![1, 3, 3]);

    test_line_numbers("token1 \n token2 \n", vec![1, 2, 3]);
    test_line_numbers("token1 \n token2 \n ", vec![1, 2, 3]);

    assert_eq!(
        *catch_unwind(|| { Token::new(TokenType::LeftParen, 5..6).line_number("abc") })
            .unwrap_err()
            .downcast_ref::<String>()
            .unwrap(),
        "Span (5..6) does not exist in the given string.".to_owned(),
    );
    assert_eq!(
        *catch_unwind(|| { Token::new(TokenType::LeftParen, 4..5).line_number("abc") })
            .unwrap_err()
            .downcast_ref::<String>()
            .unwrap(),
        "Span (4..5) does not exist in the given string.".to_owned(),
    );
    assert_eq!(
        *catch_unwind(|| { Token::new(TokenType::Eof, 3..4).line_number("abc") })
            .unwrap_err()
            .downcast_ref::<String>()
            .unwrap(),
        "assertion failed: `(left == right)`\n  left: `3`,\n right: `4`".to_owned(),
    );
    assert_eq!(
        *catch_unwind(|| { Token::new(TokenType::LeftParen, 3..3).line_number("abc") })
            .unwrap_err()
            .downcast_ref::<String>()
            .unwrap(),
        "assertion failed: `(left == right)`\n  left: `LeftParen`,\n right: `Eof`".to_owned(),
    );
    assert_eq!(
        *catch_unwind(|| { Token::new(TokenType::LeftParen, 3..4).line_number("abc") })
            .unwrap_err()
            .downcast_ref::<String>()
            .unwrap(),
        "assertion failed: `(left == right)`\n  left: `LeftParen`,\n right: `Eof`".to_owned(),
    );
}

#[test]
fn token_string() {
    assert_eq!(
        Token::new(TokenType::LeftParen, 0..3).token_string("abc"),
        "abc"
    );
    assert_eq!(
        Token::new(TokenType::LeftParen, 7..10).token_string("before abc after"),
        "abc"
    );
}

#[test]
fn eof() {
    test_tokens("\n\n\n\n", vec![]);
    test_tokens("\n\n\n\n// a comment", vec![]);
    test_tokens("\n\n\n\n// a comment\n", vec![]);
}

#[test]
fn valid_lox_code() {
    test_tokens(
        r#"
            var a = "this is a string";
            var b1 = 1;
            var b2 = 8.2;
            // This is not a useful loop.
            while (b1 < 10 and false) {
                print b1;
                b1 = b1 + 1;
            }
        "#,
        vec![
            Token::new(TokenType::Var, 13..16),
            Token::new(TokenType::Identifier, 17..18),
            Token::new(TokenType::Equal, 19..20),
            Token::new(TokenType::String, 21..39),
            Token::new(TokenType::Semicolon, 39..40),
            Token::new(TokenType::Var, 53..56),
            Token::new(TokenType::Identifier, 57..59),
            Token::new(TokenType::Equal, 60..61),
            Token::new(TokenType::Number, 62..63),
            Token::new(TokenType::Semicolon, 63..64),
            Token::new(TokenType::Var, 77..80),
            Token::new(TokenType::Identifier, 81..83),
            Token::new(TokenType::Equal, 84..85),
            Token::new(TokenType::Number, 86..89),
            Token::new(TokenType::Semicolon, 89..90),
            Token::new(TokenType::While, 145..150),
            Token::new(TokenType::LeftParen, 151..152),
            Token::new(TokenType::Identifier, 152..154),
            Token::new(TokenType::Less, 155..156),
            Token::new(TokenType::Number, 157..159),
            Token::new(TokenType::And, 160..163),
            Token::new(TokenType::False, 164..169),
            Token::new(TokenType::RightParen, 169..170),
            Token::new(TokenType::LeftBrace, 171..172),
            Token::new(TokenType::Print, 189..194),
            Token::new(TokenType::Identifier, 195..197),
            Token::new(TokenType::Semicolon, 197..198),
            Token::new(TokenType::Identifier, 215..217),
            Token::new(TokenType::Equal, 218..219),
            Token::new(TokenType::Identifier, 220..222),
            Token::new(TokenType::Plus, 223..224),
            Token::new(TokenType::Number, 225..226),
            Token::new(TokenType::Semicolon, 226..227),
            Token::new(TokenType::RightBrace, 240..241),
        ],
    );
}

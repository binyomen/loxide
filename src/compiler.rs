//! Utilities for compiling source code into bytecode chunks.

#[cfg(test)]
mod tests;

use {
    crate::{
        chunk::Chunk,
        lexer::{Lexer, Token, TokenType},
        value::Value,
    },
    std::{borrow::Borrow, fmt, hint::unreachable_unchecked, str::FromStr},
};

/// Precedences for various infix operators. Values defined lower in the enum
/// have higher discriminant values, and thus represent higher precedences.
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
enum InfixOperatorPrecedence {
    Base,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl InfixOperatorPrecedence {
    /// Return a new precedence that's one level higher than the given one.
    /// Since there is no precedence higher than Primary, we have a debug
    /// assert that makes sure Primary isn't passed in. In release builds,
    /// passing in Primary results in undefined behavior.
    fn add_one(self) -> Self {
        debug_assert!(self != Self::Primary);

        match self {
            Self::Base => Self::Assignment,
            Self::Assignment => Self::Or,
            Self::Or => Self::And,
            Self::And => Self::Equality,
            Self::Equality => Self::Comparison,
            Self::Comparison => Self::Term,
            Self::Term => Self::Factor,
            Self::Factor => Self::Unary,
            Self::Unary => Self::Call,
            Self::Call => Self::Primary,
            Self::Primary => unsafe { unreachable_unchecked() },
        }
    }
}

/// An alias for a function used in compilation. It takes in a Compiler and the
/// current token, and continues processing more tokens for its given syntax.
type CompilationFunction<'a> = fn(&mut Compiler<'a>, Token);

fn get_infix_operator_precedence(token_type: &TokenType) -> InfixOperatorPrecedence {
    match token_type {
        TokenType::Minus => InfixOperatorPrecedence::Term,
        TokenType::Plus => InfixOperatorPrecedence::Term,
        TokenType::Slash => InfixOperatorPrecedence::Factor,
        TokenType::Star => InfixOperatorPrecedence::Factor,
        _ => InfixOperatorPrecedence::Base,
    }
}

/// The data type which manages compilation from source code to bytecode.
pub struct Compiler<'a> {
    lexer: Lexer<'a>,
    chunk: Chunk,
    had_error: bool,
    in_panic_mode: bool,
    peeked_token: Option<Option<Token>>,

    #[cfg(test)]
    error_printer: Box<dyn FnMut(fmt::Arguments) + 'a>,
}

impl<'a> Compiler<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            chunk: Chunk::new(),
            had_error: false,
            in_panic_mode: false,
            peeked_token: None,

            #[cfg(test)]
            error_printer: Box::new(|format_args| eprint!("{}", format_args)),
        }
    }

    #[cfg(test)]
    fn with_error_printer(
        lexer: Lexer<'a>,
        error_printer: impl FnMut(fmt::Arguments) + 'a,
    ) -> Self {
        Self {
            error_printer: Box::new(error_printer),
            ..Self::new(lexer)
        }
    }

    /// Compile the source code represented by the given Lexer into bytecode,
    /// using a single-pass Pratt parsing technique.
    pub fn compile(mut self) -> Result<Chunk, ()> {
        if let Some(token) = self.peek_next_token() {
            // The source code is empty. Return an empty chunk.
            if token.token_type == TokenType::Eof {
                return self.finalize_and_return();
            }
        }

        self.compile_expression();

        // Don't add the return instruction if we haven't actually encountered an EOF.
        if let Some(eof_token) = self.expect_token_type(TokenType::Eof, "Expected end of file.") {
            self.chunk
                .add_return_instruction(self.line_number(&eof_token));
        }

        self.finalize_and_return()
    }

    /// After compilation has finished, run any remaining code and determine
    /// whether to return with success or an error.
    fn finalize_and_return(self) -> Result<Chunk, ()> {
        // had_error is set by error handling functions during compilation, so
        // we only need to check it when compilation has finished.
        if self.had_error {
            Err(())
        } else {
            Ok(self.chunk)
        }
    }

    /// Parse the upcoming tokens using a given infix operator precedence. We
    /// expect the next token to be a prefix, such as a number or a minus.
    ///
    /// After parsing the prefix expression, we try to parse infix expressions
    /// of the given precedence or higher, returning once the precedence is too
    /// low.
    fn parse_with_precedence(&mut self, precedence: InfixOperatorPrecedence) {
        debug_assert!(precedence > InfixOperatorPrecedence::Base);

        let prefix_token = match self.next_token() {
            None => {
                self.report_error(self.create_eof_token(), "Unexpected end of file.");
                return;
            }
            Some(t) => t,
        };

        // Execute the prefix operation. If the token doesn't have a prefix
        // compilation function, then there must be a syntax error.
        let prefix_compilation_function =
            match self.get_prefix_compilation_function(&prefix_token.token_type) {
                None => {
                    self.report_error(prefix_token, "Unexpected token.");
                    return;
                }
                Some(f) => f,
            };
        prefix_compilation_function(self, prefix_token);

        // Keep consuming infix operators until the precedence is too low.
        loop {
            // Break out of the loop if the next token is None, or if its
            // precedence is too low. Don't consume the token yet in case we
            // don't want to process it here.
            match self.peek_next_token() {
                None => break,
                Some(token) if get_infix_operator_precedence(&token.token_type) < precedence => {
                    break;
                }
                _ => (),
            }

            // We've determined this token is an infix operator based on its
            // precedence, so we can actually consume it now. Also, it must
            // have an infix compilation function.
            let infix_token = self.next_token().unwrap();

            let infix_compilation_function = self
                .get_infix_compilation_function(&infix_token.token_type)
                .unwrap();
            infix_compilation_function(self, infix_token);
        }
    }

    /// Compile top level expressions.
    fn compile_expression(&mut self) {
        self.parse_with_precedence(InfixOperatorPrecedence::Assignment);
    }

    /// Compile parenthesis groupings.
    fn compile_grouping(&mut self, token: Token) {
        debug_assert!(token.token_type == TokenType::LeftParen);

        self.compile_expression();
        self.expect_token_type(TokenType::RightParen, "Expected closing parenthesis.");
    }

    fn compile_number(&mut self, token: Token) {
        let n = f64::from_str(self.token_string(&token)).unwrap();
        let line_number = self.line_number(&token);
        let constant_index = self.add_constant(Value::Number(n), token);
        self.chunk
            .add_constant_instruction(constant_index, line_number);
    }

    /// Compile expressions that start with unary operators like minus.
    fn compile_unary_expression(&mut self, token: Token) {
        // Compile the operand.
        self.parse_with_precedence(InfixOperatorPrecedence::Unary);

        match token.token_type {
            TokenType::Minus => self.chunk.add_negate_instruction(self.line_number(&token)),
            _ => unreachable!(),
        }
    }

    /// Compile expressions that contain binary infix operators like plus or
    /// star. The passed in token is expected to be the infix operator.
    /// Everything before the infix operator has already been parsed.
    fn compile_binary_expression(&mut self, token: Token) {
        self.parse_with_precedence(get_infix_operator_precedence(&token.token_type).add_one());

        match token.token_type {
            TokenType::Plus => self.chunk.add_add_instruction(self.line_number(&token)),
            TokenType::Minus => self
                .chunk
                .add_subtract_instruction(self.line_number(&token)),
            TokenType::Star => self
                .chunk
                .add_multiply_instruction(self.line_number(&token)),
            TokenType::Slash => self.chunk.add_divide_instruction(self.line_number(&token)),
            _ => unreachable!(),
        }
    }

    fn add_constant(&mut self, value: Value, token: Token) -> u8 {
        match self.chunk.add_constant(value) {
            None => {
                self.report_error(token, "Each chunk only allows up to 256 constants.");
                // Just return 0 as the index. Since we reported an error, the
                // code won't be executed anyway.
                0
            }
            Some(constant_index) => constant_index,
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.peeked_token.take() {
            None => loop {
                match self.lexer.next() {
                    None => break None,
                    Some(token) => match &token.token_type {
                        TokenType::Error(message) => {
                            self.report_error(&token, message);
                        }
                        _ => break Some(token),
                    },
                }
            },
            Some(token) => token,
        }
    }

    fn peek_next_token(&mut self) -> Option<&Token> {
        if self.peeked_token == None {
            self.peeked_token = Some(self.next_token());
        }

        self.peeked_token.as_ref().unwrap().as_ref()
    }

    /// Report an error if the next token isn't of the given token type. Return
    /// the token if it is of the given type. Otherwise don't consume it and
    /// return None.
    fn expect_token_type(
        &mut self,
        expected_token_type: TokenType,
        message: &str,
    ) -> Option<Token> {
        match self.peek_next_token() {
            None => {
                self.report_error(self.create_eof_token(), "Unexpected end of file.");
                None
            }
            Some(token) if token.token_type != expected_token_type => {
                let token = (*token).clone();
                self.report_error(token, message);
                None
            }
            Some(_) => self.next_token(),
        }
    }

    fn create_eof_token(&self) -> Token {
        let source_code_len = self.lexer.source_code().len();
        Token::new(TokenType::Eof, source_code_len..source_code_len)
    }

    /// Report an error with the given message on the given token. If we're in
    /// panic mode, suppress the error. Otherwise, report the error, enable
    /// panic mode, and set an internal flag saying we had an error.
    fn report_error<'t>(&mut self, token: impl Borrow<Token> + 't, message: &str) {
        // If we're in panic mode, we should suppress all errors until we get
        // back into a valid state. That way the user isn't faced with a wall
        // of cascading errors.
        if self.in_panic_mode {
            return;
        }
        self.in_panic_mode = true;

        let token = token.borrow();

        self.print_error(format_args!("[line {}] Error", self.line_number(token)));

        match &token.token_type {
            TokenType::Eof => self.print_error(format_args!(" at end")),
            TokenType::Error(_) => (), // We print the error message below.
            _ => self.print_error(format_args!(
                " at '{}'",
                self.token_string(token).to_owned()
            )),
        }

        self.print_error(format_args!(": {}\n", message));
        self.had_error = true;
    }

    fn line_number(&self, token: &Token) -> usize {
        token.line_number(self.lexer.source_code())
    }

    fn token_string(&self, token: &Token) -> &str {
        token.token_string(self.lexer.source_code())
    }

    fn get_prefix_compilation_function(
        &self,
        token_type: &TokenType,
    ) -> Option<CompilationFunction<'a>> {
        match token_type {
            TokenType::LeftParen => Some(Self::compile_grouping),
            TokenType::Minus => Some(Self::compile_unary_expression),
            TokenType::Number => Some(Self::compile_number),
            _ => None,
        }
    }

    fn get_infix_compilation_function(
        &self,
        token_type: &TokenType,
    ) -> Option<CompilationFunction<'a>> {
        match token_type {
            TokenType::Minus => Some(Self::compile_binary_expression),
            TokenType::Plus => Some(Self::compile_binary_expression),
            TokenType::Slash => Some(Self::compile_binary_expression),
            TokenType::Star => Some(Self::compile_binary_expression),
            _ => None,
        }
    }

    #[cfg(not(test))]
    fn print_error(&mut self, format_args: fmt::Arguments) {
        eprint!("{}", format_args);
    }

    #[cfg(test)]
    fn print_error(&mut self, format_args: fmt::Arguments) {
        (self.error_printer)(format_args);
    }
}

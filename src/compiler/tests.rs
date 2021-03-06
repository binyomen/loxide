use {super::*, crate::chunk::Instruction, std::panic::catch_unwind};

fn vn(n: f64) -> Value {
    Value::Number(n)
}

fn compile(source_code: &str) -> Result<Chunk, ()> {
    Compiler::new(Lexer::new(source_code)).compile()
}

fn compile_with_error_output(source_code: &str) -> (Result<Chunk, ()>, String) {
    let mut output = String::new();
    let compiler = Compiler::with_error_printer(Lexer::new(source_code), |format_args| {
        output.push_str(&format_args.to_string());
    });

    (compiler.compile(), output)
}

fn test_compilation<const M: usize, const N: usize>(
    source_code: &str,
    expected_instructions: [Instruction; M],
    expected_constants: [Value; N],
    expected_lines: [usize; M],
) {
    let chunk = compile(source_code).unwrap();
    let mut cursor = chunk.cursor();

    let mut instructions = Vec::new();
    let mut lines = Vec::new();
    let mut offset = cursor.offset();
    while let Some(instruction) = cursor.read_instruction() {
        lines.push(chunk.line_at_offset(offset));
        instructions.push(instruction);
        offset = cursor.offset();
    }

    let constants = chunk.constants();

    assert_eq!(instructions.as_slice(), &expected_instructions);
    assert_eq!(lines.as_slice(), &expected_lines);
    assert_eq!(constants.as_slice(), &expected_constants);
}

fn test_error_output(source_code: &str, expected_output: &str) {
    let (result, output) = compile_with_error_output(source_code);
    assert!(result.is_err());
    assert_eq!(&output, expected_output);
}

#[test]
fn add_one() {
    assert_eq!(
        InfixOperatorPrecedence::Base.add_one(),
        InfixOperatorPrecedence::Assignment
    );
    assert_eq!(
        InfixOperatorPrecedence::Assignment.add_one(),
        InfixOperatorPrecedence::Or
    );
    assert_eq!(
        InfixOperatorPrecedence::Or.add_one(),
        InfixOperatorPrecedence::And
    );
    assert_eq!(
        InfixOperatorPrecedence::And.add_one(),
        InfixOperatorPrecedence::Equality
    );
    assert_eq!(
        InfixOperatorPrecedence::Equality.add_one(),
        InfixOperatorPrecedence::Comparison
    );
    assert_eq!(
        InfixOperatorPrecedence::Comparison.add_one(),
        InfixOperatorPrecedence::Term
    );
    assert_eq!(
        InfixOperatorPrecedence::Term.add_one(),
        InfixOperatorPrecedence::Factor
    );
    assert_eq!(
        InfixOperatorPrecedence::Factor.add_one(),
        InfixOperatorPrecedence::Unary
    );
    assert_eq!(
        InfixOperatorPrecedence::Unary.add_one(),
        InfixOperatorPrecedence::Call
    );
    assert_eq!(
        InfixOperatorPrecedence::Call.add_one(),
        InfixOperatorPrecedence::Primary
    );

    assert_eq!(
        *catch_unwind(|| { InfixOperatorPrecedence::Primary.add_one() })
            .unwrap_err()
            .downcast_ref::<&str>()
            .unwrap(),
        "assertion failed: self != Self::Primary".to_owned(),
    );
}

#[test]
fn infix_operator_precedence_ord() {
    assert!(InfixOperatorPrecedence::Base < InfixOperatorPrecedence::Assignment);
    assert!(InfixOperatorPrecedence::Assignment < InfixOperatorPrecedence::Or);
    assert!(InfixOperatorPrecedence::Or < InfixOperatorPrecedence::And);
    assert!(InfixOperatorPrecedence::And < InfixOperatorPrecedence::Equality);
    assert!(InfixOperatorPrecedence::Equality < InfixOperatorPrecedence::Comparison);
    assert!(InfixOperatorPrecedence::Comparison < InfixOperatorPrecedence::Term);
    assert!(InfixOperatorPrecedence::Term < InfixOperatorPrecedence::Factor);
    assert!(InfixOperatorPrecedence::Factor < InfixOperatorPrecedence::Unary);
    assert!(InfixOperatorPrecedence::Unary < InfixOperatorPrecedence::Call);
    assert!(InfixOperatorPrecedence::Call < InfixOperatorPrecedence::Primary);
}

#[test]
fn compile_empty() {
    test_compilation("", [], [], []);
    test_compilation("\n", [], [], []);
}

#[test]
fn compile_number_literals() {
    test_compilation(
        "1",
        [Instruction::Constant(0), Instruction::Return],
        [vn(1.0)],
        [1; 2],
    );
    test_compilation(
        "12345.6789",
        [Instruction::Constant(0), Instruction::Return],
        [vn(12345.6789)],
        [1; 2],
    );
    test_compilation(
        "0",
        [Instruction::Constant(0), Instruction::Return],
        [vn(0.0)],
        [1; 2],
    );
    test_compilation(
        "00000.00000",
        [Instruction::Constant(0), Instruction::Return],
        [vn(0.0)],
        [1; 2],
    );
}

#[test]
fn compile_keyword_literals() {
    test_compilation("nil", [Instruction::Nil, Instruction::Return], [], [1; 2]);
    test_compilation("true", [Instruction::True, Instruction::Return], [], [1; 2]);
    test_compilation(
        "false",
        [Instruction::False, Instruction::Return],
        [],
        [1; 2],
    );
}

#[test]
fn compile_unary_operators() {
    test_compilation(
        "-1",
        [
            Instruction::Constant(0),
            Instruction::Negate,
            Instruction::Return,
        ],
        [vn(1.0)],
        [1; 3],
    );
    test_compilation(
        "-12345.6789",
        [
            Instruction::Constant(0),
            Instruction::Negate,
            Instruction::Return,
        ],
        [vn(12345.6789)],
        [1; 3],
    );
    test_compilation(
        "-0",
        [
            Instruction::Constant(0),
            Instruction::Negate,
            Instruction::Return,
        ],
        [vn(0.0)],
        [1; 3],
    );
    test_compilation(
        "-000000.000000",
        [
            Instruction::Constant(0),
            Instruction::Negate,
            Instruction::Return,
        ],
        [vn(0.0)],
        [1; 3],
    );

    test_compilation(
        "----0",
        [
            Instruction::Constant(0),
            Instruction::Negate,
            Instruction::Negate,
            Instruction::Negate,
            Instruction::Negate,
            Instruction::Return,
        ],
        [vn(0.0)],
        [1; 6],
    );

    test_compilation(
        "-(0)",
        [
            Instruction::Constant(0),
            Instruction::Negate,
            Instruction::Return,
        ],
        [vn(0.0)],
        [1; 3],
    );

    test_compilation(
        "-0 + 1",
        [
            Instruction::Constant(0),
            Instruction::Negate,
            Instruction::Constant(1),
            Instruction::Add,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0)],
        [1; 5],
    );
    test_compilation(
        "-(0 + 1)",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Add,
            Instruction::Negate,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0)],
        [1; 5],
    );
    test_compilation(
        "-0 * 1",
        [
            Instruction::Constant(0),
            Instruction::Negate,
            Instruction::Constant(1),
            Instruction::Multiply,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0)],
        [1; 5],
    );
    test_compilation(
        "-(0 * 1)",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Multiply,
            Instruction::Negate,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0)],
        [1; 5],
    );

    test_compilation(
        "!true",
        [Instruction::True, Instruction::Not, Instruction::Return],
        [],
        [1; 3],
    );
    test_compilation(
        "!false",
        [Instruction::False, Instruction::Not, Instruction::Return],
        [],
        [1; 3],
    );
    test_compilation(
        "!!false",
        [
            Instruction::False,
            Instruction::Not,
            Instruction::Not,
            Instruction::Return,
        ],
        [],
        [1; 4],
    );

    test_compilation(
        "!nil",
        [Instruction::Nil, Instruction::Not, Instruction::Return],
        [],
        [1; 3],
    );
    test_compilation(
        "!0",
        [
            Instruction::Constant(0),
            Instruction::Not,
            Instruction::Return,
        ],
        [vn(0.0)],
        [1; 3],
    );
}

#[test]
fn compile_binary_operators() {
    test_compilation(
        "0 + 1",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Add,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0)],
        [1; 4],
    );
    test_compilation(
        "0 - 1",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Subtract,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0)],
        [1; 4],
    );
    test_compilation(
        "0 * 1",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Multiply,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0)],
        [1; 4],
    );
    test_compilation(
        "0 / 1",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Divide,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0)],
        [1; 4],
    );

    test_compilation(
        "0 + 1 + 2",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Add,
            Instruction::Constant(2),
            Instruction::Add,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0), vn(2.0)],
        [1; 6],
    );
    test_compilation(
        "0 - 1 - 2",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Subtract,
            Instruction::Constant(2),
            Instruction::Subtract,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0), vn(2.0)],
        [1; 6],
    );
    test_compilation(
        "0 * 1 * 2",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Multiply,
            Instruction::Constant(2),
            Instruction::Multiply,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0), vn(2.0)],
        [1; 6],
    );
    test_compilation(
        "0 / 1 / 2",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Divide,
            Instruction::Constant(2),
            Instruction::Divide,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0), vn(2.0)],
        [1; 6],
    );
}

#[test]
fn binary_operator_precedence() {
    test_compilation(
        "0 + 1 * 2",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Constant(2),
            Instruction::Multiply,
            Instruction::Add,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0), vn(2.0)],
        [1; 6],
    );
    test_compilation(
        "0 + 1 / 2",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Constant(2),
            Instruction::Divide,
            Instruction::Add,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0), vn(2.0)],
        [1; 6],
    );
    test_compilation(
        "0 - 1 * 2",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Constant(2),
            Instruction::Multiply,
            Instruction::Subtract,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0), vn(2.0)],
        [1; 6],
    );
    test_compilation(
        "0 - 1 / 2",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Constant(2),
            Instruction::Divide,
            Instruction::Subtract,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0), vn(2.0)],
        [1; 6],
    );

    test_compilation(
        "0 * 1 / 2 + 3 - 4 * 5 * 6 + 7 / 8",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Multiply,
            Instruction::Constant(2),
            Instruction::Divide,
            Instruction::Constant(3),
            Instruction::Add,
            Instruction::Constant(4),
            Instruction::Constant(5),
            Instruction::Multiply,
            Instruction::Constant(6),
            Instruction::Multiply,
            Instruction::Subtract,
            Instruction::Constant(7),
            Instruction::Constant(8),
            Instruction::Divide,
            Instruction::Add,
            Instruction::Return,
        ],
        [
            vn(0.0),
            vn(1.0),
            vn(2.0),
            vn(3.0),
            vn(4.0),
            vn(5.0),
            vn(6.0),
            vn(7.0),
            vn(8.0),
        ],
        [1; 18],
    );
}

#[test]
fn compile_groupings() {
    test_compilation(
        "(0)",
        [Instruction::Constant(0), Instruction::Return],
        [vn(0.0)],
        [1; 2],
    );
    test_compilation(
        "((0))",
        [Instruction::Constant(0), Instruction::Return],
        [vn(0.0)],
        [1; 2],
    );
    test_compilation(
        "(((0)))",
        [Instruction::Constant(0), Instruction::Return],
        [vn(0.0)],
        [1; 2],
    );

    test_compilation(
        "(0 + 1 * 2)",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Constant(2),
            Instruction::Multiply,
            Instruction::Add,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0), vn(2.0)],
        [1; 6],
    );

    test_compilation(
        "(0 + 1) + 2",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Add,
            Instruction::Constant(2),
            Instruction::Add,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0), vn(2.0)],
        [1; 6],
    );
    test_compilation(
        "0 + (1 + 2)",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Constant(2),
            Instruction::Add,
            Instruction::Add,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0), vn(2.0)],
        [1; 6],
    );
    test_compilation(
        "(0 + 1) * 2",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Add,
            Instruction::Constant(2),
            Instruction::Multiply,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0), vn(2.0)],
        [1; 6],
    );
    test_compilation(
        "0 + (1 * 2)",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Constant(2),
            Instruction::Multiply,
            Instruction::Add,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0), vn(2.0)],
        [1; 6],
    );

    test_compilation(
        "0 * (1 + (2 - 3))",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Constant(2),
            Instruction::Constant(3),
            Instruction::Subtract,
            Instruction::Add,
            Instruction::Multiply,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0), vn(2.0), vn(3.0)],
        [1; 8],
    );
    test_compilation(
        "0 * ((1 + 2) - 3)",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Constant(2),
            Instruction::Add,
            Instruction::Constant(3),
            Instruction::Subtract,
            Instruction::Multiply,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0), vn(2.0), vn(3.0)],
        [1; 8],
    );

    test_compilation(
        "(true)",
        [Instruction::True, Instruction::Return],
        [],
        [1; 2],
    );
}

#[test]
fn complex_arithmetic() {
    test_compilation(
        "0 - -1 / (2 + (3 - 4) * -((5) / 6 + 7)) / ((-8) + -(9)) * 10 - 11 + -12",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Negate,
            Instruction::Constant(2),
            Instruction::Constant(3),
            Instruction::Constant(4),
            Instruction::Subtract,
            Instruction::Constant(5),
            Instruction::Constant(6),
            Instruction::Divide,
            Instruction::Constant(7),
            Instruction::Add,
            Instruction::Negate,
            Instruction::Multiply,
            Instruction::Add,
            Instruction::Divide,
            Instruction::Constant(8),
            Instruction::Negate,
            Instruction::Constant(9),
            Instruction::Negate,
            Instruction::Add,
            Instruction::Divide,
            Instruction::Constant(10),
            Instruction::Multiply,
            Instruction::Subtract,
            Instruction::Constant(11),
            Instruction::Subtract,
            Instruction::Constant(12),
            Instruction::Negate,
            Instruction::Add,
            Instruction::Return,
        ],
        [
            vn(0.0),
            vn(1.0),
            vn(2.0),
            vn(3.0),
            vn(4.0),
            vn(5.0),
            vn(6.0),
            vn(7.0),
            vn(8.0),
            vn(9.0),
            vn(10.0),
            vn(11.0),
            vn(12.0),
        ],
        [1; 31],
    );
}

#[test]
fn compile_multiple_lines() {
    test_compilation(
        "\n0 + 1",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Add,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0)],
        [2, 2, 2, 2],
    );
    test_compilation(
        "0\n + 1",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Add,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0)],
        [1, 2, 2, 2],
    );
    test_compilation(
        "0 + \n1",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Add,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0)],
        [1, 2, 1, 2],
    );
    test_compilation(
        "0 + 1\n",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Add,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0)],
        [1, 1, 1, 2],
    );

    test_compilation(
        "(0 + 1\n)",
        [
            Instruction::Constant(0),
            Instruction::Constant(1),
            Instruction::Add,
            Instruction::Return,
        ],
        [vn(0.0), vn(1.0)],
        [1, 1, 1, 2],
    );

    test_compilation(
        "\n-0",
        [
            Instruction::Constant(0),
            Instruction::Negate,
            Instruction::Return,
        ],
        [vn(0.0)],
        [2, 2, 2],
    );
    test_compilation(
        "-\n0",
        [
            Instruction::Constant(0),
            Instruction::Negate,
            Instruction::Return,
        ],
        [vn(0.0)],
        [2, 1, 2],
    );
    test_compilation(
        "-0\n",
        [
            Instruction::Constant(0),
            Instruction::Negate,
            Instruction::Return,
        ],
        [vn(0.0)],
        [1, 1, 2],
    );
}

#[test]
fn mismatched_types() {
    test_compilation(
        "-true",
        [Instruction::True, Instruction::Negate, Instruction::Return],
        [],
        [1; 3],
    );
    test_compilation(
        "0 + nil * false",
        [
            Instruction::Constant(0),
            Instruction::Nil,
            Instruction::False,
            Instruction::Multiply,
            Instruction::Add,
            Instruction::Return,
        ],
        [vn(0.0)],
        [1; 6],
    );
}

#[test]
fn too_many_constants() {
    {
        let mut constants = Vec::with_capacity(255);
        for i in 0..=255 {
            constants.push(i.to_string());
        }

        let source_code = constants.join(" + ");
        let result = compile(&source_code);
        assert!(result.is_ok());
    }
    {
        let mut constants = Vec::with_capacity(256);
        for i in 0..=256 {
            constants.push(i.to_string());
        }
        let source_code = constants.join(" + ");

        test_error_output(
            &source_code,
            "[line 1] Error at '256': Each chunk only allows up to 256 constants.\n",
        );
    }
}

#[test]
fn unexpected_characters() {
    test_error_output("&", "[line 1] Error: Unexpected character '&'.\n");
    test_error_output("@", "[line 1] Error: Unexpected character '@'.\n");
    test_error_output("???", "[line 1] Error: Unexpected character '???'.\n");

    test_error_output("&@???", "[line 1] Error: Unexpected character '&'.\n");
    test_error_output("\n&@???", "[line 2] Error: Unexpected character '&'.\n");

    test_error_output("1 + & 2", "[line 1] Error: Unexpected character '&'.\n");
}

#[test]
fn invalid_syntax() {
    test_error_output("0 + + 1", "[line 1] Error at '+': Unexpected token.\n");
    test_error_output("0 * * 1", "[line 1] Error at '*': Unexpected token.\n");
    test_error_output("0 / / 1", "[line 1] Error at '/': Unexpected token.\n");

    test_error_output("* 0", "[line 1] Error at '*': Unexpected token.\n");

    test_error_output("()", "[line 1] Error at ')': Unexpected token.\n");

    test_error_output("0 (1)", "[line 1] Error at '(': Expected end of file.\n");
    test_error_output("(0) 1", "[line 1] Error at '1': Expected end of file.\n");
    test_error_output("(0) (1)", "[line 1] Error at '(': Expected end of file.\n");
}

#[test]
fn unmatched_parentheses() {
    test_error_output("(", "[line 1] Error at end: Unexpected token.\n");
    test_error_output(")", "[line 1] Error at ')': Unexpected token.\n");
    test_error_output(
        "(0 + 1",
        "[line 1] Error at end: Expected closing parenthesis.\n",
    );
    test_error_output("0 + 1)", "[line 1] Error at ')': Expected end of file.\n");
    test_error_output("(0 + 1 +", "[line 1] Error at end: Unexpected token.\n");
    test_error_output("0 + 1 +)", "[line 1] Error at ')': Unexpected token.\n");
    test_error_output(
        "((0)",
        "[line 1] Error at end: Expected closing parenthesis.\n",
    );
    test_error_output("(0))", "[line 1] Error at ')': Expected end of file.\n");
}

#[test]
fn end_of_file() {
    test_error_output("0 +", "[line 1] Error at end: Unexpected token.\n");
    test_error_output("0 + 1 2", "[line 1] Error at '2': Expected end of file.\n");
}

use {
    super::*,
    crate::{compiler::Compiler, lexer::Lexer, value::Value},
    std::panic::catch_unwind,
};

fn vni() -> Value {
    Value::Nil
}

fn vn(n: f64) -> Value {
    Value::Number(n)
}

fn vb(b: bool) -> Value {
    Value::Bool(b)
}

fn test_vm<const N: usize>(source_code: &str, expected_stacks: [&[Value]; N]) {
    let chunk = Compiler::new(Lexer::new(source_code)).compile().unwrap();

    let mut vm = Vm::new(&chunk);
    vm.interpret().unwrap();

    // We don't pass in vecs to this test function just to make the test code
    // slightly less verbose. Convert to vecs here.
    let expected_stacks_vec = expected_stacks
        .into_iter()
        .map(|s| s.to_vec())
        .collect::<Vec<_>>();
    assert_eq!(vm.test_stacks, expected_stacks_vec);
}

fn test_vm_error(source_code: &str, expected_error: &str, expected_line_number: usize) {
    let chunk = Compiler::new(Lexer::new(source_code)).compile().unwrap();

    let mut output = String::new();
    let mut vm = Vm::with_error_printer(&chunk, |format_args| {
        output.push_str(&format_args.to_string());
    });
    vm.interpret().unwrap_err();
    drop(vm);

    let expected_error_string = format!(
        "{}\n[line {}] in script\n",
        expected_error, expected_line_number
    );
    assert_eq!(output, expected_error_string);
}

#[test]
fn simple_chunk_doesnt_produce_an_error() {
    let mut chunk = Chunk::new();

    let constant_index = chunk.add_constant(vn(1.2)).unwrap();
    chunk.add_constant_instruction(constant_index, 123);

    chunk.add_return_instruction(123);

    let mut vm = Vm::new(&chunk);
    assert!(vm.interpret().is_ok());
}

#[test]
fn value_stack_can_push_values() {
    let mut stack = ValueStack::new();
    stack.push(vn(123.0));
    stack.push(vn(69.420));

    assert_eq!(
        stack.data.iter().take(3).collect::<Vec<_>>(),
        vec![&Some(vn(123.0)), &Some(vn(69.420)), &None]
    );
}

#[test]
fn value_stack_can_pop_values() {
    let mut stack = ValueStack::new();
    stack.push(vn(123.0));
    stack.push(vn(69.420));

    assert_eq!(stack.pop(), vn(69.420));
    assert_eq!(stack.pop(), vn(123.0));

    assert_eq!(stack.data.iter().take(1).collect::<Vec<_>>(), vec![&None]);
}

#[test]
fn value_stack_can_peek_values() {
    let mut stack = ValueStack::new();
    stack.push(vn(123.0));
    stack.push(vn(69.420));

    assert_eq!(stack.peek(0), &vn(69.420));
    assert_eq!(stack.peek(1), &vn(123.0));

    assert_eq!(
        *catch_unwind(move || {
            stack.peek(2);
        })
        .unwrap_err()
        .downcast_ref::<&str>()
        .unwrap(),
        "assertion failed: index < self.index"
    );
}

#[test]
fn value_stack_can_push_up_to_256_values() {
    let mut stack = ValueStack::new();
    for i in 0..256 {
        stack.push(vn(i as f64));
    }

    assert_eq!(stack.data.iter().take_while(|v| v.is_some()).count(), 256);
}

#[test]
fn value_stack_will_overflow() {
    let mut stack = ValueStack::new();
    for i in 0..STACK_SIZE {
        stack.push(vn(i as f64));
    }

    assert_eq!(
        *catch_unwind(move || {
            stack.push(vn(256.0));
        })
        .unwrap_err()
        .downcast_ref::<&str>()
        .unwrap(),
        "Stack overflow!"
    );
}

#[test]
fn value_stack_cant_pop_when_empty() {
    let mut stack = ValueStack::new();

    assert_eq!(
        *catch_unwind(move || {
            stack.pop();
        })
        .unwrap_err()
        .downcast_ref::<&str>()
        .unwrap(),
        "Cannot pop from an empty stack."
    );
}

#[test]
fn value_stack_can_reset() {
    let mut stack = ValueStack::new();
    stack.push(vn(123.0));
    stack.push(vn(69.420));

    assert_eq!(
        stack.data.iter().take(3).collect::<Vec<_>>(),
        vec![&Some(vn(123.0)), &Some(vn(69.420)), &None]
    );

    stack.reset();
    assert_eq!(stack.index, 0);
    assert_eq!(
        stack.data.iter().take(3).collect::<Vec<_>>(),
        vec![&None, &None, &None]
    );
}

#[test]
fn literals() {
    test_vm("0", [&[vn(0.0)], &[]]);
    test_vm("0.0", [&[vn(0.0)], &[]]);
    test_vm("1234.5678", [&[vn(1234.5678)], &[]]);
    test_vm("(1234.5678)", [&[vn(1234.5678)], &[]]);
    test_vm("((1234.5678))", [&[vn(1234.5678)], &[]]);

    test_vm("nil", [&[vni()], &[]]);
    test_vm("true", [&[vb(true)], &[]]);
    test_vm("false", [&[vb(false)], &[]]);
    test_vm("(false)", [&[vb(false)], &[]]);
    test_vm("((false))", [&[vb(false)], &[]]);
}

#[test]
fn floating_point_math() {
    test_vm(
        "0.1 * (0.2 * 0.3)",
        [
            &[vn(0.1)],
            &[vn(0.1), vn(0.2)],
            &[vn(0.1), vn(0.2), vn(0.3)],
            &[vn(0.1), vn(0.06)],
            &[vn(0.006)],
            &[],
        ],
    );
    test_vm(
        "(0.1 * 0.2) * 0.3",
        [
            &[vn(0.1)],
            &[vn(0.1), vn(0.2)],
            &[vn(0.020000000000000004)],
            &[vn(0.020000000000000004), vn(0.3)],
            &[vn(0.006000000000000001)],
            &[],
        ],
    );

    // NaN is not equal to itself, so we need to test the exact failure we get
    // here to make sure the print outs of the stacks are identical.
    assert_eq!(
        *catch_unwind(|| {
            test_vm("0 / 0", [&[vn(0.0)], &[vn(0.0), vn(0.0)], &[vn(f64::NAN)], &[]]);
        })
        .unwrap_err()
        .downcast_ref::<String>()
        .unwrap(),
        "assertion failed: `(left == right)`\n  left: `[[Number(0.0)], [Number(0.0), Number(0.0)], [Number(NaN)], []]`,\n right: `[[Number(0.0)], [Number(0.0), Number(0.0)], [Number(NaN)], []]`"
    );
}

#[test]
fn arithmetic() {
    test_vm(
        "1 + 2 + 3",
        [
            &[vn(1.0)],
            &[vn(1.0), vn(2.0)],
            &[vn(3.0)],
            &[vn(3.0), vn(3.0)],
            &[vn(6.0)],
            &[],
        ],
    );
    test_vm(
        "(1 + 2) + 3",
        [
            &[vn(1.0)],
            &[vn(1.0), vn(2.0)],
            &[vn(3.0)],
            &[vn(3.0), vn(3.0)],
            &[vn(6.0)],
            &[],
        ],
    );
    test_vm(
        "1 + (2 + 3)",
        [
            &[vn(1.0)],
            &[vn(1.0), vn(2.0)],
            &[vn(1.0), vn(2.0), vn(3.0)],
            &[vn(1.0), vn(5.0)],
            &[vn(6.0)],
            &[],
        ],
    );
    test_vm("1 - 2", [&[vn(1.0)], &[vn(1.0), vn(2.0)], &[vn(-1.0)], &[]]);
    test_vm("1 / 2", [&[vn(1.0)], &[vn(1.0), vn(2.0)], &[vn(0.5)], &[]]);

    test_vm("-1", [&[vn(1.0)], &[vn(-1.0)], &[]]);
    test_vm("-0", [&[vn(0.0)], &[vn(0.0)], &[]]);

    test_vm(
        "1 + 2 * 3",
        [
            &[vn(1.0)],
            &[vn(1.0), vn(2.0)],
            &[vn(1.0), vn(2.0), vn(3.0)],
            &[vn(1.0), vn(6.0)],
            &[vn(7.0)],
            &[],
        ],
    );

    test_vm(
        "0 - -1 / (2 + (3 - 4) * -((5) / 6 + 7)) / ((-8) + -(9)) * 10 - 11 + -12",
        [
            &[vn(0.0)],
            &[vn(0.0), vn(1.0)],
            &[vn(0.0), vn(-1.0)],
            &[vn(0.0), vn(-1.0), vn(2.0)],
            &[vn(0.0), vn(-1.0), vn(2.0), vn(3.0)],
            &[vn(0.0), vn(-1.0), vn(2.0), vn(3.0), vn(4.0)],
            &[vn(0.0), vn(-1.0), vn(2.0), vn(-1.0)],
            &[vn(0.0), vn(-1.0), vn(2.0), vn(-1.0), vn(5.0)],
            &[vn(0.0), vn(-1.0), vn(2.0), vn(-1.0), vn(5.0), vn(6.0)],
            &[vn(0.0), vn(-1.0), vn(2.0), vn(-1.0), vn(0.8333333333333334)],
            &[
                vn(0.0),
                vn(-1.0),
                vn(2.0),
                vn(-1.0),
                vn(0.8333333333333334),
                vn(7.0),
            ],
            &[vn(0.0), vn(-1.0), vn(2.0), vn(-1.0), vn(7.833333333333333)],
            &[vn(0.0), vn(-1.0), vn(2.0), vn(-1.0), vn(-7.833333333333333)],
            &[vn(0.0), vn(-1.0), vn(2.0), vn(7.833333333333333)],
            &[vn(0.0), vn(-1.0), vn(9.833333333333332)],
            &[vn(0.0), vn(-0.1016949152542373)],
            &[vn(0.0), vn(-0.1016949152542373), vn(8.0)],
            &[vn(0.0), vn(-0.1016949152542373), vn(-8.0)],
            &[vn(0.0), vn(-0.1016949152542373), vn(-8.0), vn(9.0)],
            &[vn(0.0), vn(-0.1016949152542373), vn(-8.0), vn(-9.0)],
            &[vn(0.0), vn(-0.1016949152542373), vn(-17.0)],
            &[vn(0.0), vn(0.005982053838484547)],
            &[vn(0.0), vn(0.005982053838484547), vn(10.0)],
            &[vn(0.0), vn(0.05982053838484547)],
            &[vn(-0.05982053838484547)],
            &[vn(-0.05982053838484547), vn(11.0)],
            &[vn(-11.059820538384846)],
            &[vn(-11.059820538384846), vn(12.0)],
            &[vn(-11.059820538384846), vn(-12.0)],
            &[vn(-23.059820538384848)],
            &[],
        ],
    );
}

#[test]
fn boolean_logic() {
    test_vm("!true", [&[vb(true)], &[vb(false)], &[]]);
    test_vm("!false", [&[vb(false)], &[vb(true)], &[]]);
    test_vm("!!true", [&[vb(true)], &[vb(false)], &[vb(true)], &[]]);
    test_vm("!!false", [&[vb(false)], &[vb(true)], &[vb(false)], &[]]);
    test_vm(
        "!!!true",
        [&[vb(true)], &[vb(false)], &[vb(true)], &[vb(false)], &[]],
    );
    test_vm(
        "!!!false",
        [&[vb(false)], &[vb(true)], &[vb(false)], &[vb(true)], &[]],
    );

    test_vm("!nil", [&[vni()], &[vb(true)], &[]]);
    test_vm("!0", [&[vn(0.0)], &[vb(false)], &[]]);
}

#[test]
fn arithmetic_type_errors() {
    test_vm_error("-true", "Operand 'true' must be a number.", 1);
    test_vm_error("-false", "Operand 'false' must be a number.", 1);
    test_vm_error("-nil", "Operand 'nil' must be a number.", 1);

    test_vm_error("\n-\nnil\n", "Operand 'nil' must be a number.", 2);

    for op in ['+', '-', '*', '/'] {
        test_vm_error("true + false", "Operand 'true' must be a number.", 1);
        test_vm_error(
            &format!("true {} false", op),
            "Operand 'true' must be a number.",
            1,
        );
        test_vm_error(
            &format!("true {} nil", op),
            "Operand 'true' must be a number.",
            1,
        );
        test_vm_error(
            &format!("nil {} true", op),
            "Operand 'nil' must be a number.",
            1,
        );
    }
}

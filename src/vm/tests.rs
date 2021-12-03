use {
    super::*,
    crate::{compiler::Compiler, lexer::Lexer, value::Value},
    std::panic::catch_unwind,
};

/// A helper function to make a numeric Value. This cleans up the test code slightly.
fn vn(n: f64) -> Value {
    Value::new(n)
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
fn literals() {
    test_vm("0", [&[vn(0.0)], &[]]);
    test_vm("0.0", [&[vn(0.0)], &[]]);
    test_vm("1234.5678", [&[vn(1234.5678)], &[]]);
    test_vm("(1234.5678)", [&[vn(1234.5678)], &[]]);
    test_vm("((1234.5678))", [&[vn(1234.5678)], &[]]);
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
        "assertion failed: `(left == right)`\n  left: `[[Value(0.0)], [Value(0.0), Value(0.0)], [Value(NaN)], []]`,\n right: `[[Value(0.0)], [Value(0.0), Value(0.0)], [Value(NaN)], []]`"
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

    test_vm("-1", [&[vn(1.0)], &[vn(-1.0)], &[]]);

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

use {super::*, crate::value::Value, std::panic::catch_unwind};

#[test]
fn simple_chunk_doesnt_produce_an_error() {
    let mut chunk = Chunk::new();

    let constant_index = chunk.add_constant(Value::new(1.2)).unwrap();
    chunk.add_constant_instruction(constant_index, 123);

    chunk.add_return_instruction(123);

    let mut vm = Vm::new(&chunk);
    assert!(vm.interpret().is_ok());
}

#[test]
fn value_stack_can_push_values() {
    let mut stack = ValueStack::new();
    stack.push(Value::new(123.0));
    stack.push(Value::new(69.420));

    assert_eq!(
        stack.data.iter().take(3).collect::<Vec<_>>(),
        vec![&Some(Value::new(123.0)), &Some(Value::new(69.420)), &None]
    );
}

#[test]
fn value_stack_can_pop_values() {
    let mut stack = ValueStack::new();
    stack.push(Value::new(123.0));
    stack.push(Value::new(69.420));

    assert_eq!(stack.pop(), Value::new(69.420));
    assert_eq!(stack.pop(), Value::new(123.0));

    assert_eq!(stack.data.iter().take(1).collect::<Vec<_>>(), vec![&None]);
}

#[test]
fn value_stack_can_push_up_to_256_values() {
    let mut stack = ValueStack::new();
    for i in 0..256 {
        stack.push(Value::new(i as f64));
    }

    assert_eq!(stack.data.iter().take_while(|v| v.is_some()).count(), 256);
}

#[test]
fn value_stack_will_overflow() {
    let mut stack = ValueStack::new();
    for i in 0..STACK_SIZE {
        stack.push(Value::new(i as f64));
    }

    assert_eq!(
        *catch_unwind(move || {
            stack.push(Value::new(256.0));
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
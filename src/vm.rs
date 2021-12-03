//! The virtual machine which runs the compiled bytecode.

#[cfg(feature = "debug_trace_execution")]
use crate::debug::disassemble_instruction;
use crate::{
    chunk::{Chunk, ChunkCursor, Instruction},
    value::{value_to_string, Value},
};

const STACK_SIZE: usize = 256;

/// A statically-sized stack that contains values during execution.
struct ValueStack {
    data: [Option<Value>; STACK_SIZE],
    index: usize,
}

impl ValueStack {
    fn new() -> Self {
        const EMPTY_VALUE: Option<Value> = None;
        Self {
            data: [EMPTY_VALUE; STACK_SIZE],
            index: 0,
        }
    }

    fn push(&mut self, value: Value) {
        if self.index == STACK_SIZE {
            panic!("Stack overflow!");
        }

        self.data[self.index] = Some(value);
        self.index += 1;
    }

    fn pop(&mut self) -> Value {
        if self.index == 0 {
            panic!("Cannot pop from an empty stack.");
        }

        self.index -= 1;
        self.data[self.index].take().unwrap()
    }

    #[cfg(feature = "debug_trace_execution")]
    fn debug_print(&self) {
        print!("          ");

        if self.index == 0 {
            println!("Stack is empty");
            return;
        }

        for i in 0..self.index {
            print!("[ {} ]", value_to_string(self.data[i].as_ref().unwrap()));
        }
        println!();
    }
}

/// The actual virtual machine that executes Lox bytecode.
pub struct Vm<'a> {
    chunk: &'a Chunk,
    cursor: ChunkCursor<'a>,
    stack: ValueStack,
}

impl<'a> Vm<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        Self {
            chunk,
            cursor: chunk.cursor(),
            stack: ValueStack::new(),
        }
    }

    pub fn interpret(&mut self) -> Result<(), ()> {
        loop {
            #[cfg(feature = "debug_trace_execution")]
            let offset = self.cursor.offset();

            let instruction = self.cursor.read_instruction().unwrap();

            // Optionally trace each instruction as we execute, showing the
            // contents of the stack before each one.
            #[cfg(feature = "debug_trace_execution")]
            {
                self.stack.debug_print();
                disassemble_instruction(self.chunk, offset, &instruction);
            }

            match instruction {
                Instruction::Constant(index) => {
                    let constant = self.chunk.get_constant(index);
                    self.stack.push(constant.clone());
                }
                Instruction::Add => {
                    self.execute_binary_operation(Value::add);
                }
                Instruction::Subtract => {
                    self.execute_binary_operation(Value::subtract);
                }
                Instruction::Multiply => {
                    self.execute_binary_operation(Value::multiply);
                }
                Instruction::Divide => {
                    self.execute_binary_operation(Value::divide);
                }
                Instruction::Negate => {
                    let negated_value = self.stack.pop().negate();
                    self.stack.push(negated_value)
                }
                Instruction::Return => {
                    println!("{}", value_to_string(&self.stack.pop()));
                    return Ok(());
                }
            }
        }
    }

    fn execute_binary_operation(&mut self, op: impl FnOnce(&Value, Value) -> Value) {
        let b = self.stack.pop();
        let a = self.stack.pop();
        self.stack.push(op(&a, b));
    }
}

#[cfg(test)]
mod tests {
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
}

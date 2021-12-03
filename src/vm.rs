//! The virtual machine which runs the compiled bytecode.

#[cfg(test)]
mod tests;

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

    #[cfg(test)]
    fn to_vec(&self) -> Vec<Value> {
        self.data
            .iter()
            .take(self.index)
            .map(|o| o.clone().unwrap())
            .collect()
    }
}

/// The actual virtual machine that executes Lox bytecode.
pub struct Vm<'a> {
    chunk: &'a Chunk,
    cursor: ChunkCursor<'a>,
    stack: ValueStack,

    #[cfg(test)]
    test_stacks: Vec<Vec<Value>>,
}

impl<'a> Vm<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        Self {
            chunk,
            cursor: chunk.cursor(),
            stack: ValueStack::new(),

            #[cfg(test)]
            test_stacks: Vec::new(),
        }
    }

    pub fn interpret(&mut self) -> Result<(), ()> {
        let result = loop {
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
                    break Ok(());
                }
            }

            #[cfg(test)]
            self.test_stacks.push(self.stack.to_vec());
        };

        #[cfg(test)]
        self.test_stacks.push(self.stack.to_vec());

        result
    }

    fn execute_binary_operation(&mut self, op: impl FnOnce(&Value, Value) -> Value) {
        let b = self.stack.pop();
        let a = self.stack.pop();
        self.stack.push(op(&a, b));
    }
}

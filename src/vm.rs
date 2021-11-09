#[cfg(feature = "debug_trace_execution")]
use crate::debug::disassemble_instruction;
use crate::{
    chunk::{Chunk, ChunkCursor, Instruction},
    value::{value_to_string, Value},
};

const STACK_SIZE: usize = 256;

/// An error returned from the interpreter, either a compile error or a runtime
/// error.
#[derive(Debug)]
pub enum InterpretError {
    // CompileError,
// RuntimeError,
}

/// A statically-sized stack that contains values during execution.
struct ValueStack {
    data: [Option<Value>; STACK_SIZE],
    index: usize,
}

impl ValueStack {
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
            print!("[ {} ]", value_to_string(&self.data[i].as_ref().unwrap()));
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
        const EMPTY_VALUE: Option<Value> = None;
        Self {
            chunk,
            cursor: chunk.cursor(),
            stack: ValueStack {
                data: [EMPTY_VALUE; STACK_SIZE],
                index: 0,
            },
        }
    }

    pub fn interpret(&mut self) -> Result<(), InterpretError> {
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
                Instruction::Return => {
                    println!("{}", value_to_string(&self.stack.pop()));
                    return Ok(());
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use {super::*, crate::value::Value};

    #[test]
    fn simple_chunk_doesnt_produce_an_error() {
        let mut chunk = Chunk::new();

        let constant_index = chunk.add_constant(Value::new(1.2));
        chunk.add_constant_instruction(constant_index, 123);

        chunk.add_return_instruction(123);

        let mut vm = Vm::new(&chunk);
        assert!(vm.interpret().is_ok());
    }
}

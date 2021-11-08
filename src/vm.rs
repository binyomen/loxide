use crate::{
    chunk::{Chunk, ChunkCursor, Instruction},
    value::value_to_string,
};

/// An error returned from the interpreter, either a compile error or a runtime
/// error.
#[derive(Debug)]
pub enum InterpretError {
    // CompileError,
// RuntimeError,
}

/// The actual virtual machine that executes Lox bytecode.
pub struct Vm<'a> {
    chunk: &'a Chunk,
    cursor: ChunkCursor<'a>,
}

impl<'a> Vm<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        Self {
            chunk,
            cursor: chunk.cursor(),
        }
    }

    pub fn interpret(&mut self) -> Result<(), InterpretError> {
        loop {
            match self.cursor.read_instruction().unwrap() {
                Instruction::Constant(index) => {
                    let constant = self.chunk.get_constant(index);
                    println!("{}", value_to_string(constant));
                }
                Instruction::Return => {
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

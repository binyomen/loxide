//! Utilities for dealing with chunks of bytecode.

#[cfg(debug_assertions)]
use strum::EnumCount;
use {
    crate::value::Value,
    std::mem::transmute,
    strum_macros::{EnumCount, EnumIter},
};

/// A single byte used in the interpreter's bytecode. A newtype for `u8`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(transparent)]
struct CodeByte(u8);

impl CodeByte {
    fn new(byte: u8) -> Self {
        Self(byte)
    }
}

/// A byte representing an instruction in the interpreter's bytecode.
#[derive(Clone, Copy, Debug, EnumCount, EnumIter, Eq, PartialEq)]
#[repr(u8)]
enum OpCode {
    Constant,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Return,
}

impl OpCode {
    /// Turns the given [`CodeByte`] into an [`OpCode`] effectively as a noop.
    /// In debug mode this function makes sure the conversion is valid. In
    /// release mode, passing in a byte value that doesn't correspond to an op
    /// code results in undefined behavior.
    fn from_byte(byte: CodeByte) -> Self {
        #[cfg(debug_assertions)]
        if byte.0 >= OpCode::COUNT as u8 {
            panic!("Value {} is not a valid op code.", byte.0);
        }

        unsafe { transmute::<CodeByte, Self>(byte) }
    }

    fn as_byte(&self) -> CodeByte {
        CodeByte::new(*self as u8)
    }
}

/// A bytecode instruction, varying by opcode and including arguments.
#[derive(PartialEq, Eq, Debug)]
pub enum Instruction {
    Constant(u8),
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Return,
}

/// A chunk of bytecode, representing a top-level program or a function.
pub struct Chunk {
    code: Vec<CodeByte>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn add_constant_instruction(&mut self, constant_index: u8, line_number: usize) {
        self.add_byte(OpCode::Constant.as_byte(), line_number);
        self.add_byte(CodeByte::new(constant_index), line_number);
    }

    pub fn add_return_instruction(&mut self, line_number: usize) {
        self.add_byte(OpCode::Return.as_byte(), line_number);
    }

    pub fn add_add_instruction(&mut self, line_number: usize) {
        self.add_byte(OpCode::Add.as_byte(), line_number);
    }

    pub fn add_subtract_instruction(&mut self, line_number: usize) {
        self.add_byte(OpCode::Subtract.as_byte(), line_number);
    }

    pub fn add_multiply_instruction(&mut self, line_number: usize) {
        self.add_byte(OpCode::Multiply.as_byte(), line_number);
    }

    pub fn add_divide_instruction(&mut self, line_number: usize) {
        self.add_byte(OpCode::Divide.as_byte(), line_number);
    }

    pub fn add_negate_instruction(&mut self, line_number: usize) {
        self.add_byte(OpCode::Negate.as_byte(), line_number);
    }

    /// Add a constant to the chunk. This function returns the index of the
    /// constant in the chunk's constant list so that other instructions can
    /// refer to it.
    ///
    /// Chunks can currently only store up to 256 constants. None is returned
    /// if we've increased beyond the max.
    pub fn add_constant(&mut self, constant: Value) -> Option<u8> {
        if self.constants.len() > Into::<usize>::into(u8::MAX) {
            None
        } else {
            self.constants.push(constant);
            Some((self.constants.len() - 1) as u8)
        }
    }

    pub fn get_constant(&self, index: u8) -> &Value {
        &self.constants[Into::<usize>::into(index)]
    }

    /// This function shouldn't be needed in actual code, but is handy in tests.
    #[cfg(test)]
    pub fn constants(&self) -> Vec<Value> {
        self.constants.clone()
    }

    pub fn cursor(&self) -> ChunkCursor {
        ChunkCursor {
            chunk: self,
            offset: 0,
        }
    }

    pub fn line_at_offset(&self, offset: usize) -> usize {
        self.lines[offset]
    }

    fn add_byte(&mut self, byte: CodeByte, line_number: usize) {
        self.code.push(byte);
        self.lines.push(line_number);
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}

/// A cursor to give random access into the bytecode [`Chunk`].
pub struct ChunkCursor<'a> {
    chunk: &'a Chunk,
    offset: usize,
}

impl<'a> ChunkCursor<'a> {
    pub fn read_instruction(&mut self) -> Option<Instruction> {
        if self.at_end() {
            None
        } else {
            let op_code = OpCode::from_byte(self.chunk.code[self.offset]);
            self.offset += 1;
            match op_code {
                OpCode::Constant => {
                    let index = if self.at_end() {
                        panic!("No byte following Constant op code.");
                    } else {
                        self.chunk.code[self.offset].0
                    };
                    self.offset += 1;
                    Some(Instruction::Constant(index))
                }
                OpCode::Add => Some(Instruction::Add),
                OpCode::Subtract => Some(Instruction::Subtract),
                OpCode::Multiply => Some(Instruction::Multiply),
                OpCode::Divide => Some(Instruction::Divide),
                OpCode::Negate => Some(Instruction::Negate),
                OpCode::Return => Some(Instruction::Return),
            }
        }
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn previous_line(&self) -> usize {
        debug_assert!(self.offset > 0);
        self.chunk.line_at_offset(self.offset - 1)
    }

    fn at_end(&self) -> bool {
        self.offset >= self.chunk.code.len()
    }
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        std::{mem::size_of, panic::catch_unwind},
        strum::IntoEnumIterator,
    };

    fn vn(n: f64) -> Value {
        Value::Number(n)
    }

    #[test]
    fn code_byte_has_correct_representation() {
        assert_eq!(size_of::<CodeByte>(), size_of::<u8>());

        assert_eq!(CodeByte::new(0).0, 0);
        assert_eq!(CodeByte::new(1).0, 1);
        assert_eq!(CodeByte::new(127).0, 127);
        assert_eq!(CodeByte::new(113).0, 113);
        assert_eq!(CodeByte::new(255).0, 255);
    }

    #[test]
    fn op_code_has_correct_representation() {
        assert_eq!(size_of::<OpCode>(), size_of::<u8>());

        for (i, val) in OpCode::iter().enumerate() {
            assert_eq!(val.as_byte(), CodeByte::new(i.try_into().unwrap()));
        }
    }

    #[test]
    fn op_code_can_be_constructed_from_byte() {
        assert_eq!(OpCode::from_byte(CodeByte::new(0)), OpCode::Constant);
        assert_eq!(OpCode::from_byte(CodeByte::new(6)), OpCode::Return);
        assert_eq!(
            *catch_unwind(|| { OpCode::from_byte(CodeByte::new(7)) })
                .unwrap_err()
                .downcast_ref::<String>()
                .unwrap(),
            "Value 7 is not a valid op code."
        );
        assert_eq!(
            *catch_unwind(|| { OpCode::from_byte(CodeByte::new(8)) })
                .unwrap_err()
                .downcast_ref::<String>()
                .unwrap(),
            "Value 8 is not a valid op code."
        );
        assert_eq!(
            *catch_unwind(|| { OpCode::from_byte(CodeByte::new(100)) })
                .unwrap_err()
                .downcast_ref::<String>()
                .unwrap(),
            "Value 100 is not a valid op code."
        );
    }

    #[test]
    fn chunk_can_add_instructions() {
        let mut chunk = Chunk::new();
        chunk.add_constant_instruction(23, 150);
        chunk.add_negate_instruction(77);
        chunk.add_return_instruction(99);
        assert_eq!(
            chunk.code,
            vec![
                CodeByte::new(0),
                CodeByte::new(23),
                CodeByte::new(5),
                CodeByte::new(6)
            ]
        );
        assert_eq!(chunk.lines, vec![150, 150, 77, 99]);
    }

    #[test]
    fn chunk_can_add_constant() {
        let mut chunk = Chunk::new();
        assert_eq!(chunk.add_constant(vn(1.2)), Some(0));
        assert_eq!(chunk.add_constant(vn(500.3928)), Some(1));
        assert_eq!(chunk.constants.len(), 2);

        assert_eq!(chunk.get_constant(0), &vn(1.2));
        assert_eq!(chunk.get_constant(1), &vn(500.3928));
    }

    #[test]
    fn chunk_can_add_up_to_256_constants() {
        let mut chunk = Chunk::new();
        for i in 0..256 {
            assert_eq!(chunk.add_constant(vn(i as f64)), Some(i as u8));
        }
    }

    #[test]
    fn chunk_cannot_add_more_than_256_constants() {
        let mut chunk = Chunk::new();
        for i in 0..256 {
            assert_eq!(chunk.add_constant(vn(i as f64)), Some(i as u8));
        }

        assert_eq!(chunk.add_constant(vn(256.0)), None);
    }

    #[test]
    fn cursor_can_read_instructions() {
        {
            let chunk = {
                let mut chunk = Chunk::new();
                chunk.add_return_instruction(100);
                chunk
            };
            let mut cursor = chunk.cursor();
            assert_eq!(cursor.read_instruction(), Some(Instruction::Return));
            assert_eq!(cursor.read_instruction(), None);
        }
        {
            let chunk = {
                let mut chunk = Chunk::new();
                chunk.add_return_instruction(100);
                chunk.add_constant_instruction(87, 123);
                chunk
            };
            let mut cursor = chunk.cursor();
            assert_eq!(cursor.read_instruction(), Some(Instruction::Return));
            assert_eq!(cursor.read_instruction(), Some(Instruction::Constant(87)));
            assert_eq!(cursor.read_instruction(), None);
        }
    }

    #[test]
    fn cursor_fails_to_read_constant_instruction_at_end_of_chunk() {
        let chunk = {
            let mut chunk = Chunk::new();
            chunk.add_return_instruction(100);
            chunk.add_byte(OpCode::Constant.as_byte(), 123);
            chunk
        };
        let mut cursor = chunk.cursor();
        assert_eq!(cursor.read_instruction(), Some(Instruction::Return));
        assert_eq!(
            *catch_unwind(move || { cursor.read_instruction() })
                .unwrap_err()
                .downcast_ref::<&str>()
                .unwrap(),
            "No byte following Constant op code."
        );
    }
}

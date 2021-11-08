//! Utilities for dealing with chunks of bytecode.

use {
    crate::value::Value,
    std::mem::transmute,
    strum::EnumCount,
    strum_macros::{EnumCount, EnumIter},
};

/// A single byte used in the interpreter's bytecode. A newtype for `u8`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(transparent)]
pub struct CodeByte(u8);

impl CodeByte {
    pub fn new(byte: u8) -> Self {
        Self(byte)
    }
}

/// A byte representing an instruction in the interpreter's bytecode.
#[derive(Clone, Copy, Debug, EnumCount, EnumIter, Eq, PartialEq)]
#[repr(u8)]
pub enum OpCode {
    Constant,
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

    pub fn as_byte(&self) -> CodeByte {
        CodeByte::new(*self as u8)
    }
}

/// A bytecode instruction, varying by opcode and including arguments.
#[derive(PartialEq, Eq, Debug)]
pub enum Instruction {
    Constant(u8),
    Return,
}

/// A chunk of bytecode, representing a top-level program or a function.
pub struct Chunk {
    code: Vec<CodeByte>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn add_byte(&mut self, byte: CodeByte) {
        self.code.push(byte);
    }

    /// Add a constant to the chunk. This function returns the index of the
    /// constant in the chunk's constant list so that other instructions can
    /// refer to it.
    ///
    /// Chunks can currently only store up to 256 constants.
    pub fn add_constant(&mut self, constant: Value) -> u8 {
        self.constants.push(constant);
        debug_assert!(self.constants.len() <= u8::MAX.into());
        (self.constants.len() - 1) as u8
    }

    pub fn get_constant(&self, index: u8) -> &Value {
        &self.constants[Into::<usize>::into(index)]
    }

    pub fn cursor(&self) -> ChunkCursor {
        ChunkCursor {
            chunk: self,
            offset: 0,
        }
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
                OpCode::Return => Some(Instruction::Return),
            }
        }
    }

    pub fn offset(&self) -> usize {
        self.offset
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
        assert_eq!(OpCode::from_byte(CodeByte::new(1)), OpCode::Return);
        assert_eq!(
            *catch_unwind(|| { OpCode::from_byte(CodeByte::new(2)) })
                .unwrap_err()
                .downcast_ref::<String>()
                .unwrap(),
            "Value 2 is not a valid op code."
        );
        assert_eq!(
            *catch_unwind(|| { OpCode::from_byte(CodeByte::new(3)) })
                .unwrap_err()
                .downcast_ref::<String>()
                .unwrap(),
            "Value 3 is not a valid op code."
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
    fn chunk_can_add_byte() {
        let mut chunk = Chunk::new();
        chunk.add_byte(CodeByte::new(123));
        chunk.add_byte(CodeByte::new(18));
        assert_eq!(chunk.code.len(), 2);
    }

    #[test]
    fn chunk_can_add_constant() {
        let mut chunk = Chunk::new();
        assert_eq!(chunk.add_constant(Value::new(1.2)), 0);
        assert_eq!(chunk.add_constant(Value::new(500.3928)), 1);
        assert_eq!(chunk.constants.len(), 2);

        assert_eq!(chunk.get_constant(0), &Value::new(1.2));
        assert_eq!(chunk.get_constant(1), &Value::new(500.3928));
    }

    #[test]
    fn chunk_can_read_instructions() {
        {
            let chunk = {
                let mut chunk = Chunk::new();
                chunk.add_byte(CodeByte::new(1));
                chunk
            };
            let mut cursor = chunk.cursor();
            assert_eq!(cursor.read_instruction(), Some(Instruction::Return));
            assert_eq!(cursor.read_instruction(), None);
        }
        {
            let chunk = {
                let mut chunk = Chunk::new();
                chunk.add_byte(CodeByte::new(1));
                chunk.add_byte(CodeByte::new(0));
                chunk.add_byte(CodeByte::new(87));
                chunk
            };
            let mut cursor = chunk.cursor();
            assert_eq!(cursor.read_instruction(), Some(Instruction::Return));
            assert_eq!(cursor.read_instruction(), Some(Instruction::Constant(87)));
            assert_eq!(cursor.read_instruction(), None);
        }
    }
}

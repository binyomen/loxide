//! Utilities for dealing with chunks of bytecode.

/// A single byte used in the interpreter's bytecode. A newtype for `u8`.
#[derive(PartialEq, Eq, Debug)]
#[repr(transparent)]
pub struct CodeByte(u8);

impl CodeByte {
    fn new(byte: u8) -> Self {
        Self(byte)
    }
}

/// A byte representing an instruction in the interpreter's bytecode.
#[derive(Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    Return = 0x0,
}

impl OpCode {
    pub fn as_byte(&self) -> CodeByte {
        CodeByte::new(*self as u8)
    }
}

/// A chunk of bytecode, representing a top-level program or a function.
pub struct Chunk {
    code: Vec<CodeByte>,
}

impl Chunk {
    pub fn new() -> Self {
        Self { code: Vec::new() }
    }

    pub fn add_byte(&mut self, byte: CodeByte) {
        self.code.push(byte);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;

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

        assert_eq!(OpCode::Return.as_byte(), CodeByte::new(0));
    }

    #[test]
    fn chunk_can_add_byte() {
        let mut chunk = Chunk::new();
        chunk.add_byte(CodeByte::new(123));
    }
}

mod chunk;
mod debug;
mod value;

use {
    chunk::{Chunk, CodeByte, OpCode},
    debug::disassemble_chunk,
    value::Value,
};

pub fn test() {
    let mut chunk = Chunk::new();
    chunk.add_byte(OpCode::Return.as_byte());

    let constant_index = chunk.add_constant(Value::new(1.2));
    chunk.add_byte(OpCode::Constant.as_byte());
    chunk.add_byte(CodeByte::new(constant_index));

    disassemble_chunk(&chunk, "test chunk");
}

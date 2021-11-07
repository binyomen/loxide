mod chunk;
mod debug;

use {
    chunk::{Chunk, OpCode},
    debug::disassemble_chunk,
};

pub fn test() {
    let mut chunk = Chunk::new();
    chunk.add_byte(OpCode::Return.as_byte());

    disassemble_chunk(&chunk, "test chunk");
}

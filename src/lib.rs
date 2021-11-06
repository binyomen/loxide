mod chunk;

use chunk::{Chunk, OpCode};

pub fn test() {
    let mut chunk = Chunk::new();
    chunk.add_byte(OpCode::Return.as_byte());
}

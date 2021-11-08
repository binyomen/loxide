mod chunk;
mod debug;
mod value;

use {chunk::Chunk, debug::disassemble_chunk, value::Value};

pub fn test() {
    let mut chunk = Chunk::new();
    chunk.add_return_instruction();

    let constant_index = chunk.add_constant(Value::new(1.2));
    chunk.add_constant_instruction(constant_index);

    disassemble_chunk(&chunk, "test chunk");
}

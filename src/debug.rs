use {
    crate::{
        chunk::{Chunk, ChunkCursor, Instruction},
        value::{value_to_string, Value},
    },
    std::fmt::Display,
};

/// Print out a disassembly of the given chunk.
pub fn disassemble_chunk(chunk: &Chunk, name: impl Display) {
    println!("== {} ==", name);

    let mut cursor = chunk.cursor();
    while let Some(instruction) = cursor.read_instruction() {
        disassemble_instruction(chunk, &cursor, instruction);
    }
}

fn disassemble_instruction(chunk: &Chunk, cursor: &ChunkCursor, instruction: Instruction) {
    print!("{:04} ", cursor.offset());

    match instruction {
        Instruction::Constant(index) => {
            disassemble_constant_instruction(chunk.get_constant(index), index)
        }
        Instruction::Return => disassemble_simple_instruction("Return"),
    }
}

fn disassemble_simple_instruction(name: impl Display) {
    println!("{}", name);
}

fn disassemble_constant_instruction(constant: &Value, constant_index: u8) {
    println!(
        "{:-16} {:4} '{}'",
        "Constant",
        constant_index,
        value_to_string(constant)
    );
}

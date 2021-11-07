use {
    crate::chunk::{Chunk, Instruction},
    std::fmt::Display,
};

pub fn disassemble_chunk(chunk: &Chunk, name: impl AsRef<str> + Display) {
    println!("== {} ==", name);

    let mut cursor = chunk.cursor();
    let mut offset = cursor.offset();
    while let Some(instruction) = cursor.read_instruction() {
        disassemble_instruction(instruction, offset);
        offset = cursor.offset();
    }
}

fn disassemble_instruction(instruction: Instruction, offset: usize) {
    print!("{:04} ", offset);

    let name = match instruction {
        Instruction::Return => "Return",
    };

    println!("{}", name);
}

//! Utilities to help debug the interpreter.

use {
    crate::{
        chunk::{Chunk, Instruction},
        value::{value_to_string, Value},
    },
    std::fmt::Display,
};

/// Print out a disassembly of the given chunk.
pub fn disassemble_chunk(chunk: &Chunk, name: impl Display) {
    println!("== {} ==", name);

    let mut cursor = chunk.cursor();
    let mut offset = cursor.offset();
    while let Some(instruction) = cursor.read_instruction() {
        disassemble_instruction(chunk, offset, &instruction);
        offset = cursor.offset();
    }
}

/// Print out a disassembly of the given instruction from the given chunk.
pub fn disassemble_instruction(chunk: &Chunk, offset: usize, instruction: &Instruction) {
    print!("{:04} ", offset);

    // If we're at the same line number as the previous instruction, just print a pipe.
    if offset > 0 && chunk.line_at_offset(offset) == chunk.line_at_offset(offset - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.line_at_offset(offset));
    }

    match instruction {
        Instruction::Constant(index) => {
            disassemble_constant_instruction(chunk.get_constant(*index), *index)
        }
        Instruction::Negate => disassemble_simple_instruction("Negate"),
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

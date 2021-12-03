mod chunk;
mod compiler;
mod debug;
mod lexer;
mod value;
mod vm;

use {chunk::Chunk, compiler::Compiler, debug::disassemble_chunk, lexer::Lexer, vm::Vm};

/// A stand-in for a loxide error type.
pub struct LoxideError;

impl From<()> for LoxideError {
    fn from(_: ()) -> Self {
        LoxideError
    }
}

/// Compile the given source code into a bytecode representation. Errors
/// indicate compilation errors.
pub fn compile(source_code: &str) -> Result<Chunk, LoxideError> {
    let lexer = Lexer::new(source_code);

    let chunk = Compiler::new(lexer).compile()?;

    // If we want to look at the compiled bytecode without executing it, we do
    // that here.
    if cfg!(feature = "debug_dump_disassembly") {
        disassemble_chunk(&chunk, "[top level]");
        return Err(LoxideError);
    }

    Ok(chunk)
}

/// Execute the given bytecode. Errors indicate runtime errors.
pub fn execute(chunk: Chunk) -> Result<(), LoxideError> {
    let mut vm = Vm::new(&chunk);
    vm.interpret()?;

    Ok(())
}

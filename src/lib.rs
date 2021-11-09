mod chunk;
mod debug;
mod value;
mod vm;

use {chunk::Chunk, debug::disassemble_chunk, value::Value, vm::Vm};

pub fn test() {
    let mut chunk = Chunk::new();

    let constant_index = chunk.add_constant(Value::new(1.2));
    chunk.add_constant_instruction(constant_index, 123);

    chunk.add_negate_instruction(123);
    chunk.add_return_instruction(123);

    // If we want to look at the compiled bytecode without executing it, we do
    // that here.
    if cfg!(feature = "debug_dump_disassembly") {
        disassemble_chunk(&chunk, "test chunk");
        return;
    }

    let mut vm = Vm::new(&chunk);
    vm.interpret().unwrap();
}

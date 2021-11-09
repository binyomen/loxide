var searchIndex = JSON.parse('{\
"loxide":{"doc":"","t":[0,0,5,0,0,12,13,13,3,3,3,13,13,13,13,4,13,13,13,13,4,3,13,13,13,13,11,11,11,11,11,11,11,11,11,11,11,12,11,11,11,11,11,11,11,11,11,11,11,11,12,11,11,11,11,11,11,12,12,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,12,11,11,11,11,11,11,11,11,11,11,12,12,11,11,11,11,11,11,11,11,12,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,12,5,5,5,5,12,3,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,5,4,17,3,3,11,11,11,11,11,11,12,12,12,11,11,11,11,11,12,11,11,11,11,11,11,11,11,12,11,11,11,11,11,11,11,11,11],"n":["chunk","debug","test","value","vm","0","Add","Add","Chunk","ChunkCursor","CodeByte","Constant","Constant","Divide","Divide","Instruction","Multiply","Multiply","Negate","Negate","OpCode","OpCodeIter","Return","Return","Subtract","Subtract","add_add_instruction","add_byte","add_constant","add_constant_instruction","add_divide_instruction","add_multiply_instruction","add_negate_instruction","add_return_instruction","add_subtract_instruction","as_byte","at_end","back_idx","borrow","borrow","borrow","borrow","borrow","borrow","borrow_mut","borrow_mut","borrow_mut","borrow_mut","borrow_mut","borrow_mut","chunk","clone","clone","clone","clone_into","clone_into","clone_into","code","constants","cursor","eq","eq","eq","fmt","fmt","fmt","from","from","from","from","from","from","from_byte","get","get_constant","idx","into","into","into","into","into","into","into_iter","iter","len","line_at_offset","lines","marker","ne","ne","new","new","next","next_back","nth","offset","offset","read_instruction","size_hint","to_owned","to_owned","to_owned","try_from","try_from","try_from","try_from","try_from","try_from","try_into","try_into","try_into","try_into","try_into","try_into","type_id","type_id","type_id","type_id","type_id","type_id","0","disassemble_chunk","disassemble_constant_instruction","disassemble_instruction","disassemble_simple_instruction","0","Value","add","borrow","borrow_mut","clone","clone_into","divide","eq","fmt","from","into","multiply","ne","negate","new","subtract","to_owned","try_from","try_into","type_id","value_to_string","InterpretError","STACK_SIZE","ValueStack","Vm","borrow","borrow","borrow","borrow_mut","borrow_mut","borrow_mut","chunk","cursor","data","execute_binary_operation","fmt","from","from","from","index","interpret","into","into","into","new","new","pop","push","stack","try_from","try_from","try_from","try_into","try_into","try_into","type_id","type_id","type_id"],"q":["loxide","","","","","loxide::chunk","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","loxide::chunk::Instruction","loxide::debug","","","","loxide::value","","","","","","","","","","","","","","","","","","","","","","loxide::vm","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""],"d":["Utilities for dealing with chunks of bytecode.","Utilities to help debug the interpreter.","","Utilities for dealing with Lox values.","","","","","A chunk of bytecode, representing a top-level program or a …","A cursor to give random access into the bytecode <code>Chunk</code>.","A single byte used in the interpreter’s bytecode. A …","","","","","A bytecode instruction, varying by opcode and including …","","","","","A byte representing an instruction in the interpreter’s …","","","","","","","","Add a constant to the chunk. This function returns the …","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","Turns the given <code>CodeByte</code> into an <code>OpCode</code> effectively as a …","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","Print out a disassembly of the given chunk.","","Print out a disassembly of the given instruction from the …","","","A Lox value as represented in the interpreter. For …","","","","","","","","","","","","","","","","","","","","","An error returned from the interpreter, either a compile …","","A statically-sized stack that contains values during …","The actual virtual machine that executes Lox bytecode.","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""],"i":[0,0,0,0,0,1,2,3,0,0,0,2,3,2,3,0,2,3,2,3,0,0,2,3,2,3,4,4,4,4,4,4,4,4,4,2,5,6,4,5,1,2,6,3,4,5,1,2,6,3,5,1,2,6,1,2,6,4,4,4,1,2,3,1,2,3,4,5,1,2,6,3,2,6,4,6,4,5,1,2,6,3,6,2,6,4,4,6,1,3,4,1,6,6,6,5,5,5,6,1,2,6,4,5,1,2,6,3,4,5,1,2,6,3,4,5,1,2,6,3,7,0,0,0,0,8,0,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,0,0,0,0,0,9,10,11,9,10,11,10,10,9,10,11,9,10,11,9,10,9,10,11,9,10,9,9,10,9,10,11,9,10,11,9,10,11],"f":[null,null,[[]],null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,[[["usize",15]]],[[["usize",15],["codebyte",3]]],[[["value",3]],["u8",15]],[[["usize",15],["u8",15]]],[[["usize",15]]],[[["usize",15]]],[[["usize",15]]],[[["usize",15]]],[[["usize",15]]],[[],["codebyte",3]],[[],["bool",15]],null,[[]],[[]],[[]],[[]],[[]],[[]],[[]],[[]],[[]],[[]],[[]],[[]],null,[[],["codebyte",3]],[[],["opcode",4]],[[],["opcodeiter",3]],[[]],[[]],[[]],null,null,[[],["chunkcursor",3]],[[["codebyte",3]],["bool",15]],[[["opcode",4]],["bool",15]],[[["instruction",4]],["bool",15]],[[["formatter",3]],["result",6]],[[["formatter",3]],["result",6]],[[["formatter",3]],["result",6]],[[]],[[]],[[]],[[]],[[]],[[]],[[["codebyte",3]]],[[["usize",15]],[["opcode",4],["option",4,["opcode"]]]],[[["u8",15]],["value",3]],null,[[]],[[]],[[]],[[]],[[]],[[]],[[]],[[],["opcodeiter",3]],[[],["usize",15]],[[["usize",15]],["usize",15]],null,null,[[["codebyte",3]],["bool",15]],[[["instruction",4]],["bool",15]],[[]],[[["u8",15]]],[[],["option",4]],[[],["option",4]],[[["usize",15]],["option",4]],[[],["usize",15]],null,[[],[["option",4,["instruction"]],["instruction",4]]],[[]],[[]],[[]],[[]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["typeid",3]],[[],["typeid",3]],[[],["typeid",3]],[[],["typeid",3]],[[],["typeid",3]],[[],["typeid",3]],null,[[["chunk",3]]],[[["value",3],["u8",15]]],[[["usize",15],["instruction",4],["chunk",3]]],[[]],null,null,[[["value",3]],["value",3]],[[]],[[]],[[],["value",3]],[[]],[[["value",3]],["value",3]],[[["value",3]],["bool",15]],[[["formatter",3]],["result",6]],[[]],[[]],[[["value",3]],["value",3]],[[["value",3]],["bool",15]],[[],["value",3]],[[["f64",15]]],[[["value",3]],["value",3]],[[]],[[],["result",4]],[[],["result",4]],[[],["typeid",3]],[[["value",3]],["string",3]],null,null,null,null,[[]],[[]],[[]],[[]],[[]],[[]],null,null,null,[[]],[[["formatter",3]],["result",6]],[[]],[[]],[[]],null,[[],[["interpreterror",4],["result",4,["interpreterror"]]]],[[]],[[]],[[]],[[]],[[["chunk",3]]],[[],["value",3]],[[["value",3]]],null,[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["result",4]],[[],["typeid",3]],[[],["typeid",3]],[[],["typeid",3]]],"p":[[3,"CodeByte"],[4,"OpCode"],[4,"Instruction"],[3,"Chunk"],[3,"ChunkCursor"],[3,"OpCodeIter"],[13,"Constant"],[3,"Value"],[3,"ValueStack"],[3,"Vm"],[4,"InterpretError"]]}\
}');
if (window.initSearch) {window.initSearch(searchIndex)};
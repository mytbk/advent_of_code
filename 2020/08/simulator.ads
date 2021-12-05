package Simulator is
	type ArchState is record
		PC: Natural := 0;
		Accumulator: Integer := 0;
	end record;

	type Opcode_Type is (acc, jmp, nop);

	type Instruction is record
		opcode: Opcode_Type;
		arg: Integer;
	end record;

	function Assemble(asm: String) return Instruction;
	function Image(insn: Instruction) return String;
	procedure Execute(machine: in out ArchState; insn: in Instruction);
end Simulator;

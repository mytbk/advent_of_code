with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

package body Simulator is
	function Assemble(asm: String) return Instruction is
		insn: Instruction;
		Unused_Last: Positive;
	begin
		insn.opcode := Opcode_Type'Value(asm(asm'First .. asm'First + 2));
		Get(asm(asm'First + 4 .. asm'Last), insn.arg, Unused_Last);
		return insn;
	end Assemble;

	function Image(insn: Instruction) return String is
	begin
		return Opcode_Type'Image(insn.opcode) & Integer'Image(insn.arg);
	end Image;

	procedure Execute(machine: in out ArchState; insn: in Instruction) is
	begin
		case insn.opcode is
			when acc =>
				machine.Accumulator := machine.Accumulator + insn.arg;
				machine.PC := machine.PC + 1;
			when jmp =>
				machine.PC := machine.PC + insn.arg;
			when nop =>
				machine.PC := machine.PC + 1;
		end case;
	end Execute;
end Simulator;

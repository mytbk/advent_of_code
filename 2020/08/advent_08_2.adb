with Simulator;
use Simulator;
with Ada.Containers.Vectors;
with Ada.Text_IO;
use Ada.Text_IO;

procedure advent_08_2 is
	type Inst_Cell is record
		insn: Instruction;
		accessed: Boolean;
	end record;

	package IVec is new Ada.Containers.Vectors(Element_Type => Inst_Cell, Index_Type => Natural);
	subtype Rom_Type is IVec.Vector;

	procedure Try_Run(rom: in out Rom_Type; final_state: out ArchState) is
		state : ArchState;
	begin
		for cell of rom loop
			cell.accessed := False;
		end loop;

		state := (PC => IVec.First_Index(rom), Accumulator => 0);
		while state.PC <= IVec.Last_Index(rom) and then not rom(state.PC).accessed loop
			rom(state.PC).accessed := True;
			Execute(state, rom(state.PC).insn);
		end loop;

		final_state := state;
	end Try_Run;

	rom : Rom_Type;
	state : ArchState;
begin
	while not End_Of_File loop
		declare
			L: constant String := Get_Line;
			insn: constant Instruction := Assemble(L);
		begin
			rom.Append((insn => insn, accessed => False));
		end;
	end loop;

	for cell of rom loop
		declare
			orig_insn : constant Instruction := cell.insn;
		begin
			if orig_insn.opcode = nop then
				cell.insn.opcode := jmp;
			elsif orig_insn.opcode = jmp then
				cell.insn.opcode := nop;
			end if;

			Try_Run(rom, state);
			if state.PC > IVec.Last_Index(rom) then
				Put_Line("Change " & Image(orig_insn));
				Put_Line(Integer'Image(state.Accumulator));
			end if;

			cell.insn := orig_insn;
		end;
	end loop;
end advent_08_2;

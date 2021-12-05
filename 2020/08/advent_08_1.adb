with Simulator;
use Simulator;
with Ada.Containers.Vectors;
with Ada.Text_IO;
use Ada.Text_IO;

procedure advent_08_1 is
	type Inst_Cell is record
		insn: Instruction;
		accessed: Boolean;
	end record;

	package IVec is new Ada.Containers.Vectors(Element_Type => Inst_Cell, Index_Type => Natural);
	subtype Rom_Type is IVec.Vector;

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

	state.PC := IVec.First_Index(rom);
	while not rom(state.PC).accessed loop
		rom(state.PC).accessed := True;
		-- Put_Line("PC = " & Natural'Image(state.PC) & " Accumulator = " & Integer'Image(state.Accumulator));
		Execute(state, rom(state.PC).insn);
	end loop;

	Put_Line(Integer'Image(state.Accumulator));
end advent_08_1;

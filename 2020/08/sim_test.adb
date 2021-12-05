-- currently, only assembly test ... 
with Ada.Text_IO;
use Ada.Text_IO;
with Simulator;
use Simulator;

procedure Sim_Test is
begin
	while not End_Of_File loop
		declare
			L: constant String := Get_Line;
			insn: constant Instruction := Assemble(L);
		begin
			Put_Line(Image(insn));
		end;
	end loop;
end Sim_Test;

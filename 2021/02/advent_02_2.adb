with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Commands;
use Commands;

procedure advent_02_2 is
	type position is record
		horizontal: Integer;
		depth: Integer;
		aim: Integer;
	end record;

	procedure Run_Command(p: in out position; cmd: Command_Type) is
	begin
		case cmd.op is
			when forward =>
				p.horizontal := p.horizontal + cmd.x;
				p.depth := p.depth + cmd.x * p.aim;
			when up =>
				p.aim := p.aim - cmd.x;
			when down =>
				p.aim := p.aim + cmd.x;
		end case;
	end Run_Command;

	current_pos : position := (horizontal => 0, depth => 0, aim => 0);
begin
	while not End_Of_File loop
		declare
			cmdstr : constant String := Get_Line;
		begin
			Run_Command(current_pos, parse_command(cmdstr));
		end;
	end loop;

	Put(current_pos.horizontal * current_pos.depth);
end advent_02_2;

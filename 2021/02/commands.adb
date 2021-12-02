with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

package body Commands is
	function parse_command(s: String) return Command_Type is
		fstr : constant String := "forward";
		upstr: constant String := "up";
		downstr: constant String := "down";
		Unused_Last : Positive;
		cmd : Command_Type;
	begin
		if s'Length > fstr'Length and then s(s'First .. s'First + fstr'Length - 1) = fstr then
			cmd.op := forward;
			Get(s(s'First + fstr'Length .. s'Last), cmd.x, Unused_Last);
		elsif s'Length > upstr'Length and then s(s'First .. s'First + upstr'Length - 1) = upstr then
			cmd.op := up;
			Get(s(s'First + upstr'Length .. s'Last), cmd.x, Unused_Last);
		elsif s'Length > downstr'Length and then s(s'First .. s'First + downstr'Length - 1) = downstr then
			cmd.op := down;
			Get(s(s'First + downstr'Length .. s'Last), cmd.x, Unused_Last);
		else
			raise Data_Error;
		end if;
		return cmd;
	end parse_command;
end Commands;

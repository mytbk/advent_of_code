with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Text_IO;
use Ada.Text_IO;

procedure advent_01_1 is
	last_depth, current_depth : Natural;
	num_increases: Natural := 0;
begin
	Get(last_depth);
	while not End_Of_File loop
		Get(current_depth);
		if current_depth > last_depth then
			num_increases := num_increases + 1;
		end if;
		last_depth := current_depth;
	end loop;

	Put(num_increases);
end advent_01_1;

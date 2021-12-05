with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure advent_03_1 is
	column_increment: constant Integer := 3;
	column : Positive := 1;
	num_trees : Natural := 0;
begin
	while not End_Of_File loop
		declare
			row: constant String := Get_Line;
		begin
			if column > row'Last then
				if column mod row'Length = 0 then
					column := row'Length;
				else
					column := column mod row'Length;
				end if;
			end if;
			if row(column) = '#' then
				num_trees := num_trees + 1;
			end if;
		end;
		column := column + column_increment;
	end loop;

	Put(num_trees); New_Line;
end advent_03_1;

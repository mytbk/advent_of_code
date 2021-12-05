with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

procedure advent_03_2 is
	package String_Vectors is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Unbounded_String);
	subtype String_Vec is String_Vectors.Vector;

	function count_encountered_trees(map: String_Vec; row_increment, column_increment: Positive) return Natural is
		current_row : Natural := map.First_Index;
		current_column : Positive := 1;
		num_trees : Natural := 0;
	begin
		while current_row <= map.Last_Index loop
			declare
				row : Unbounded_String renames map(current_row);
				len : constant Natural := Length(row);
				c : Character;
			begin
				if current_column > len then
					if current_column mod len = 0 then
						c := Element(row, len);
					else
						c := Element(row, current_column mod len);
					end if;
				else
					c := Element(row, current_column);
				end if;
				
				if c = '#' then
					num_trees := num_trees + 1;
				end if;
			end;
			current_row := current_row + row_increment;
			current_column := current_column + column_increment;
		end loop;
		return num_trees;
	end count_encountered_trees;

	map : String_Vec;
	type Long_Natural is range 0 .. 2 ** 63 - 1;
	trees_multiplied : Long_Natural := 1;

	package Long_Natural_IO is new Ada.Text_IO.Integer_IO(Long_Natural);
	use Long_Natural_IO;

	type Slope_Type is record
		Right, Down : Positive;
	end record;
	type Slope_Array is array (Positive range 1 .. 5) of Slope_Type;
	slopes : constant Slope_Array := ( (1, 1), (3, 1), (5, 1), (7, 1), (1, 2) );
begin
	while not End_Of_File loop
		map.Append(To_Unbounded_String(Get_Line));
	end loop;

	for I in Slope_Array'Range loop
		declare
			slope : Slope_Type;
			num_trees: Natural;
		begin
			slope := slopes(I);
			num_trees := count_encountered_trees(map, slope.Down, slope.Right);
			Put(num_trees); New_Line;
			trees_multiplied := trees_multiplied * Long_Natural(num_trees);
		end;
	end loop;
	Put(trees_multiplied); New_Line;
end advent_03_2;

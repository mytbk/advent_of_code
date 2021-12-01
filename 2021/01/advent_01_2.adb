with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Text_IO;
use Ada.Text_IO;

procedure advent_01_2 is
	type Depth_Index is mod 3;
	type Depth_Array is array(Depth_Index) of Natural;

	current_depth_index: Depth_Index;
	saved_depths: Depth_Array;

	last_sum, current_sum: Natural;
	current_depth: Natural;
	num_increases: Natural := 0;
begin
	Get(saved_depths(0));
	Get(saved_depths(1));
	Get(saved_depths(2));
	last_sum := saved_depths(0) + saved_depths(1) + saved_depths(2);
	current_depth_index := 0;

	while not End_Of_File loop
		Get(current_depth);
		current_sum := last_sum - saved_depths(current_depth_index) + current_depth;
		saved_depths(current_depth_index) := current_depth;
		current_depth_index := Depth_Index'Succ(current_depth_index);

		if current_sum > last_sum then
			num_increases := num_increases + 1;
		end if;
		last_sum := current_sum;
	end loop;

	Put(num_increases);
end advent_01_2;

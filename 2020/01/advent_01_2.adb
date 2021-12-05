with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

-- End_Error is in Text_IO
with Ada.Text_IO;
use Ada.Text_IO;

procedure advent_01_2 is
	subtype Entry_Range is Integer range 0 .. 2020;
	number_map : array (Entry_Range) of Boolean := (others => False);
	input_num : Integer;
begin
	begin
		loop
			Get(input_num);
			if input_num >= Entry_Range'First and then input_num <= Entry_Range'Last then
				number_map(input_num) := True;
			end if;
		end loop;
	exception
		when End_Error => null;
	end;

	for I in Entry_Range loop
		if number_map(I) then
			for J in I + 1 .. Entry_Range'Last loop
				declare
					K: constant Integer := 2020 - I - J;
				begin
					exit when K <= J;
					if number_map(J) and number_map(K) then
						Put(I * J * K); New_Line;
					end if;
				end;
			end loop;
		end if;
	end loop;
end advent_01_2;

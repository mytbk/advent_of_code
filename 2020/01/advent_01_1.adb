with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

-- End_Error is in Text_IO
with Ada.Text_IO;
use Ada.Text_IO;

procedure advent_01 is
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
		declare
			J: constant Integer := Entry_Range'First + Entry_Range'Last - I;
		begin
			exit when I > J;
			if number_map(I) and then number_map(J) then
				Put(I * J, Width => 0);
				Put_Line("");
			end if;
		end;
	end loop;
end advent_01;

with Ada.Text_IO;
use Ada.Text_IO;

procedure advent_06_1 is
	total_questions: Natural := 0;

	subtype Question_Type is Character range 'a' .. 'z';
	type Yes_Questions is array (Question_Type) of Boolean;

	function Count_Yes(q: Yes_Questions) return Natural is
		count : Natural := 0;
	begin
		for I in q'Range loop
			if q(I) = True then
				count := count + 1;
			end if;
		end loop;
		return count;
	end Count_Yes;

	group_questions: Yes_Questions := (others => False);
begin
	while not End_Of_File loop
		declare
			line : String := Get_Line;
		begin
			if line'Length = 0 then
				total_questions := total_questions + Count_Yes(group_questions);
				group_questions := (others => False);
			else
				for c of line loop
					group_questions(c) := True;
				end loop;
			end if;
		end;
	end loop;

	total_questions := total_questions + Count_Yes(group_questions);
	Put_Line(Natural'Image(total_questions));
end advent_06_1;

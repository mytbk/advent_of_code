with Ada.Text_IO;
use Ada.Text_IO;

procedure advent_06_2 is
	total_questions: Natural := 0;

	type Yes_Questions_Bitmap is mod 2 ** 26;

	function Count_Yes(q: Yes_Questions_Bitmap) return Natural is
		count : Natural := 0;
		bitmap : Yes_Questions_Bitmap := q;
	begin
		-- implement popcount
		while bitmap /= 0 loop
			count := count + 1;
			bitmap := bitmap and (bitmap - 1);
		end loop;
		return count;
	end Count_Yes;

	group_questions: Yes_Questions_Bitmap := 2 ** 26 - 1;
begin
	while not End_Of_File loop
		declare
			line : String := Get_Line;
			question_per_persion : Yes_Questions_Bitmap := 0;
		begin
			if line'Length = 0 then
				total_questions := total_questions + Count_Yes(group_questions);
				group_questions := 2 ** 26 - 1;
			else
				for c of line loop
					question_per_persion := question_per_persion or (2 ** (Character'Pos(c) - Character'Pos('a')));
				end loop;
				group_questions := group_questions and question_per_persion;
			end if;
		end;
	end loop;

	total_questions := total_questions + Count_Yes(group_questions);
	Put_Line(Natural'Image(total_questions));
end advent_06_2;

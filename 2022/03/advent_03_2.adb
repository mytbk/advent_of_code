with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Advent_03_2 is
  function Priority(C: Character) return Positive is
  begin
    case C is
      when 'a' .. 'z' => return Character'Pos(C) - Character'Pos('a') + 1;
      when 'A' .. 'Z' => return Character'Pos(C) - Character'Pos('A') + 27;
      when others => raise Constraint_Error;
    end case;
  end Priority;

  function Find_All_Appear(S1, S2, S3: String) return Character is
  begin
    for C1 of S1 loop
      for C2 of S2 loop
        for C3 of S3 loop
	  if C1 = C2 and C1 = C3 then
	    return C1;
	  end if;
        end loop;
      end loop;
    end loop;

    raise Constraint_Error;
  end Find_All_Appear;

  Sum: Natural := 0;
begin
  while not End_Of_File loop
    declare
      S1: constant String := Get_Line;
      S2: constant String := Get_Line;
      S3: constant String := Get_Line;
    begin
      Sum := Sum + Priority(Find_All_Appear(S1, S2, S3));
    end;
  end loop;
  Put(Sum);
  New_Line;
end Advent_03_2;
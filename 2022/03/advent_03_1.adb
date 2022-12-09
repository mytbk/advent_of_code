with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Advent_03_1 is
  function Priority(C: Character) return Positive is
  begin
    case C is
      when 'a' .. 'z' => return Character'Pos(C) - Character'Pos('a') + 1;
      when 'A' .. 'Z' => return Character'Pos(C) - Character'Pos('A') + 27;
      when others => raise Constraint_Error;
    end case;
  end Priority;

  function Find_Both_Appear(S: String) return Character is
    Middle: constant Integer := (S'First + S'Last) / 2;
    Left: constant String := S(S'First .. Middle);
    Right: constant String := S(Middle + 1 .. S'Last);
  begin
    for Left_Char of Left loop
      for Right_Char of Right loop
        if Left_Char = Right_Char then
	  return Left_Char;
	end if;
      end loop;
    end loop;

    raise Constraint_Error;
  end Find_Both_Appear;

  Sum: Natural := 0;
begin
  while not End_Of_File loop
    Sum := Sum + Priority(Find_Both_Appear(Get_Line));
  end loop;
  Put(Sum);
  New_Line;
end Advent_03_1;
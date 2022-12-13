with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Advent_06 is
  S: constant String := Get_Line;

  function All_Unique(S: String) return Boolean is
  begin
    for I in S'Range loop
      for J in I + 1 .. S'Last loop
        if S(J) = S(I) then
	  return False;
	end if;
      end loop;
    end loop;
    return True;
  end All_Unique;
begin
  -- part 1
  for I in 4 .. S'Last loop
    if All_Unique(S(I - 3 .. I)) then
      Put(I);
      New_Line;
      exit;
    end if;
  end loop;
  -- part 2
  for I in 14 .. S'Last loop
    if All_Unique(S(I - 13 .. I)) then
      Put(I);
      New_Line;
      exit;
    end if;
  end loop;
end Advent_06;
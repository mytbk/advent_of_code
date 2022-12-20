with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Advent_20_1 is
  type Grove_Number is record
    Num: Integer;
    Moved: Boolean;
  end record;

  package Grove_Vectors is new Ada.Containers.Vectors
    (Index_Type => Natural, Element_Type => Grove_Number);

  subtype Grove_Vector is Grove_Vectors.Vector;

  Grove_Numbers: Grove_Vector;

  procedure Print_Grove_Numbers is
    begin
    for N of Grove_Numbers loop
      Put(N.Num);
    end loop;
    New_Line;
  end Print_Grove_Numbers;

begin
  while not End_Of_File loop
    declare
      N: Integer;
    begin
      Get(N);
      Grove_Numbers.Append((Num => N, Moved => False));
    end;
  end loop;

  declare
    I: Natural := 0;
    Last: constant Natural := Grove_Numbers.Last_Index;
    Len: constant Natural := Natural(Grove_Numbers.Length);
  begin
    Put(Len); Put_Line("Numbers.");
    while I <= Last loop
      if not Grove_Numbers(I).Moved then
        declare
	  N: Integer := Grove_Numbers(I).Num;
	  Tmp: constant Grove_Number := (Moved => True, Num => N);
	  -- Note that we'll wrap when moving Len - 1 steps
	  Move_Steps : Integer := N rem (Len - 1);
	  Moved_To: Integer;
	begin
	  if Move_Steps > 0 then
	    if I + Move_Steps > Last then
	      Moved_To := (I + Move_Steps + 1) mod Len;
	    else
	      Moved_To := I + Move_Steps;
	    end if;
	  elsif Move_Steps < 0 then
	    if I + Move_Steps <= 0 then
	      Moved_To := (I + Move_Steps - 1) mod Len;
	    else
	      Moved_To := I + Move_Steps;
	    end if;
	  else
	    Moved_To := I;
	  end if;

          if Moved_To > I then
	    for J in I .. Moved_To - 1 loop
	      Grove_Numbers(J) := Grove_Numbers(J + 1);
	    end loop;
	  else
	    for J in reverse Moved_To + 1 .. I loop
	      Grove_Numbers(J) := Grove_Numbers(J - 1);
	    end loop;
	  end if;

          Grove_Numbers(Moved_To) := Tmp;
          -- Print_Grove_Numbers;
	end;
      else
        I := I + 1;
      end if;
    end loop;
  end;

  -- find the number 0
  declare
    Len: constant Natural := Natural(Grove_Numbers.Length);
    Zero_Index: Natural;
    Index_1000, Index_2000, Index_3000: Natural;
    Sum: Integer;
  begin
    for I in 0 .. Grove_Numbers.Last_Index loop
      if Grove_Numbers(I).Num = 0 then
        Zero_Index := I;
	exit;
      end if;
    end loop;
    Index_1000 := (Zero_Index + 1000) mod Len;
    Index_2000 := (Zero_Index + 2000) mod Len;
    Index_3000 := (Zero_Index + 3000) mod Len;
    -- Put(Grove_Numbers(Index_1000).Num);
    -- Put(Grove_Numbers(Index_2000).Num);
    -- Put(Grove_Numbers(Index_3000).Num);
    -- New_Line;
    Sum := Grove_Numbers(Index_1000).Num + Grove_Numbers(Index_2000).Num + Grove_Numbers(Index_3000).Num;
    Put(Sum);
    New_Line;
  end;
end Advent_20_1;
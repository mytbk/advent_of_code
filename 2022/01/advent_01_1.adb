with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Advent_01_1 is
  Max_Energy : Natural := 0;
begin
  while not End_Of_File loop
    declare
      Total_Energy : Natural := 0;
      Energy : Natural;
    begin
      while not End_Of_File loop
        declare
          S : constant String := Get_Line;
	  Unused_Last : Positive;
        begin
	  exit when S'Length = 0;
  	  Get(S, Energy, Unused_Last);
        end;
        Total_Energy := Total_Energy + Energy;
      end loop;
      if Total_Energy > Max_Energy then
        Max_Energy := Total_Energy;
      end if;
    end;
  end loop;

  Put(Max_Energy);
  New_Line;
end Advent_01_1;
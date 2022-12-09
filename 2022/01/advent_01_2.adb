with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Advent_01_2 is
  type E_Index is range 1 .. 3;
  Top_Three_Energy : array (E_Index) of Natural := (others => 0);

  procedure Replace_Minimum(E: Natural) is
    Min_Index : E_Index := 1;
  begin
    for I in E_Index'Range loop
      if Top_Three_Energy(I) < Top_Three_Energy(Min_Index) then
        Min_Index := I;
      end if;
    end loop;
    if Top_Three_Energy(Min_Index) < E then
      Top_Three_Energy(Min_Index) := E;
    end if;
  end Replace_Minimum;
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

      Replace_Minimum(Total_Energy);
    end;
  end loop;

  declare
    Total_Energy_Top_Three : Natural := 0;
  begin
    for E of Top_Three_Energy loop
      Total_Energy_Top_Three := Total_Energy_Top_Three + E;
    end loop;
    Put(Total_Energy_Top_Three);
    New_Line;
  end;
end Advent_01_2;
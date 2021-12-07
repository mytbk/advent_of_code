-- TODO: refine the code
-- TODO: print the best horizontal position

with Ada.Text_Io;
use Ada.Text_Io;
with Ada.Integer_Text_Io;
use Ada.Integer_Text_Io;

with String_Util;
use String_Util;

with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

procedure Advent_07 is
   package Int_Vectors is new Ada.Containers.Vectors
     (Element_Type => Integer, Index_Type => Natural);
   subtype Int_Vector is Int_Vectors.Vector;
   
   package Int_Vector_Sort is new Int_Vectors.Generic_Sorting;
   
   Positions: Int_Vector;
   
   Mid_Value: Integer;
   Sum: Integer := 0;
   
   -- question 2: we want to get the minimum value of
   -- f(x) = Sum( (|x-a|(|x-a|+1))/2 )
   --      = 1/2 Sum((x**2)-2ax+a**2+|x-a|)
   --      = 1/2 (n*(x**2)-2(Sum(a))x+Sum(|x-a|)+C)
   -- for x in (a(i),a(i+1)), the x that makes f'(x)=0 is 2(Sum(a))/(n+2i)
   
   function Sum_Fuel(Positions: Int_Vector; Dest: Integer) return Integer is
      Sum : Integer := 0;
      Dist: Integer;
   begin
      for I of Positions loop
	 Dist := abs(I - Dest);
	 Sum := Sum + (Dist * (Dist + 1)) / 2;
      end loop;
      return Sum;
   end Sum_Fuel;
   
   function Minimum_Fuel(Positions: Int_Vector) return Integer is
      -- assume Positions is sorted
      Min_Fuel: Integer := Integer'Last;
      Sum_Vec: Integer := 0;
      Len: constant Positive := Positive(Positions.Length);
      Min_Point: Integer;
   begin
      for I of Positions loop
	 Sum_Vec := Sum_Vec + I;
      end loop;
      
      for I in Positions.First_Index .. Positions.Last_Index loop
	 Min_Point := 2 * Sum_Vec / (Len + 2 * (I - Positions.First_Index + 1));
	 -- just calculate it without considering if Min_Point is in range
	 Min_Fuel := Integer'Min(Min_Fuel, Sum_Fuel(Positions, Positions(I)));
	 Min_Fuel := Integer'Min(Min_Fuel, Sum_Fuel(Positions, Min_Point));
	 Min_Fuel := Integer'Min(Min_Fuel, Sum_Fuel(Positions, Min_Point + 1));
      end loop;
      return Min_Fuel;
   end Minimum_Fuel;
   
begin
   declare
      L: String := Get_Line;
      Int_Strings: StringVec := Split_String(L, ',');
   begin
      for I of Int_Strings loop
	 Positions.Append(Integer'Value(To_String(I)));
      end loop;
   end;
   Int_Vector_Sort.Sort(Positions);
   
   Mid_Value := Positions((Positions.First_Index + Positions.Last_Index) / 2);
   for I of Positions loop
      Sum := Sum + abs(I - Mid_Value);
   end loop;
   Put(Sum);
   Put(Minimum_Fuel(Positions));
end Advent_07;

with Ada.Containers.Vectors;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_Io; use Ada.Text_Io;

procedure Aoc_2024_01_1 is
   package Int_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Integer);
   subtype Int_Vector is Int_Vectors.Vector;
   
   package Int_Vector_Sorting is new Int_Vectors.Generic_Sorting;
   procedure Sort(V : in out Int_Vector) renames Int_Vector_Sorting.Sort;
   
   Left_List, Right_List : Int_Vector;
begin
   while not End_Of_File loop
      declare
	 L, R : Integer;
      begin
	 Get(L);
	 Get(R);
	 Left_List.Append(L);
	 Right_List.Append(R);
      end;
   end loop;
   
   Sort(Left_List);
   Sort(Right_List);
   
   declare
      Sum_Diff : Natural := 0;
   begin
      for I in Left_List.First_Index .. Left_List.Last_Index loop
	 Sum_Diff := Sum_Diff + abs(Left_List(I) - Right_List(I));
      end loop;
      Put(Sum_Diff); New_Line;
   end;
end Aoc_2024_01_1;

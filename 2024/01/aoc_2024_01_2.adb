with Ada.Containers.Vectors;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_Io; use Ada.Text_Io;

procedure Aoc_2024_01_2 is
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
      Similarity : Natural := 0;
      Right_Index : Natural := Right_List.First_Index;
      Last_Similarity : Natural;
      Checking_Num : Natural;
   begin
      for I in Left_List.First_Index .. Left_List.Last_Index loop
         if I > Left_List.First_Index and then Left_List(I) = Left_List(I - 1) then
	    Similarity := Similarity + Last_Similarity;
	 else
	    Last_Similarity := 0;
	    Checking_Num := Left_List(I);
	    while Right_Index /= Right_List.Last_Index and Right_List(Right_Index) < Checking_Num loop
	       Right_Index := Right_Index + 1;
	    end loop;

	    while Checking_Num = Right_List(Right_Index) loop
	       Last_Similarity := Last_Similarity + Checking_Num;
	       exit when Right_Index = Right_List.Last_Index;
	       Right_Index := Right_Index + 1;
	    end loop;
	    
	    Similarity := Similarity + Last_Similarity;
	 end if;
      end loop;
      Put(Similarity); New_Line;
   end;
end Aoc_2024_01_2;

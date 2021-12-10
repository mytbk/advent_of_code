with Ada.Text_Io; use Ada.Text_Io;
with Symbol_Matching; use Symbol_Matching;
with Ada.Containers.Vectors;

procedure Advent_10 is
   Sum_Points : Natural := 0;
   
   package Score_Vectors is new Ada.Containers.Vectors
     (Element_Type => Completion_Score_Type, Index_Type => Positive);
   package Score_Vector_Sort is new Score_Vectors.Generic_Sorting;
   use Score_Vector_Sort;
   
   subtype Score_Vector is Score_Vectors.Vector;
   Completion_Scores : Score_Vector;

begin
   while not End_Of_File loop
      declare
	 L : constant String := Get_Line;
	 First_Unmatched : Positive;
      begin
	 if not Check_Match(L, First_Unmatched) then
	    Sum_Points := Sum_Points + Symbol_Score(To_Symbol(L(First_Unmatched)));
	 else
	    Completion_Scores.Append(Completion_Score(L));
	 end if;
      end;
   end loop;
   Put_Line(Natural'Image(Sum_Points));
   
   Sort(Completion_Scores);
   Put_Line(Completion_Score_Type'Image
	      (Completion_Scores((Completion_Scores.First_Index + Completion_Scores.Last_Index) / 2)));
end Advent_10;

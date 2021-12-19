with Ada.Text_IO; use Ada.Text_IO;
with Pairs; use Pairs;
with Pairs.Parsing; use Pairs.Parsing;
with Ada.Containers.Vectors;

procedure Advent_18 is
   package Tree_Vectors is new Ada.Containers.Vectors
     (Element_Type => Pair_Access, Index_Type => Positive);
   subtype Tree_Vector is Tree_Vectors.Vector;
   
   All_Input_Trees : Tree_Vector;

   Tree: Pair_Access := null;
   Input_Tree: Pair_Access;
   Max_Magnitude: Integer := Integer'First;
begin
   while not End_Of_File loop
      Input_Tree := Parse_Pair_String(Get_Line);
      All_Input_Trees.Append(Input_Tree);
   end loop;
   
   for T of All_Input_Trees loop
      if Tree = null then
	 Tree := Clone(T.all);
      else
	 Tree := Add_Pairs(Tree, Clone(T.all));
	 -- Print_Pair(Tree.all); New_Line;
      end if;
   end loop;
   
   Put_Line(Integer'Image(Magnitude(Tree.all)));
   
   for I in All_Input_Trees.First_Index .. All_Input_Trees.Last_Index loop
      for J in All_Input_Trees.First_Index .. All_Input_Trees.Last_Index loop
	 if I /= J then
	    declare
	       Mag: Integer := Magnitude(Add_Pairs
					   (Clone(All_Input_Trees(I).all),
					    Clone(All_Input_Trees(J).all))
					   .all);
	    begin
	       if Mag > Max_Magnitude then
		  Max_Magnitude := Mag;
	       end if;
	    end;
	 end if;
      end loop;
   end loop;

   Put_Line(Integer'Image(Max_Magnitude));
   
   -- for Pt of All_Input_Trees loop
   --    Print_Pair(Pt.all); New_Line;
   -- end loop;
end Advent_18;

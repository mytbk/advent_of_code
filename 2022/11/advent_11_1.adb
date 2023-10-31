with Monkeys; use Monkeys;
with Monkey_Defs; use Monkey_Defs;
with Ada.Containers.Vectors;
with Ada.Text_Io; use Ada.Text_Io;

procedure Advent_11_1 is
   package Natural_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Natural);
   Num_Inspected : Natural_Vectors.Vector;
   
   package Natural_Vectors_Sorting is new Natural_Vectors.Generic_Sorting;
begin
   Init_Monkeys;
   for M of All_Monkeys loop
      Num_Inspected.Append(0);
   end loop;
   
   -- 20 rounds
   for Round in 1 .. 20 loop
      for I in All_Monkeys.First_Index .. All_Monkeys.Last_Index loop
	 declare
	    M: Monkey_Desc renames All_Monkeys(I);
	 begin
	    for Value of M.State.Items loop
	       declare
		  Level : constant Worry_Level := M.Subprogs.Operation(Value) / 3;
		  Next_Monkey : constant Integer := M.Subprogs.Next_Func(Level);
	       begin
		  All_Monkeys(Next_Monkey).State.Items.Append(Level);
	       end;
	    end loop;
	    Num_Inspected(I) := Num_Inspected(I) + Natural(M.State.Items.Length);
	    M.State.Items.Clear;
	 end;
      end loop;
   end loop;
   
   -- for N of Num_Inspected loop
   --    Put_Line(Natural'Image(N));
   -- end loop;
   
   Natural_Vectors_Sorting.Sort(Num_Inspected);
   declare
      Last : constant Natural := Num_Inspected.Last_Index;
   begin
      Put_Line(Natural'Image(Num_Inspected(Last) * Num_Inspected(Last - 1)));
   end;
end Advent_11_1;

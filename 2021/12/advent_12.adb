with Graph; use Graph;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_Io; use Ada.Text_Io;

procedure Advent_12 is
   G: Sparse_Graph;
   
   procedure Add_Edge(S: String) is
   begin
      for I in S'First .. S'Last loop
	 if S(I) = '-' then
	    Add_Edge(G, To_Unbounded_String(S(S'First .. I - 1)),
		     To_Unbounded_String(S(I + 1 .. S'Last)));
	 end if;
      end loop;
   end Add_Edge;
   
   Paths: Path_Vector;
begin
   Initialize(G);
   
   while not End_Of_File loop
      declare
	 L : constant String := Get_Line;
      begin
	 Add_Edge(L);
      end;
   end loop;
   
   Find_Paths(G, Paths);
   
   Put_Line(Natural'Image(Natural(Paths.Length)));
   
   -- part 2
   Find_Paths(G, Paths, Path_With_At_Most_One_Repeated_Small'Access);
   Put_Line(Natural'Image(Natural(Paths.Length)));
end Advent_12;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-- with Ada.Text_Io; use Ada.Text_Io;

package body Graph is
   use String_Vectors;
   
   procedure Initialize(G: in out Sparse_Graph) is
   begin
      Add_Vertex(G, To_Unbounded_String("start"));
      Add_Vertex(G, To_Unbounded_String("end"));
   end Initialize;
   
   procedure Add_Vertex(G: in out Sparse_Graph; Name: Unbounded_String) is
      Edges: Vertex_Vectors.Vector;
   begin
      for V of G.Vertex_Names loop
	 if V = Name then
	    return;
	 end if;
      end loop;
      G.Vertex_Names.Append(Name);
      G.Edge_List.Append(Edges);
   end Add_Vertex;
   
   function To_Vertex_Id(G: Sparse_Graph; Name: Unbounded_String) return Vertex_Id is
   begin
      for I in G.Vertex_Names.First_Index .. G.Vertex_Names.Last_Index loop
	 if G.Vertex_Names(I) = Name then
	    return I;
	 end if;
      end loop;
      raise Constraint_Error;
   end To_Vertex_Id;
   
   procedure Add_Edge(G: in out Sparse_Graph; Src: Unbounded_String; Dest: Unbounded_String) is
      Src_Id: Vertex_Id;
      Dest_Id: Vertex_Id;
   begin
      Add_Vertex(G, Src);
      Add_Vertex(G, Dest);
      Src_Id := To_Vertex_Id(G, Src);
      Dest_Id := To_Vertex_Id(G, Dest);
      G.Edge_List(Src_Id).Append(Dest_Id);
      G.Edge_List(Dest_Id).Append(Src_Id);
   end Add_Edge;
   
   function Path_Without_Repeated_Small_Cave(G: Sparse_Graph; Path: Vertex_Vector) return Boolean is
      Current_Vertex: constant Vertex_Id := Path.Last_Element;
   begin
      if Is_Small(G, Current_Vertex) then
	 for I in Path.First_Index .. Path.Last_Index - 1 loop
	    if Path(I) = Current_Vertex then
	       -- go to a same small cave twice
	       return False;
	    end if;
	 end loop;
      end if;
      return True;
   end Path_Without_Repeated_Small_Cave;
   
   function Path_With_At_Most_One_Repeated_Small(G: Sparse_Graph; Path: Vertex_Vector) return Boolean is
      type Visit_Times_Array is array (Vertex_Id range <>) of Natural;
      Visit_Times: Visit_Times_Array (1 .. G.Vertex_Names.Last_Index) := (others => 0);
      Has_Same_Small: Boolean := False;
   begin
      for V of Path loop
	 if Is_Small(G, V) then
	    Visit_Times(V) := Visit_Times(V) + 1;
	    if Visit_Times(V) > 1 then
	       -- special case: "start" and "end" can appear at most once
	       if V = Start_Vertex or else V = End_Vertex then
		  return False;
	       end if;
	       
	       if Has_Same_Small then
		  return False;
	       else
		  Has_Same_Small := True;
	       end if;
	    end if;
	 end if;
      end loop;
      return True;      
   end Path_With_At_Most_One_Repeated_Small;

   procedure Find_Paths(G: Sparse_Graph; Paths: out Path_Vector;
			Predicate: Graph_Predicate := Path_Without_Repeated_Small_Cave'Access) is
      Path: Vertex_Vectors.Vector;

      procedure Find_Paths_Dfs is
	 Current_Vertex: Vertex_Id;
      begin
	 if Path.Is_Empty then
	    -- finished, just return
	    return;
	 end if;
	 
	 --  Put("current path: ");
	 --  for V of Path loop
	 --     Put(To_String(G.Vertex_Names(V)));
	 --     Put(" ");
	 --  end loop;
	 --  New_Line;
	 
	 Current_Vertex := Path.Last_Element;
	 if Current_Vertex = End_Vertex then
	    -- find a valid path
	    
	    --  Put("found path: ");
	    --  for V of Path loop
	    --     Put(To_String(G.Vertex_Names(V)));
	    --     Put(" ");
	    --  end loop;
	    --  New_Line;

	    Paths.Append(Path);
	    return;
	 end if;
	 
	 if not Predicate(G, Path) then
	    return;
	 end if;
	 
	 -- add a vertex to search next
	 -- first add a dummy vertex, we change it later
	 Path.Append(Start_Vertex);
	 for V of G.Edge_List(Current_Vertex) loop
	    Path(Path.Last_Index) := V;
	    Find_Paths_Dfs;
	 end loop;
	 -- at last, remove the added vertex
	 Path.Delete_Last;
      end Find_Paths_Dfs;
   begin
      Paths.Clear;
      Path.Append(Start_Vertex); -- add "start" to path
      Find_Paths_Dfs;
   end Find_Paths;
   
   function Is_Small(G: Sparse_Graph; V: Vertex_Id) return Boolean is
      C: constant Character := To_String(G.Vertex_Names(V))(1);
   begin
      return C >= 'a' and then C <= 'z';
   end Is_Small;
end Graph;

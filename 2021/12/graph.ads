with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Graph is
   type Vertex_Id is new Positive;
   
   package String_Vectors is new Ada.Containers.Vectors
     (Element_Type => Unbounded_String, Index_Type => Vertex_Id);
   
   subtype Vertex_Name_List is String_Vectors.Vector;
   
   package Vertex_Vectors is new Ada.Containers.Vectors
     (Element_Type => Vertex_Id, Index_Type => Positive);
   -- Vertex_Vectors store vertices connected from a vertex
   
   use Vertex_Vectors;
   subtype Vertex_Vector is Vertex_Vectors.Vector;
   package Connected_Node_Vectors is new Ada.Containers.Vectors
     (Element_Type => Vertex_Vector, Index_Type => Vertex_Id);
   -- Connected_Node_Vectors store connected vertices for each vertex
   
   subtype Connected_Node_Vector is Connected_Node_Vectors.Vector;
   
   type Sparse_Graph is record
      Vertex_Names: Vertex_Name_List;
      Edge_List: Connected_Node_Vector;
   end record;
   
   Start_Vertex: constant Vertex_Id := 1;
   End_Vertex: constant Vertex_Id := 2;
   
   procedure Initialize(G: in out Sparse_Graph) with
     Post => (G.Vertex_Names(Start_Vertex) = "start" and G.Vertex_Names(End_Vertex) = "end");
   -- for convenience, we add two nodes "start" and "end"

   procedure Add_Vertex(G: in out Sparse_Graph; Name: Unbounded_String) with
     Post => G.Vertex_Names.Length = G.Edge_List.Length;
   -- if no vertex named with the specified name is in the graph, add it, otherwise do nothing
   
   function To_Vertex_Id(G: Sparse_Graph; Name: Unbounded_String) return Vertex_Id;

   procedure Add_Edge(G: in out Sparse_Graph; Src: Unbounded_String; Dest: Unbounded_String);
   
   package Path_Vectors is new Ada.Containers.Vectors
     (Element_Type => Vertex_Vectors.Vector, Index_Type => Positive);
   
   subtype Path_Vector is Path_Vectors.Vector;
   
   -- predicates to check the last vertex of a path
   type Graph_Predicate is access function(G: Sparse_Graph; Path: Vertex_Vector) return Boolean;
   function Path_Without_Repeated_Small_Cave(G: Sparse_Graph; Path: Vertex_Vector) return Boolean;
   function Path_With_At_Most_One_Repeated_Small(G: Sparse_Graph; Path: Vertex_Vector) return Boolean;
   
   procedure Find_Paths(G: Sparse_Graph; Paths: out Path_Vector;
			Predicate: Graph_Predicate := Path_Without_Repeated_Small_Cave'Access);
   
   function Is_Small(G: Sparse_Graph; V: Vertex_Id) return Boolean;
end Graph;

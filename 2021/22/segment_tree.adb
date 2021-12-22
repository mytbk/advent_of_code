with Ada.Text_Io; use Ada.Text_Io;
with Ada.Finalization;

package body Segment_Tree is
   --  function Image(Seg: Segment_Node) return String is
   --  begin
   --     return "(" & Segment_Type'Image(Seg.Min_Value) & "," & Segment_Type'Image(Seg.Max_Value) & ")";
   --  end Image;
   procedure Finalize(Node: in out Segment_Node) is
   begin
      Free(Node.Left);
      Free(Node.Right);
   end Finalize;
   
   function Clone(Node: Segment_Node) return Segment_Access is
      New_Node: Segment_Access := new Segment_Node;
   begin
      New_Node.all := Node;
      
      if Node.N_Type /= Leaf then
	 New_Node.Left := Clone(Node.Left.all);
	 New_Node.Right := Clone(Node.Right.all);
      end if;
      
      return New_Node;
   end Clone;

   function Half(A,B:Segment_Type) return Segment_Type is
   begin
      if (A mod 2) /= (B mod 2) then
	 return (A + B - 1) / 2;
      else
	 return (A + B) / 2;
      end if;
   end Half;

   procedure Split_X(Node: in out Segment_Node) is
      Mid_X : constant Segment_Type :=
	Half(Node.Min_Value, Node.Max_Value);
      
      Left : Segment_Access := Clone(Node);
      Right : Segment_Access := Clone(Node);
   begin
      Node.N_Type := Split_X;
      Node.Left := Left;
      Node.Right := Right;
      
      Left.Max_Value := Mid_X;
      Right.Min_Value := Mid_X + 1;
   end Split_X;
   
   procedure Split_Y(Node: in out Segment_Node) is
      Mid_Y : constant Segment_Type :=
	Half(Node.Min_Value, Node.Max_Value);
      
      Left : Segment_Access := Clone(Node);
      Right : Segment_Access := Clone(Node);
   begin
      Node.N_Type := Split_Y;
      Node.Left := Left;
      Node.Right := Right;
      
      if Node.Dim = Y then
	 -- nothing special if spliting a Y segment
	 Left.Max_Value := Mid_Y;
	 Right.Min_Value := Mid_Y + 1;
      else
	 -- if splitting an X segment
	 --
	 -- create two new Y segment, but keep the original N_Type,
	 -- Left and Right tree, which is cloned
	 Left.Dim := Y;
	 Left.Min_Value := Segment_Type'First;
	 Left.Max_Value := Half(Segment_Type'First, Segment_Type'Last);
	 
	 Right.Dim := Y;
	 Right.Min_Value := Left.Max_Value + 1;
	 Right.Max_Value := Segment_Type'Last;
      end if;
   end Split_Y;
   
   procedure Split_Z(Node: in out Segment_Node) is
      Mid_Z : constant Segment_Type :=
	Half(Node.Min_Value, Node.Max_Value);
      
      Left : Segment_Access := Clone(Node);
      Right : Segment_Access := Clone(Node);
   begin
      Node.N_Type := Split_Z;
      Node.Left := Left;     
      Node.Right := Right;
      
      if Node.Dim = Z then
	 -- nothing special if spliting a Y segment
	 Left.Max_Value := Mid_Z;
	 Right.Min_Value := Mid_Z + 1;
      else
	 -- if splitting an X or Y segment
	 --
	 -- create two new Z segment, but keep the original N_Type,
	 -- Left and Right tree, which is cloned
	 Left.Dim := Z;
	 Left.Min_Value := Segment_Type'First;
	 Left.Max_Value := Half(Segment_Type'First, Segment_Type'Last);
	 
	 Right.Dim := Z;
	 Right.Min_Value := Left.Max_Value + 1;
	 Right.Max_Value := Segment_Type'Last;
      end if;
   end Split_Z;
   
   procedure Insert(Node: in out Segment_Node;
		    Xmin,Xmax,Ymin,Ymax,Zmin,Zmax: Extended_Segment_Type;
		    V: Element_Type) is
      
      procedure Insert_X(Xmin,Xmax,Ymin,Ymax,Zmin,Zmax: Extended_Segment_Type;
			 V: Element_Type) is
      begin
	 if Xmin > Xmax or else Xmin > Node.Max_Value or else Xmax < Node.Min_Value then
	    return;
	 end if;
	 
	 if Node.N_Type /= Split_X and then
	   not (Node.Min_Value = Xmin and then Node.Max_Value = Xmax) then
	    Split_X(Node);
	 end if;

	 if Node.N_Type = Split_X then
	    Insert(Node.Left.all, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, V);
	    Insert(Node.Right.all, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, V);
	    return;
	 end if;
	 
	 pragma Assert(Node.Min_Value = Xmin and then Node.Max_Value = Xmax);
	 if Node.N_Type /= Split_Y then
	    Split_Y(Node);
	 end if;
	 
	 -- go inserting with Y
	 Insert(Node.Left.all, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, V);
	 Insert(Node.Right.all, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, V);
      end Insert_X;
      
      procedure Insert_Y(Ymin,Ymax,Zmin,Zmax: Extended_Segment_Type;
			 V: Element_Type) is
      begin
	 if Ymin > Ymax or else Ymin > Node.Max_Value or else Ymax < Node.Min_Value then
	    return;
	 end if;
	 
	 if Node.N_Type /= Split_Y and then
	   not (Node.Min_Value = Ymin and then Node.Max_Value = Ymax) then
	    Split_Y(Node);
	 end if;
	 
	 if Node.N_Type = Split_Y then
	    Insert(Node.Left.all, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, V);
	    Insert(Node.Right.all, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, V);
	    return;
	 end if;
	 
	 pragma Assert(Node.Min_Value = Ymin and then Node.Max_Value = Ymax);
	 if Node.N_Type /= Split_Z then
	    Split_Z(Node);
	 end if;
	 
	 -- go inserting with Z
	 Insert(Node.Left.all, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, V);
	 Insert(Node.Right.all, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, V);
      end Insert_Y;
      
      procedure Insert_Z(Zmin,Zmax: Extended_Segment_Type; V: Element_Type) is
      begin
	 if Zmin > Zmax or else Zmin > Node.Max_Value or else Zmax < Node.Min_Value then
	    return;
	 end if;
	 
	 if Node.Min_Value = Zmin and then Node.Max_Value = Zmax then
	    Node.N_Type := Leaf;
	    Node.Value := V;
	    Free(Node.Left);
	    Free(Node.Right);
	    Node.Left := null;
	    Node.Right := null;
	    return;
	 end if;
	 
	 if Node.N_Type /= Split_Z  then
	    Split_Z(Node);
	 end if;
	 
	 Insert(Node.Left.all, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, V);
	 Insert(Node.Right.all, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, V);
      end Insert_Z;
   begin
      case Node.Dim is
	 when X => Insert_X
	   (Segment_Type'Max(Node.Min_Value,Xmin), Segment_Type'Min(Node.Max_Value,Xmax),
	    Ymin, Ymax, Zmin, Zmax, V);
	 when Y => Insert_Y
	   (Segment_Type'Max(Node.Min_Value,Ymin), Segment_Type'Min(Node.Max_Value,Ymax),
	    Zmin, Zmax, V);
	 when Z => Insert_Z
	   (Segment_Type'Max(Node.Min_Value,Zmin), Segment_Type'Min(Node.Max_Value,Zmax), V);
      end case;
   end Insert;   
   
   function New_Segment_Cube return Segment_Access is
      New_Node : Segment_Access := new Segment_Node'
	(Ada.Finalization.Controlled with
	 Dim => X, N_Type => Leaf,
	 Min_Value => Segment_Type'First, Max_Value => Segment_Type'Last,
	 Left => null, Right => null, Value => False);
   begin
      return New_Node;
   end New_Segment_Cube;
   
   function Count_Value(Node: in out Segment_Node; V: Element_Type) return Seg_Count_Type is
      Count: Seg_Count_Type := 0;
      
      function Count_X(V: Element_Type) return Seg_Count_Type is
      begin
	 case Node.N_Type is
	    when Leaf =>
	       if Node.Value = V then
		  return Len(Node) * Max_Seg_Len * Max_Seg_Len;
	       else
		  return 0;
	       end if;
	    when Split_X =>
	       return Count_Value(Node.Left.all, V) + Count_Value(Node.Right.all, V);
	    when Split_Y =>
	       return Len(Node) *
		 (Count_Value(Node.Left.all, V) + Count_Value(Node.Right.all, V));
	    when Split_Z =>
	       return Len(Node) * Max_Seg_Len *
		 (Count_Value(Node.Left.all, V) + Count_Value(Node.Right.all, V));
	 end case;
      end Count_X;
      
      function Count_Y(V: Element_Type) return Seg_Count_Type is
      begin
	 case Node.N_Type is
	    when Leaf =>
	       if Node.Value = V then
		  return Len(Node) * Max_Seg_Len;
	       else
		  return 0;
	       end if;
	    when Split_Y =>
	       return Count_Value(Node.Left.all, V) + Count_Value(Node.Right.all, V);
	    when Split_Z =>
	       return Len(Node) *
		 (Count_Value(Node.Left.all, V) + Count_Value(Node.Right.all, V));
	    when others =>
	       raise Constraint_Error;
	 end case;
      end Count_Y;
      
      function Count_Z(V: Element_Type) return Seg_Count_Type is
      begin
	 case Node.N_Type is
	    when Leaf =>
	       if Node.Value = V then
		  return Len(Node);
	       else
		  return 0;
	       end if;
	    when Split_Z =>
	       return Count_Value(Node.Left.all, V) + Count_Value(Node.Right.all, V);
	    when others =>
	       raise Constraint_Error;
	 end case;
      end Count_Z;
   begin
      case Node.Dim is
	 when X => return Count_X(V);
	 when Y => return Count_Y(V);
	 when Z => return Count_Z(V);
      end case;
   end Count_Value;

end Segment_Tree;

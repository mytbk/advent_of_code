with Ada.Finalization;
with Ada.Unchecked_Deallocation;

package Segment_Tree is
   -- we just use integer here, can be made generic
   subtype Extended_Segment_Type is Integer;
   subtype Segment_Type is Extended_Segment_Type range -(10 ** 6) .. (10 ** 6);
   subtype Element_Type is Boolean;
   
   function Half(A,B:Segment_Type) return Segment_Type;
   
   type Seg_Count_Type is range 0 .. 2 ** 63 - 1;
   Max_Seg_Len : constant Seg_Count_Type :=
     Seg_Count_Type(Segment_Type'Last - Segment_Type'First + 1);
   
   type Segment_Node;
   type Segment_Access is access Segment_Node;
   
   type Dim_Type is (X, Y, Z);
   type Node_Type is (Leaf, Split_X, Split_Y, Split_Z);
   type Segment_Node is new Ada.Finalization.Controlled with record
      Dim: Dim_Type;
      N_Type: Node_Type;
      Min_Value: Segment_Type;
      Max_Value: Segment_Type;
      Left: Segment_Access := null;
      Right: Segment_Access := null;
      Value: Element_Type;
   end record;
   
   procedure Finalize(Node: in out Segment_Node);
   
   function Len(Seg: Segment_Node) return Seg_Count_Type is
     (Seg_Count_Type(Seg.Max_Value - Seg.Min_Value + 1));
   
   procedure Split_X(Node: in out Segment_Node) with
     Pre => Node.Dim = X and then Node.N_Type /= Split_X,
     Post => Node.N_Type = Split_X;
   procedure Split_Y(Node: in out Segment_Node) with
     Pre => Node.Dim <= Y and then (Node.N_Type = Leaf or else Node.N_Type > Split_Y),
     Post => Node.N_Type = Split_Y;
   procedure Split_Z(Node: in out Segment_Node) with
     Pre => Node.N_Type = Leaf,
     Post => Node.N_Type = Split_Z;
   
   function New_Segment_Cube return Segment_Access;
   procedure Insert(Node: in out Segment_Node; Xmin,Xmax,Ymin,Ymax,Zmin,Zmax: Extended_Segment_Type; V: Element_Type);
   function Count_Value(Node: in out Segment_Node; V: Element_Type) return Seg_Count_Type;

   function Clone(Node: Segment_Node) return Segment_Access;
   
   procedure Free is new Ada.Unchecked_Deallocation
     (Segment_Node, Segment_Access);
end Segment_Tree;

package Pairs is
   type Pair is private;
   type Pair_Access is access Pair;
   
   function Is_Leaf(P: Pair) return Boolean;
   function Get_Value(P: Pair) return Integer with
       Pre => Is_Leaf(P);
   
   type Explode_Op is (Explode, Add_Left, Add_Right, Completed, Nop);
   type Explode_Result(Op: Explode_Op) is record
      case Op is
	 when Add_Left | Add_Right =>
	    Value: Integer;
	 when Explode =>
	    Left, Right: Integer;
	 when others =>
	    null;
      end case;
   end record;
   
   function Clone(P: Pair) return Pair_Access;
   -- clone a binary tree

   function Try_Explode_Pair(P: in out Pair; Depth: Natural := 0) return Explode_Result;
   function Try_Split_Pair(P: in out Pair) return Boolean;
   
   function Add_Pairs(P1,P2: Pair_Access) return Pair_Access;
   -- Note: the binary tree P1, P2 can change
   
   function Magnitude(P: Pair) return Integer;
   
   procedure Print_Pair(P: Pair);
private
   type Pair is record
      Left, Right: Pair_Access := null;
      Value: Integer;
   end record;
   
   function New_Value_Node(V: Integer) return Pair_Access;
   function New_Parent_Node(L, R: Pair_Access) return Pair_Access;
end Pairs;

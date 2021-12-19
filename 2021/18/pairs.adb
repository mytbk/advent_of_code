with Ada.Text_Io;
use Ada.Text_Io;

package body Pairs is
   function Is_Leaf(P: Pair) return Boolean is
   begin
      return P.Left = null and P.Right = null;
   end Is_Leaf;
   
   function Get_Value(P: Pair) return Integer is
   begin
      return P.Value;
   end Get_Value;
   
   function New_Value_Node(V: Integer) return Pair_Access is
   begin
      return new Pair'(Left => null, Right => null, Value => V);
   end New_Value_Node;

   function New_Parent_Node(L, R: Pair_Access) return Pair_Access is
   begin
      return new Pair'(Left => L, Right => R, Value => 0);
   end New_Parent_Node;
   
   function Clone(P: Pair) return Pair_Access is
   begin
      if Is_Leaf(P) then
	 return New_Value_Node(Get_Value(P));
      else
	 return New_Parent_Node(Clone(P.Left.all), Clone(P.Right.all));
      end if;
   end Clone;

   function Try_Explode_Pair(P: in out Pair; Depth: Natural := 0) return Explode_Result is
      -- add val to the leftmost node of P
      procedure Add_Left(P: in out Pair; Val: Integer) is
      begin
	 if Is_Leaf(P) then
	    P.Value := P.Value + Val;
	 else
	    Add_Left(P.Left.all, Val);
	 end if;
      end Add_Left;
      -- add val to the rightmost node of P
      procedure Add_Right(P: in out Pair; Val: Integer) is
      begin
	 if Is_Leaf(P) then
	    P.Value := P.Value + Val;
	 else
	    Add_Right(P.Right.all, Val);
	 end if;
      end Add_Right;
      
   begin
      if Is_Leaf(P) then
	 return (Op => Nop);
      end if;
      
      -- Put_Line("exploding:");
      -- Print_Pair(P);
      -- New_Line;

      if Is_Leaf(P.Left.all) and then Is_Leaf(P.Right.all) then
	 if Depth >= 4 then
	    declare
	       Ret: constant Explode_Result := (Op => Explode,
						Left => Get_Value(P.Left.all),
						Right => Get_Value(P.Right.all));
	    begin
	       P.Left := null;
	       P.Right := null;
	       P.Value := 0;
	       return Ret;
	    end;
	 else
	    return (Op => Nop);
	 end if;
      end if;
      
      declare
	 Exp_Res: constant Explode_Result := Try_Explode_Pair(P.Left.all, Depth + 1);
      begin
	 case Exp_Res.Op is
	    when Explode =>
	       Add_Left(P.Right.all, Exp_Res.Right);
	       return (Op => Add_Left, Value => Exp_Res.Left);
	    when Add_Left =>
	       return Exp_Res;
	    when Add_Right =>
	       Add_Left(P.Right.all, Exp_Res.Value);
	       return (Op => Completed);
	    when Completed =>
	       return (Op => Completed);
	    when others => null; -- continue to the right part
	 end case;
      end;
      
      declare
	 Exp_Res: constant Explode_Result := Try_Explode_Pair(P.Right.all, Depth + 1);
      begin
	 case Exp_Res.Op is
	    when Explode =>
	       Add_Right(P.Left.all, Exp_Res.Left);
	       return (Op => Add_Right, Value => Exp_Res.Right);
	    when Add_Left =>
	       Add_Right(P.Left.all, Exp_Res.Value);
	       return (Op => Completed);
	    when Add_Right =>
	       return Exp_Res;
	    when others =>
	       return Exp_Res;
	 end case;
      end;
   end Try_Explode_Pair;
   
   function Try_Split_Pair(P: in out Pair) return Boolean is
   begin
      if Is_Leaf(P) then
	 declare
	    V: constant Integer := Get_Value(P);
	    Half: constant Integer := V / 2;
	 begin
	    if V >= 10 then
	       P.Left := New_Value_Node(Half);
	       P.Right := New_Value_Node(V - Half);
	       return True;
	    else
	       return False;
	    end if;
	 end;
      end if;
      
      if Try_Split_Pair(P.Left.all) then
	 return True;
      else
	 return Try_Split_Pair(P.Right.all);
      end if;
   end Try_Split_Pair;
   
   function Add_Pairs(P1,P2: Pair_Access) return Pair_Access is
      New_Tree: Pair_Access := New_Parent_Node(P1, P2);
   begin
      loop
	 declare
	    Exp_Res: Explode_Result := Try_Explode_Pair(New_Tree.all);
	 begin
	    if Exp_Res.Op = Nop then
	       if not Try_Split_Pair(New_Tree.all) then
		  return New_Tree;
	       end if;
	    end if;
	 end;
      end loop;
   end Add_Pairs;
   
   function Magnitude(P: Pair) return Integer is
   begin
      if Is_Leaf(P) then
	 return Get_Value(P);
      end if;
      
      return Magnitude(P.Left.all) * 3 + Magnitude(P.Right.all) * 2;
   end Magnitude;
   
   procedure Print_Pair(P: Pair) is
   begin
      if Is_Leaf(P) then
	 Put(Integer'Image(Get_Value(P)));
	 return;
      end if;
      Put("(");
      Print_Pair(P.Left.all);
      Put(" . ");
      Print_Pair(P.Right.all);
      Put(")");
   end Print_Pair;
end Pairs;

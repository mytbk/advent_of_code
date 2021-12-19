with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;

package body Pairs.Parsing is
   function Parse_Pair_String(S: String; Last: out Positive) return Pair_Access is
      Next_Index: Positive := S'First;
      
      procedure Skip_Spaces is
      begin
	 while Next_Index <= S'Last and then S(Next_Index) = ' ' loop
	    Next_Index := Next_Index + 1;
	 end loop;
      end Skip_Spaces;
      
      function Parse_Node(Last: out Positive) return Pair_Access is
	 Last_Out: Positive;
	 Number: Integer;
	 Node: Pair_Access;
      begin
	 if S(Next_Index) = '[' then
	    Node := Parse_Pair_String(S(Next_Index .. S'Last), Last_Out);
	    Last := Last_Out;
	    return Node;
	 else
	    Get(S(Next_Index .. S'Last), Number, Last_Out);
	    Node := New_Value_Node(Number);
	    Last := Last_Out;
	    return Node;
	 end if;
      end Parse_Node;

      Left, Right : Pair_Access := null;
      Last_Out: Positive;
   begin
      Skip_Spaces;
      if S(Next_Index) /= '[' then
	 raise Parse_Error;
      end if;
      
      -- skip '['
      Next_Index := Next_Index + 1;
      Skip_Spaces;
      
      Left := Parse_Node(Last_Out);
      Next_Index := Last_Out + 1;
      Skip_Spaces;
      
      if Next_Index > S'Last or else S(Next_Index) /= ',' then
	 raise Parse_Error;
      end if;
      
      Next_Index := Next_Index + 1;
      Skip_Spaces;
      
      Right := Parse_Node(Last_Out);
      Next_Index := Last_Out + 1;
      Skip_Spaces;

      if Next_Index > S'Last or else S(Next_Index) /= ']' then
	 raise Parse_Error;
      end if;
      
      Last := Next_Index;
      return New_Parent_Node(Left, Right);
   end Parse_Pair_String;
   
   function Parse_Pair_String(S: String) return Pair_Access is
      Unused_Last: Positive;
   begin
      return Parse_Pair_String(S, Unused_Last);
   end Parse_Pair_String;
   
end Pairs.Parsing;

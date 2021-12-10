package Symbol_Matching is
   type Chunk_Symbol is ('(', '[', '{', '<', ')', ']', '}', '>');
   subtype Open_Symbol is Chunk_Symbol range '(' .. '<';
   subtype Closing_Symbol is Chunk_Symbol range ')' .. '>';
   
   Symbol_Score: constant array (Closing_Symbol) of Natural := (3, 57, 1197, 25137);
   Matched_Close: constant array (Open_Symbol) of Closing_Symbol := (')', ']', '}', '>');
   
   type Open_Symbol_Array is array (Positive range <>) of Open_Symbol;
   type Open_Symbol_Stack(Max_Length: Positive) is record
      Stack_Top: Natural := 0;
      Data: Open_Symbol_Array (1 .. Max_Length);
   end record;
   
   procedure Push(Stack: in out Open_Symbol_Stack; Sym: Open_Symbol);
   procedure Pop(Stack: in out Open_Symbol_Stack);
   function Empty(Stack: Open_Symbol_Stack) return Boolean is (Stack.Stack_Top = 0);
   function Top(Stack: Open_Symbol_Stack) return Open_Symbol is (Stack.Data(Stack.Stack_Top));
   
   function To_Symbol(C: Character) return Chunk_Symbol;
   function Check_Match(S: String; First_Unmatch: out Positive) return Boolean;
   
   type Completion_Score_Type is range 0 .. 2 ** 63 - 1;
   function Completion_Score(S: String) return Completion_Score_Type;
private
   Complete_Score: constant array (Closing_Symbol) of Completion_Score_Type := (1, 2, 3, 4);
end Symbol_Matching;

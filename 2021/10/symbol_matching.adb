package body Symbol_Matching is
   procedure Push(Stack: in out Open_Symbol_Stack; Sym: Open_Symbol) is
   begin
      Stack.Stack_Top := Stack.Stack_Top + 1;
      Stack.Data(Stack.Stack_Top) := Sym;
   end Push;
   
   procedure Pop(Stack: in out Open_Symbol_Stack) is
   begin
      Stack.Stack_Top := Stack.Stack_Top - 1;
   end Pop;
      
   function To_Symbol(C: Character) return Chunk_Symbol is
      Table: constant array (Character) of Chunk_Symbol :=
	('(' => '(', '[' => '[', '{' => '{', '<' => '<',
	 ')' => ')', ']' => ']', '}' => '}', '>' => '>',
	 others => <>);
   begin
      case C is
	 when '(' | ')' | '[' | ']' | '{' | '}' | '<' | '>' => return Table(C);
	 when others => raise Constraint_Error;
      end case;
   end To_Symbol;

   function Check_Match(S: String; First_Unmatch: out Positive) return Boolean is
      Stack: Open_Symbol_Stack(S'Length);
      Current_Symbol: Chunk_Symbol;
   begin
      for I in S'First .. S'Last loop
	 Current_Symbol := To_Symbol(S(I));
	 if Current_Symbol <= Open_Symbol'Last then
	    Push(Stack, Current_Symbol);
	 else
	    if Empty(Stack) or else Matched_Close(Top(Stack)) /= Current_Symbol then
	       First_Unmatch := I;
	       return False;
	    else
	       Pop(Stack);
	    end if;
	 end if;
      end loop;
      return True;
   end Check_Match;
   
   function Completion_Score(S: String) return Completion_Score_Type is
      Current_Symbol: Chunk_Symbol;
      Stack: Open_Symbol_Stack(S'Length);
      Score: Completion_Score_Type := 0;
   begin
      for I in S'First .. S'Last loop
	 Current_Symbol := To_Symbol(S(I));
	 if Current_Symbol <= Open_Symbol'Last then
	    Push(Stack, Current_Symbol);
	 else
	    if Empty(Stack) or else Matched_Close(Top(Stack)) /= Current_Symbol then
	       raise Constraint_Error;
	    else
	       Pop(Stack);
	    end if;
	 end if;
      end loop;
      -- now calculate the score for the remaining stuff
      while not Empty(Stack) loop
	 Score := Score * 5 + Complete_Score(Matched_Close(Top(Stack)));
	 Pop(Stack);
      end loop;
      return Score;
   end Completion_Score;

end Symbol_Matching;

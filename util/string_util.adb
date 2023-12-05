package body String_Util is
	function Split_String(src: String; delim: Character) return String_Vec is
		vec: String_Vec;
		start: Positive := src'First;
	begin
		for idx in src'Range loop
			if src(idx) = delim then
				if idx > start then
					vec.Append(To_Unbounded_String(src(start .. idx - 1)));
				else
					vec.Append(To_Unbounded_String(""));
				end if;
				start := idx + 1;
			end if;
		end loop;
		if start <= src'Last then
			vec.Append(To_Unbounded_String(src(start .. src'Last)));
		else
			vec.Append(To_Unbounded_String(""));
		end if;
		return vec;
	end Split_String;
   
   procedure Find_Number(S : String; Num : out Integer; Found : out Boolean; Last : out Positive) is
      Pos, End_Pos : Positive;
      
      function Is_Num_Char(C : Character) return Boolean is
      begin
	 return C = '-' or (C >= '0' and C <= '9');
      end Is_Num_Char;
   begin
      Pos := S'First;
      while Pos <= S'Last and then (not Is_Num_Char(S(Pos))) loop
	 Pos := Pos + 1;
      end loop;
      
      if Pos > S'Last then
	 Found := False;
	 return;
      end if;

      End_Pos := Pos;
      while End_Pos <= S'Last and then Is_Num_Char(S(End_Pos)) loop
	 End_Pos := End_Pos + 1;
      end loop;
      
      Found := True;
      Num := Integer'Value(S(Pos .. End_Pos - 1));
      Last := End_Pos - 1;
   end Find_Number;
   
   function Contains(S : String; Key : String) return Boolean is
   begin
      for I in S'Range loop
	 exit when I + Key'Length - 1 > S'Last;
	 if S(I .. I + Key'Length - 1) = Key then
	    return True;
	 end if;
      end loop;
      return False;
   end Contains;

end String_Util;

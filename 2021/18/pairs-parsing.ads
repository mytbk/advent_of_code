package Pairs.Parsing is
   Parse_Error: exception;

   function Parse_Pair_String(S: String; Last: out Positive) return Pair_Access;
   function Parse_Pair_String(S: String) return Pair_Access;
end Pairs.Parsing;

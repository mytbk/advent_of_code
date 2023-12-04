with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package String_Util is
	package String_Vectors is new Ada.Containers.Vectors(
		Element_Type => Unbounded_String,
		Index_Type => Positive);

	subtype StringVec is String_Vectors.Vector;

   function Split_String(src: String; delim: Character) return StringVec;
   -- Split a string with a specific delimeter, return an unbounded string vector
   
   procedure Find_Number(S : String; Num : out Integer;
			 Found : out Boolean; Last : out Positive);
   -- Find a number in the string S, store the last digit position in Last
   
   function Contains(S : String; Key : String) return Boolean;
   -- check if string S has Key as substring
end String_Util;

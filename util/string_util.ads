with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package String_Util is
	package String_Vectors is new Ada.Containers.Vectors(
		Element_Type => Unbounded_String,
		Index_Type => Positive);

	subtype StringVec is String_Vectors.Vector;

	function Split_String(src: String; delim: Character) return StringVec;
end String_Util;

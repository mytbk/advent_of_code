-- grammar of bag rules:
--   <rule> ::= <color> "bags contain" <bag list> "."
--   <bag list> ::= "no other bags" | <bag desc> {, <bag desc>}...
--   <bag desc> ::= <number> <color> ("bag" | "bags")
--
--   <color> is a list of words, <number> is a digit string

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Bag_Rules is
	type Inner_Bag_Desc is record
		Num: Natural;
		Color: Unbounded_String;
	end record;

	package IBDVec is new Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Inner_Bag_Desc);
	subtype Bag_Desc_List is IBDVec.Vector;

	type Bag_Rule is record
		Outer_Bag_Color: Unbounded_String;
		Inner_Bags: Bag_Desc_List;
	end record;

	function Parse_Bag_Rule(rule_string: String) return Bag_Rule;
	procedure Print_Bag_Rule(rule: Bag_Rule);
private
	function Parse_Bag_Desc_List(desc_string: String) return Bag_Desc_List;
end Bag_Rules;

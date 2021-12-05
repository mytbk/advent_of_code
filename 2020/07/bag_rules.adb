with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Text_IO;
use Ada.Text_IO;

package body Bag_Rules is
	procedure Print_Bag_Rule(rule: Bag_Rule) is
	begin
		Put(To_String(rule.Outer_Bag_Color)  & " bags contain ");
		for inner of rule.Inner_Bags loop
			Put(inner.Num, 0);
			Put(" " & To_String(inner.Color) & " bags,");
		end loop;
		New_Line;
	end Print_Bag_Rule;

	function Parse_Bag_Rule(rule_string: String) return Bag_Rule is
		bags_contains_str: constant String := " bags contain ";
		result: Bag_Rule;
	begin
		for I in rule_string'Range loop
			exit when I + bags_contains_str'Length - 1 > rule_string'Last;
			if rule_string(I .. I + bags_contains_str'Length - 1) = bags_contains_str then
				result.Outer_Bag_Color := To_Unbounded_String(rule_string(rule_string'First .. I - 1));
				result.Inner_Bags := Parse_Bag_Desc_List(
					rule_string(I + bags_contains_str'Length .. rule_string'Last));
				return result;
			end if;
		end loop;
		raise Data_Error;
	end Parse_Bag_Rule;

	function Parse_Bag_Desc_List(desc_string: String) return Bag_Desc_List is
		desc_list: Bag_Desc_List;
		no_other_bags_str: constant String := "no other bags";
		I: Positive := desc_string'First;
	begin
		if desc_string'Length >= no_other_bags_str'Length and then
			desc_string(desc_string'First .. desc_string'First + no_other_bags_str'Length - 1) = no_other_bags_str
		then
			return desc_list;
		end if;

		while I <= desc_string'Last and then Is_Digit(desc_string(I)) loop
			declare
				inner_bag: Inner_Bag_Desc;
				Last: Positive;
			begin
				Get(desc_string(I .. desc_string'Last), inner_bag.Num, Last);
				I := Last + 1;
				while I < desc_string'Last and then desc_string(I) = ' ' loop
					I := I + 1;
				end loop;

				for J in I .. desc_string'Last loop
					if desc_string(J .. J + 3) = " bag" or else desc_string(J .. J + 4) = " bags"
					then
						inner_bag.Color := To_Unbounded_String(desc_string(I .. J - 1));
						desc_list.Append(inner_bag);
						I := J + 4; -- " bag"
						if desc_string(I) = 's' then -- " bags"
							I := I + 1;
						end if;
						exit;
					end if;
				end loop;

				-- clean up ',' and spaces
				while I <= desc_string'Last and then
					(desc_string(I) = ',' or else desc_string(I) = '.' or else desc_string(I) = ' ') loop
					I := I + 1;
				end loop;
			end;
		end loop;
		return desc_list;
	end Parse_Bag_Desc_List;
end Bag_Rules;

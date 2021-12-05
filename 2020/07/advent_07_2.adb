with Ada.Containers.Vectors;
with Ada.Text_IO;
use Ada.Text_IO;
with Bag_Rules;
use Bag_Rules;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

procedure advent_07_2 is
	type Bag_Rule_Node is record
		rule : Bag_Rule;
		visited : Boolean;
		count : Natural;
	end record;
	package BRV is new Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Bag_Rule_Node);
	subtype Bag_Rule_Vec is BRV.Vector;

	bag_rules : Bag_Rule_Vec;
	num_connects_to : Natural := 0;

	-- brute force search ...
	function count_bags(rules: in out Bag_Rule_Vec; color: String) return Natural is
	begin
		for rule_node of rules loop
			if To_String(rule_node.rule.Outer_Bag_Color) = color then
				declare
					result : Natural := 0;
				begin
					if rule_node.visited then
						return rule_node.count;
					end if;

					for inner of rule_node.rule.Inner_Bags loop
						result := result + inner.Num + inner.Num * count_bags(rules, To_String(inner.Color));
					end loop;
					rule_node.visited := True;
					rule_node.count := result;
					return result;
				end;
			end if;
		end loop;
		return 0;
	end count_bags;
begin
	while not End_Of_File loop
		declare
			L: constant String := Get_Line;
			rule : constant Bag_Rule := Parse_Bag_Rule(L);
		begin
			bag_rules.Append((rule => rule, visited => False, count => 0));
		end;
	end loop;

	Put(Natural'Image(count_bags(bag_rules, "shiny gold")));
end advent_07_2;

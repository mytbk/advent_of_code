with Ada.Containers.Vectors;
with Ada.Text_IO;
use Ada.Text_IO;
with Bag_Rules;
use Bag_Rules;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

procedure advent_07_1 is
	type Bag_Rule_Node is record
		rule : Bag_Rule;
		connects_to : Boolean;
	end record;
	package BRV is new Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Bag_Rule_Node);
	subtype Bag_Rule_Vec is BRV.Vector;

	bag_rules : Bag_Rule_Vec;
	num_connects_to : Natural := 0;

	-- brute force search ...
	procedure search(rules: in out Bag_Rule_Vec; color: String) is
	begin
		for rule_node of rules loop
			if not rule_node.connects_to then
				for inner of rule_node.rule.Inner_Bags loop
					if To_String(inner.Color) = color then
						rule_node.connects_to := True;
						search(rules, To_String(rule_node.rule.Outer_Bag_Color));
					end if;
				end loop;
			end if;
		end loop;
	end search;
begin
	while not End_Of_File loop
		declare
			L: constant String := Get_Line;
			rule : constant Bag_Rule := Parse_Bag_Rule(L);
		begin
			bag_rules.Append((rule => rule, connects_to => False));
		end;
	end loop;

	search(bag_rules, "shiny gold");

	for node of bag_rules loop
		if node.connects_to then
			num_connects_to := num_connects_to + 1;
		end if;
	end loop;

	Put(Natural'Image(num_connects_to));
end advent_07_1;

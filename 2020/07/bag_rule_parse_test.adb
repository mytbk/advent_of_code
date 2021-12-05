with Ada.Text_IO;
use Ada.Text_IO;
with Bag_Rules;
use Bag_Rules;

procedure bag_rule_parse_test is
begin
	while not End_Of_File loop
		declare
			L: constant String := Get_Line;
			rule: constant Bag_Rule := Parse_Bag_Rule(L);
		begin
			Print_Bag_Rule(rule);
		end;
	end loop;
end bag_rule_parse_test;

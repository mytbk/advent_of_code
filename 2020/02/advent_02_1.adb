with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Password_Policy;
use Password_Policy;

procedure advent_02_1 is
	function check_policy_1(passwd: String; policy: Password_Policy_Type) return Boolean is
		count: Natural := 0;
	begin
		for I in passwd'Range loop
			if passwd(I) = policy.Letter then
				count := count + 1;
			end if;
		end loop;
		return count >= policy.Min and count <= policy.Max;
	end check_policy_1;

	function check_policy_2(passwd: String; policy: Password_Policy_Type) return Boolean is
		num: Integer := 0;
	begin
		if policy.Min >= passwd'First and then policy.Min <= passwd'Last and then passwd(policy.Min) = policy.Letter then
			num := num + 1;
		end if;
		if policy.Max >= passwd'First and then policy.Max <= passwd'Last and then passwd(policy.Max) = policy.Letter then
			num := num + 1;
		end if;
		return num = 1;
	end check_policy_2;

	num_valid_1: Natural := 0;
	num_valid_2: Natural := 0;
begin
	while not End_Of_File loop
		declare
			policy_and_passwd : constant String := Get_Line;
			pol : Password_Policy_Type;
			passwd : String := policy_and_passwd;
			passwd_len: Natural;
		begin
			parse_policy_passwd(policy_and_passwd, pol, passwd, passwd_len);
			if check_policy_1(passwd(passwd'First .. passwd_len), pol) then
				num_valid_1 := num_valid_1 + 1;
			end if;
			if check_policy_2(passwd(passwd'First .. passwd_len), pol) then
				Put_Line(policy_and_passwd);
				num_valid_2 := num_valid_2 + 1;
			end if;
		end;
	end loop;
	Put(num_valid_1); New_Line;
	Put(num_valid_2); New_Line;
end advent_02_1;

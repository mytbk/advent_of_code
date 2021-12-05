with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Characters.Handling;
use Ada.Characters.Handling;

package body Password_Policy is
	procedure parse_policy_passwd(policy_passwd: String; policy: out Password_Policy_Type; passwd: out String; passwd_len: out Natural) is
		I: Positive := policy_passwd'First;
		Last : constant Positive := policy_passwd'Last;
	begin
		if Is_Digit(policy_passwd(I)) then
			Get(policy_passwd, policy.Min, I);
			I := I + 1;
		else
			raise Data_Error;
		end if;
		-- Put(policy.Min);
		if I <= Last and then policy_passwd(I) = '-' then
			I := I + 1;
		else
			raise Data_Error;
		end if;

		if Is_Digit(policy_passwd(I)) then
			Get(policy_passwd(I .. Last), policy.Max, I);
			I := I + 1;
		else
			raise Data_Error;
		end if;
		--Put(policy.Max); New_Line;

		if I <= Last and then policy_passwd(I) = ' ' then
			while I <= Last and then policy_passwd(I) = ' ' loop
				I := I + 1;
			end loop;
		else
			raise Data_Error;
		end if;
		if I <= Last then
			policy.Letter := policy_passwd(I);
			I := I + 1;
		else
			raise Data_Error;
		end if;
		--Put_Line(Character'Image(policy.Letter));

		if I + 2 <= Last and then policy_passwd(I .. I + 1) = ": " then
			passwd_len := Last - (I + 2) + 1;
			passwd(passwd'First .. passwd_len) := policy_passwd(I + 2 .. Last);
		else
			raise Data_Error;
		end if;
		--Put_Line(passwd(passwd'First .. passwd_len));
	end;
end Password_Policy;

package Password_Policy is
	type Password_Policy_Type is record
		Min, Max: Natural;
		Letter: Character;
	end record;

	procedure parse_policy_passwd(policy_passwd: String; policy: out Password_Policy_Type; passwd: out String; passwd_len: out Natural);
end Password_Policy;

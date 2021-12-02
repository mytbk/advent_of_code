package Commands is
	type Operation is (forward, up, down);
	type Command_Type is record
		op: Operation;
		x: Integer;
	end record;

	function parse_command(s: String) return Command_Type;
end Commands;

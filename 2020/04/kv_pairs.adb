package body kv_pairs is
	function get_key(kvstr: String) return String is
	begin
		for I in kvstr'Range loop
			if kvstr(I) = ':' then
				return kvstr(kvstr'First .. I - 1);
			end if;
		end loop;
		return "";
	end get_key;

	function get_value(kvstr: String) return String is
	begin
		for I in kvstr'Range loop
			if kvstr(I) = ':' and I < kvstr'Last then
				return kvstr(I + 1 .. kvstr'Last);
			end if;
		end loop;
		return "";
	end get_value;

	function get_non_spaced_string(str: String; lastPos: out Positive) return String is
	begin
		for I in str'Range loop
			if str(I) = ' ' then
				lastPos := I - 1;
				return str(str'First .. I - 1);
			end if;
		end loop;
		lastPos := str'Last;
		return str;
	end get_non_spaced_string;
end kv_pairs;

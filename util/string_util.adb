package body String_Util is
	function Split_String(src: String; delim: Character) return StringVec is
		vec: StringVec;
		start: Positive := src'First;
	begin
		for idx in src'Range loop
			if src(idx) = delim then
				if idx > start then
					vec.Append(To_Unbounded_String(src(start .. idx - 1)));
				else
					vec.Append(To_Unbounded_String(""));
				end if;
				start := idx + 1;
			end if;
		end loop;
		if start <= src'Last then
			vec.Append(To_Unbounded_String(src(start .. src'Last)));
		else
			vec.Append(To_Unbounded_String(""));
		end if;
		return vec;
	end Split_String;
end String_Util;

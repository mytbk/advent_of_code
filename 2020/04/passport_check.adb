with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

package body passport_check is
	function Is_Valid_Num_String(s: String; Len: Positive) return Boolean is
	begin
		if s'Length /= Len then
			return False;
		end if;

		for I in s'Range loop
			if s(I) >= '0' and s(I) <= '9' then
				null;
			else
				return False;
			end if;
		end loop;
		return True;
	end Is_Valid_Num_String;

	function Is_Valid_Hex_String(s: String; Len: Positive) return Boolean is
	begin
		if s'Length /= Len then
			return False;
		end if;

		for I in s'Range loop
			if s(I) >= '0' and s(I) <= '9' then
				null;
			elsif s(I) >= 'a' and s(I) <= 'f' then
				null;
			else
				return False;
			end if;
		end loop;
		return True;
	end Is_Valid_Hex_String;

	function Is_Valid_Birth_Year(s: String) return Boolean is
		y: Integer;
		Unused_Last: Positive;
	begin
		if not Is_Valid_Year(s) then
			return False;
		end if;

		Get(s, y, Unused_Last);
		return y >= 1920 and then y <= 2002;
	end Is_Valid_Birth_Year;

	function Is_Valid_Issue_Year(s: String) return Boolean is
		y: Integer;
		Unused_Last: Positive;
	begin
		if not Is_Valid_Year(s) then
			return False;
		end if;

		Get(s, y, Unused_Last);
		return y >= 2010 and then y <= 2020;
	end Is_Valid_Issue_Year;

	function Is_Valid_Expiration_Year(s: String) return Boolean is
		y: Integer;
		Unused_Last: Positive;
	begin
		if not Is_Valid_Year(s) then
			return False;
		end if;

		Get(s, y, Unused_Last);
		return y >= 2020 and then y <= 2030;
	end Is_Valid_Expiration_Year;

	function Is_Valid_Height(s: String) return Boolean is
		h: Integer;
		Unused_Last: Positive;
	begin
		if s'Length = 5 then
			if Is_Valid_Num_String(s(s'First .. s'First + 2), 3) and then s(s'First + 3 .. s'First + 4) = "cm" then
				Get(s(s'First .. s'First + 2), h, Unused_Last);
				return h >= 150 and then h <= 193;
			else
				return False;
			end if;
		elsif s'Length = 4 then
			if Is_Valid_Num_String(s(s'First .. s'First + 1), 2) and then s(s'First + 2 .. s'First + 3) = "in" then
				Get(s(s'First .. s'First + 1), h, Unused_Last);
				return h >= 59 and then h <= 76;
			else
				return False;
			end if;
		else
			return False;
		end if;
	end Is_Valid_Height;

	function Is_Valid_Hair_Color(s: String) return Boolean is
	begin
		return s'Length = 7 and then s(s'First) = '#' and then Is_Valid_Hex_String(s(s'First + 1 .. s'Last), 6);
	end Is_Valid_Hair_Color;

	function Is_Valid_Eye_Color(s: String) return Boolean is
		subtype Color_String is String(1 .. 3);
		type Color_Array_Index is range 1 .. 7;
		type Color_Array is array (Color_Array_Index) of Color_String;
		colors : constant Color_Array := ("amb", "blu", "brn", "gry", "grn", "hzl", "oth");
	begin
		for I in colors'Range loop
			if s = colors(I) then
				return True;
			end if;
		end loop;

		return False;
	end Is_Valid_Eye_Color;

	function Is_Valid_Passport_ID(s: String) return Boolean is
	begin
		return Is_Valid_Num_String(s, 9);
	end Is_Valid_Passport_ID;
end passport_check;

with String_Util;
use String_Util;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Text_IO;
use Ada.Text_IO;

procedure Test_String_Util is
	s1: String := "1 2 3 4 5";
	s2: String := "/usr/bin/ls";
	s3: String := "foo bar";
begin
	for s of Split_String(s1, ' ') loop
		Put_Line(To_String(s));
	end loop;

	for s of Split_String(s2, '/') loop
		Put_Line("""" & To_String(s) & """");
	end loop;

	for s of Split_String(s3, '/') loop
		Put_Line("""" & To_String(s) & """");
	end loop;
end Test_String_Util;

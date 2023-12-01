with Ada.Text_Io;
use Ada.Text_Io;

procedure Advent_01_2 is
   Sum : Natural := 0;
   
   type String_Access is access String;
   Letter_Strings : constant array (1 .. 9) of String_Access :=
     (new String'("one"), new String'("two"), new String'("three"),
      new String'("four"), new String'("five"), new String'("six"),
      new String'("seven"), new String'("eight"), new String'("nine"));
begin
   while not End_Of_File loop
      declare
	 S : constant String := Get_Line;
	 C : Character;
	 Has_Digit : Boolean := False;
	 Num_Str : String(1 .. 2);
	 Number : Natural;
      begin
	 for I in S'Range loop
	    C := S(I);
	    if C >= '0' and C <= '9' then
	       if not Has_Digit then
		  Num_Str(1) := C;
		  Has_Digit := True;
	       end if;
	       Num_Str(2) := C;
	    end if;
	    for J in Letter_Strings'Range loop
	       declare
		  Letter_String : constant String := Letter_Strings(J).all;
		  Len : constant Positive := Letter_String'Length;
		  Char_Digit : Character := Character'Val(Character'Pos('0') + J);
	       begin
		  if I + Len - 1 <= S'Last and then
		    S(I .. I + Len - 1) = Letter_String then
		     if not Has_Digit then
			Num_Str(1) := Char_Digit;
			Has_Digit := True;
		     end if;
		     Num_Str(2) := Char_Digit;
		     exit;
		  end if;
	       end;
	    end loop;
	 end loop;
	 Number := Natural'Value(Num_Str);
	 -- Put_Line(Num_Str);
	 Sum := Sum + Number;
      end;
   end loop;
   Put_Line(Natural'Image(Sum));
end Advent_01_2;

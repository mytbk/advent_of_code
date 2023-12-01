with Ada.Text_Io;
use Ada.Text_Io;

procedure Advent_01_1 is
   Sum : Natural := 0;
begin
   while not End_Of_File loop
      declare
	 S : constant String := Get_Line;
	 Has_Digit : Boolean := False;
	 Num_Str : String(1 .. 2);
	 Number : Natural;
      begin
	 for C of S loop
	    if C >= '0' and C <= '9' then
	       if not Has_Digit then
		  Num_Str(1) := C;
		  Has_Digit := True;
	       end if;
	       Num_Str(2) := C;
	    end if;
	 end loop;
	 Number := Natural'Value(Num_Str);
	 Sum := Sum + Number;
      end;
   end loop;
   Put_Line(Natural'Image(Sum));
end Advent_01_1;

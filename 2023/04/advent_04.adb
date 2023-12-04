with String_Util; use String_Util;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Advent_04 is
   type Winning_Card_Set is array (0 .. 99) of Boolean;
   
   -- my input has 220 cards
   type Card_Number_Array is array (1 .. 220) of Natural;
   Num_Cards : Natural := 0;
   Card_Numbers : Card_Number_Array := (others => 0);
   
   function Parse_Card(S : String) return Natural is
      Splited : constant Stringvec := Split_String(S, '|');
      Found : Boolean;
      Last : Positive;
      Num : Integer;
      Left : constant String := To_String(Splited(1));
      Right : constant String := To_String(Splited(2));
      
      Winning_Numbers : Winning_Card_Set := (others => False);
      Num_Winning : Natural := 0;
   begin
      -- eat game id
      Find_Number(Left, Num, Found, Last);
      
      -- get the winning cards
      Last := Last + 1;
      while Last < Left'Last loop
	 Find_Number(Left(Last .. Left'Last), Num, Found, Last);
	 exit when not Found;
	 Winning_Numbers(Num) := True;
	 Last := Last + 1;
      end loop;
      
      -- get the numbers you have
      Last := 1;
      while Last < Right'Last loop
	 Find_Number(Right(Last .. Right'Last), Num, Found, Last);
	 exit when not Found;
	 if Winning_Numbers(Num) then
	    Num_Winning := Num_Winning + 1;
	 end if;
	 Last := Last + 1;
      end loop;
      
      return Num_Winning;
   end Parse_Card;
   
   Sum_Points : Natural := 0;
   Total_Number_Of_Cards : Natural := 0;
begin
   while not End_Of_File loop
      declare
	 S : constant String := Get_Line;
	 Num_Winning_Numbers : Natural := Parse_Card(S);
      begin
	 if Num_Winning_Numbers > 0 then
	    Sum_Points := Sum_Points + 2 ** (Num_Winning_Numbers - 1);
	 end if;
	 
	 Num_Cards := Num_Cards + 1;
	 Card_Numbers(Num_Cards) := Card_Numbers(Num_Cards) + 1;
	 for I in 1 .. Num_Winning_Numbers loop
	    Card_Numbers(Num_Cards + I) := Card_Numbers(Num_Cards + I) + Card_Numbers(Num_Cards);
	 end loop;
      end;
   end loop;
   
   -- part 1
   Put_Line(Natural'Image(Sum_Points));
   
   -- part 2
   for I in 1 .. Num_Cards loop
      Total_Number_Of_Cards := Total_Number_Of_Cards + Card_Numbers(I);
   end loop;
   Put_Line(Natural'Image(Total_Number_Of_Cards));
end Advent_04;

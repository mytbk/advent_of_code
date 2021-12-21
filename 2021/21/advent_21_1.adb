with Ada.Text_Io; use Ada.Text_Io;

procedure Advent_21_1 is
   subtype Game_Position is Positive range 1 .. 10;
   type Player_Id is mod 2;
   
   type Player_Info is record
      Position : Game_Position;
      Score : Natural;
   end record;
   
   Deterministic_Die : Positive := 1;
   Roll_Times: Natural := 0;
   
   -- my input
   Turn : Player_Id := 0;
   Players : array (Player_Id) of Player_Info := ((9, 0), (4, 0));

begin
   -- part one
   loop
      exit when Players(0).Score >= 1000 or Players(1).Score >= 1000;
      declare
	 New_Pos : Natural := (Players(Turn).Position + Deterministic_Die * 3 + 3) mod Game_Position'Last;
      begin
	 if New_Pos = 0 then
	    Players(Turn).Position := 10;
	 else
	    Players(Turn).Position := New_Pos;
	 end if;
      end;
      Players(Turn).Score := Players(Turn).Score + Players(Turn).Position;
      -- next turn
      Deterministic_Die := Deterministic_Die + 3;
      Roll_Times := Roll_Times + 3;
      Turn := Turn + 1;
   end loop;
   
   declare
      Losing_Score: Natural;
   begin
      if Players(0).Score < 1000 then
	 Losing_Score := Players(0).Score;
      else
	 Losing_Score := Players(1).Score;
      end if;
      
      Put_Line(Natural'Image(Losing_Score));
      Put_Line(Natural'Image(Roll_Times));
      Put_Line(Natural'Image(Losing_Score * Roll_Times));
   end;
end Advent_21_1;

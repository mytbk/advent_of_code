with Ada.Text_Io; use Ada.Text_Io;

procedure Advent_10 is
   X_Values : array (1 .. 241) of Integer := (1 => 1, others => <>);
   Cycle : Positive := 1;
   
   Sum_1 : Integer := 0;
begin
   while Cycle < 240 loop
      declare
	 S : constant String := Get_Line;
	 V : Integer;
      begin
	 if S(1 .. 4) = "noop" then
	    X_Values(Cycle + 1) := X_Values(Cycle);
	    Cycle := Cycle + 1;
	 elsif S(1 .. 4) = "addx" then
	    V := Integer'Value(S(6 .. S'Last));
	    X_Values(Cycle + 1) := X_Values(Cycle);
	    X_Values(Cycle + 2) := X_Values(Cycle) + V;
	    Cycle := Cycle + 2;
	 end if;
      end;
   end loop;
   
   declare
      Acc_Cycle : Positive := 20;
   begin
      while Acc_Cycle <= 220 loop
	 Sum_1 := Sum_1 + Acc_Cycle * X_Values(Acc_Cycle);
	 Acc_Cycle := Acc_Cycle + 40;
      end loop;
   end;
   
   Put_Line(Integer'Image(Sum_1));
   
   for I in 1 .. 240 loop
      if abs(((I - 1) mod 40) - X_Values(I)) <= 1 then
	 Put('#');
      else
	 Put('.');
      end if;
      if I mod 40 = 0 then
	 New_Line;
      end if;
   end loop;
   
end Advent_10;

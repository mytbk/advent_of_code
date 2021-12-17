with Ada.Text_Io; use Ada.Text_Io;

procedure Advent_17 is
   function Valid_Initial_Velocity(Vx0, Vy0, Xmin, Xmax, Ymin, Ymax: Integer) return Boolean is
      X,Y : Integer := 0;
      Vx : Integer := Vx0;
      Vy : Integer := Vy0;
   begin
      -- we just simulate it
      loop
	 if X >= Xmin and then X <= Xmax and then Y >= Ymin and then Y <= Ymax then
	    return True;
	 end if;
	 X := X + Vx;
	 Y := Y + Vy;
	 if Vx > 0 then
	    if X > Xmax then
	       return False;
	    end if;
	    Vx := Vx - 1;
	 elsif Vx < 0 then
	    if X < Xmin then
	       return False;
	    end if;
	    Vx := Vx + 1;
	 else
	    if X < Xmin or else X > Xmax then
	       return False;
	    end if;
	 end if;

	 Vy := Vy - 1;
	 if Vy < 0 and then Y < Ymin then
	    return False;
	 end if;
      end loop;
   end Valid_Initial_Velocity;
   
   -- my input, this can change for other people
   Xmin : constant := 119;
   Xmax : constant := 176;
   Ymin : constant := -141;
   Ymax : constant := -84;
   
   Count : Natural := 0;
begin
   -- for part 1, when Ymin and Ymax are both negative, and Vy0 > 0,
   -- the Y position first go up and the go down, Vy goes from Vy0 to 0
   -- at the highest place, and to -Vy0 when go back to Y=0, the next
   -- Vy at Y=0 is -(Vy0 + 1), so we need to make -(Vy0 + 1) >= Ymin,
   -- i.e. Vy0 <= Ymin - 1
   Put_Line(Natural'Image((abs(Ymin) - 1) * abs(Ymin) / 2));
   
   -- for part 2, iterate all the possible Vx0,Vy0 and simulate them
   for Vx0 in 1 .. Integer'Max(abs(Xmin), abs(Xmax)) loop
      for Vy0 in -Integer'Max(abs(Ymin),abs(Ymax)) .. Integer'Max(abs(Ymin),abs(Ymax)) loop
	 if Valid_Initial_Velocity(Vx0, Vy0, Xmin, Xmax, Ymin, Ymax) then
	    Count := Count + 1;
	 end if;
      end loop;
   end loop;
   Put_Line(Natural'Image(Count));
end Advent_17;

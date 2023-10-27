with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Sets;

procedure Advent_09_2 is
   type Grid_Point is record
      X, Y : Integer;
   end record;
   
   function Hash(P : Grid_Point) return Hash_Type is
   begin
      return Hash_Type'Mod(P.X) * Hash_Type'Mod(P.X) + Hash_Type'Mod(P.Y);
   end Hash;
   
   package Grid_Point_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Grid_Point, Hash => Hash, Equivalent_Elements => "=",
      "=" => "=");
   
   subtype Grid_Point_Set is Grid_Point_Sets.Set;
   
   Visited_Set : Grid_Point_Set;
   
   Points : array (0 .. 9) of Grid_Point :=
     (others => (0, 0));
   
   function Is_Touching(Head_Point, Tail_Point : Grid_Point) return Boolean is
   begin
      return abs(Head_Point.X - Tail_Point.X) <= 1 and
	abs(Head_Point.Y - Tail_Point.Y) <= 1;
   end Is_Touching;
   
   procedure Move_Tail(Tail_Point : in out Grid_Point; Head_Point : Grid_Point) is
   begin
      if Is_Touching(Head_Point, Tail_Point) then
	 return;
      end if;

      if Tail_Point.X > Head_Point.X then
	 Tail_Point.X := Tail_Point.X - 1;
      elsif Tail_Point.X < Head_Point.X then
	 Tail_Point.X := Tail_Point.X + 1;
      end if;

      if Tail_Point.Y > Head_Point.Y then
	 Tail_Point.Y := Tail_Point.Y - 1;
      elsif Tail_Point.Y < Head_Point.Y then
	 Tail_Point.Y := Tail_Point.Y + 1;
      end if;
   end Move_Tail;

   procedure Move_Left(Head_Point : in out Grid_Point) is
   begin
      Head_Point.X := Head_Point.X - 1;
   end Move_Left;
   
   procedure Move_Right(Head_Point : in out Grid_Point) is
   begin
      Head_Point.X := Head_Point.X + 1;
   end Move_Right;

   procedure Move_Up(Head_Point : in out Grid_Point) is
   begin
      Head_Point.Y := Head_Point.Y + 1;
   end Move_Up;
   
   procedure Move_Down(Head_Point : in out Grid_Point) is
   begin
      Head_Point.Y := Head_Point.Y - 1;
   end Move_Down;
begin
   Visited_Set.Insert(Points(9));
   
   while not End_Of_File loop
      declare
	 Direction: Character;
	 Distance : Integer;
      begin
	 Get(Direction);
	 Get(Distance);
	 
	 if Direction = 'L' then
	    for I in 1 .. Distance loop
	       Move_Left(Points(0));
	       for J in 1 .. 9 loop
		  Move_Tail(Points(J), Points(J - 1));
	       end loop;
	       if not Visited_Set.Contains(Points(9)) then
		  Visited_Set.Insert(Points(9));
	       end if;
	    end loop;
	 elsif Direction = 'R' then
	    for I in 1 .. Distance loop
	       Move_Right(Points(0));
	       for J in 1 .. 9 loop
		  Move_Tail(Points(J), Points(J - 1));
	       end loop;
	       if not Visited_Set.Contains(Points(9)) then
		  Visited_Set.Insert(Points(9));
	       end if;
	    end loop;
	 elsif Direction = 'U' then
	    for I in 1 .. Distance loop
	       Move_Up(Points(0));
	       for J in 1 .. 9 loop
		  Move_Tail(Points(J), Points(J - 1));
	       end loop;
	       if not Visited_Set.Contains(Points(9)) then
		  Visited_Set.Insert(Points(9));
	       end if;
	    end loop;
	 elsif Direction = 'D' then
	    for I in 1 .. Distance loop
	       Move_Down(Points(0));
	       for J in 1 .. 9 loop
		  Move_Tail(Points(J), Points(J - 1));
	       end loop;
	       if not Visited_Set.Contains(Points(9)) then
		  Visited_Set.Insert(Points(9));
	       end if;
	    end loop;
	 end if;
      end;
   end loop;
   
   Put_Line(Count_Type'Image(Visited_Set.Length));
end Advent_09_2;

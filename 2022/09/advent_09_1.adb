with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Sets;

procedure Advent_09_1 is
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
   
   Head_Point : Grid_Point := (0, 0);
   Tail_Point : Grid_Point := (0, 0);
   
   function Is_Touching return Boolean is
   begin
      return abs(Head_Point.X - Tail_Point.X) <= 1 and
	abs(Head_Point.Y - Tail_Point.Y) <= 1;
   end Is_Touching;
   
   procedure Move_Tail is
   begin
      if Is_Touching then
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

      if not Visited_Set.Contains(Tail_Point) then
	 Visited_Set.Insert(Tail_Point);
      end if;
   end Move_Tail;

   procedure Move_Left(Distance : Natural) is
   begin
      for I in 1 .. Distance loop
	 Head_Point.X := Head_Point.X - 1;
	 Move_Tail;
      end loop;
   end Move_Left;
   
   procedure Move_Right(Distance : Natural) is
   begin
      for I in 1 .. Distance loop
	 Head_Point.X := Head_Point.X + 1;
	 Move_Tail;
      end loop;
   end Move_Right;

   procedure Move_Up(Distance : Natural) is
   begin
      for I in 1 .. Distance loop
	 Head_Point.Y := Head_Point.Y + 1;
	 Move_Tail;
      end loop;
   end Move_Up;
   
   procedure Move_Down(Distance : Natural) is
   begin
      for I in 1 .. Distance loop
	 Head_Point.Y := Head_Point.Y - 1;
	 Move_Tail;
      end loop;
   end Move_Down;
begin
   Visited_Set.Insert(Tail_Point);
   
   while not End_Of_File loop
      declare
	 Direction: Character;
	 Distance : Integer;
      begin
	 Get(Direction);
	 Get(Distance);
	 
	 if Direction = 'L' then
	    Move_Left(Distance);
	 elsif Direction = 'R' then
	    Move_Right(Distance);
	 elsif Direction = 'U' then
	    Move_Up(Distance);
	 elsif Direction = 'D' then
	    Move_Down(Distance);
	 end if;
      end;
   end loop;
   
   Put_Line(Count_Type'Image(Visited_Set.Length));
end Advent_09_1;

with Ada.Containers.Vectors;
with Ada.Text_Io; use Ada.Text_Io;
with String_Util; use String_Util;

procedure Advent_18 is
   type Lava_Cube is record
      X, Y, Z : Integer;
      Num_Adjacent : Natural;
   end record;
   
   package Lava_Vectors is new Ada.Containers.Vectors
     (Element_Type => Lava_Cube, Index_Type => Positive);
   
   subtype Lava_Vector is Lava_Vectors.Vector;
   
   Lava_Cubes : Lava_Vector;
   
   -- part 2: use flood fill
   type Space_Element is (Unknown, Steam, Lava);
   type Space_State is array
     (Integer range <>, Integer range <>, Integer range <>) of Space_Element;
   
   type Point_Offset is array (1 .. 3) of Integer;
   Adjacent_Offsets : constant array (1 .. 6) of Point_Offset :=
     ((-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1));

   procedure Flood_Fill(Space : in out Space_State; X, Y, Z : Integer) is
   begin
      for I in 1 .. 6 loop
	 declare
	    P : constant Point_Offset := Adjacent_Offsets(I);
	    X1 : constant Integer := X + P(1);
	    Y1 : constant Integer := Y + P(2);
	    Z1 : constant Integer := Z + P(3);
	 begin
	    if Space(X1, Y1, Z1) = Unknown then
	       Space(X1, Y1, Z1) := Steam;
	       Flood_Fill(Space, X1, Y1, Z1);
	    end if;
	 end;
      end loop;
   end Flood_Fill;
   
   X_Min, X_Max, Y_Min, Y_Max, Z_Min, Z_Max : Integer;
begin
   while not End_Of_File loop
      declare
	 S : constant String := Get_Line;
	 Last : Positive;
	 Cube : Lava_Cube;
	 Found : Boolean;
      begin
	 Find_Number(S, Cube.X, Found, Last);
	 Find_Number(S(Last + 1 .. S'Last), Cube.Y, Found, Last);
	 Find_Number(S(Last + 1 .. S'Last), Cube.Z, Found, Last);
	 Cube.Num_Adjacent := 0;
	 Lava_Cubes.Append(Cube);
      end;
   end loop;
   
   for I in Lava_Cubes.First_Index + 1 .. Lava_Cubes.Last_Index loop
      for J in Lava_Cubes.First_Index .. I - 1 loop
	 declare
	    C1 : Lava_Cube renames Lava_Cubes(I);
	    C2 : Lava_Cube renames Lava_Cubes(J);
	 begin
	    if abs(C1.X - C2.X) + abs (C1.Y - C2.Y) + abs(C1.Z - C2.Z) = 1 then
	       C1.Num_Adjacent := C1.Num_Adjacent + 1;
	       C2.Num_Adjacent := C2.Num_Adjacent + 1;
	    end if;
	 end;
      end loop;
   end loop;
   
   -- part 1
   declare
      Area : Natural := 0;
   begin
      for Cube of Lava_Cubes loop
	 Area := Area + 6 - Cube.Num_Adjacent;
      end loop;
      Put_Line(Natural'Image(Area));
   end;
   
   -- part 2
   X_Min := Lava_Cubes(1).X;
   X_Max := Lava_Cubes(1).X;
   Y_Min := Lava_Cubes(1).Y;
   Y_Max := Lava_Cubes(1).Y;
   Z_Min := Lava_Cubes(1).Z;
   Z_Max := Lava_Cubes(1).Z;
   for Cube of Lava_Cubes loop
      X_Min := Integer'Min(X_Min, Cube.X);
      X_Max := Integer'Max(X_Max, Cube.X);
      Y_Min := Integer'Min(Y_Min, Cube.Y);
      Y_Max := Integer'Max(Y_Max, Cube.Y);
      Z_Min := Integer'Min(Z_Min, Cube.Z);
      Z_Max := Integer'Max(Z_Max, Cube.Z);
   end loop;
   
   declare
      Space : Space_State
	(X_Min - 2 .. X_Max + 2, Y_Min - 2 .. Y_Max + 2, Z_Min - 2 .. Z_Max + 2);
      Exterior_Surface : Natural := 0;
   begin
      for X in Space'Range(1) loop
	 for Y in Space'Range(2) loop
	    for Z in Space'Range(3) loop
	       if X < X_Min or X > X_Max or Y < Y_Min or Y > Y_Max or
		 Z < Z_Min or Z > Z_Max then
		  Space(X, Y, Z) := Steam;
	       else
		  Space(X, Y, Z) := Unknown;
	       end if;
	    end loop;
	 end loop;
      end loop;

      for Cube of Lava_Cubes loop
	 Space(Cube.X, Cube.Y, Cube.Z) := Lava;
      end loop;

      for X in X_Min - 1 .. X_Max + 1 loop
	 for Y in Y_Min - 1 .. Y_Max + 1 loop
	    for Z in Z_Min - 1 .. Z_Max + 1 loop
	       if Space(X, Y, Z) = Steam then
		  Flood_Fill(Space, X, Y, Z);
	       end if;
	    end loop;
	 end loop;
      end loop;
      
      for Cube of Lava_Cubes loop
	 if Space(Cube.X - 1, Cube.Y, Cube.Z) = Steam then
	    Exterior_Surface := Exterior_Surface + 1;
	 end if;
	 if Space(Cube.X + 1, Cube.Y, Cube.Z) = Steam then
	    Exterior_Surface := Exterior_Surface + 1;
	 end if;
	 if Space(Cube.X, Cube.Y - 1, Cube.Z) = Steam then
	    Exterior_Surface := Exterior_Surface + 1;
	 end if;
	 if Space(Cube.X, Cube.Y + 1, Cube.Z) = Steam then
	    Exterior_Surface := Exterior_Surface + 1;
	 end if;
	 if Space(Cube.X, Cube.Y, Cube.Z - 1) = Steam then
	    Exterior_Surface := Exterior_Surface + 1;
	 end if;
	 if Space(Cube.X, Cube.Y, Cube.Z + 1) = Steam then
	    Exterior_Surface := Exterior_Surface + 1;
	 end if;
      end loop;
      Put_Line(Natural'Image(Exterior_Surface));
   end;
end Advent_18;

with Ada.Text_Io; use Ada.Text_Io;

procedure Advent_17_1 is
   type Element is (Rock, Empty_Space);
   type Rock_Row is array (1 .. 7) of Element;
   
   type Position is record
     X, Y : Natural;
   end record;
   
   type Rock_Position_Array is array (1 .. 5) of Position;
   
   type Rock_Shape is record
      Num_Rocks : Positive;
      Pos : Rock_Position_Array;
   end record;
   
   type Shape_Id is mod 5;
   All_Rock_Shapes : constant array (Shape_Id) of Rock_Shape :=
     ((4, ((0, 0), (1, 0), (2, 0), (3, 0), others => <>)),
      (5, ((1, 0), (0, 1), (1, 1), (2, 1), (1, 2))),
      (5, ((0, 0), (1, 0), (2, 0), (2, 1), (2, 2))),
      (4, ((0, 0), (0, 1), (0, 2), (0, 3), others => <>)),
      (4, ((0, 0), (1, 0), (0, 1), (1, 1), others => <>)));
   
   -- 10000 rows should be enough
   type Cave_Elements is array (0 .. 10000) of Rock_Row;
   type Cave_State is record
      Elements : Cave_Elements := (0 => (others => Rock), -- floor
				   others => (others => Empty_Space));
      Max_Rock_Height : Natural := 0;
   end record;
   
   -- Time starts from 0
   procedure Simulate_One_Rock (Cave : in out Cave_State; Time : in out Natural;
				Shape : Shape_Id; Jet_Pattern : String) is
      Jet_Idx : Positive;
      Rock_X : Positive := 3;
      Rock_Y : Natural := Cave.Max_Rock_Height + 4;
      
      Current_Shape : constant Rock_Shape := All_Rock_Shapes(Shape);
      
      function Try(X : Positive; Y : Natural) return Boolean is
      begin
	 for I in 1 .. Current_Shape.Num_Rocks loop
	    declare
	       Shape_X : Positive := X + Current_Shape.Pos(I).X;
	       Shape_Y : Natural := Y + Current_Shape.Pos(I).Y;
	    begin
	       if Shape_X > Rock_Row'Last then
		  return False;
	       end if;

	       if Cave.Elements (Shape_Y)(Shape_X) = Rock then
		  return False;
	       end if;
	    end;
	 end loop;
	 return True;
      end Try;
      
      procedure Fill(X : Positive; Y : Natural) is
      begin
	 for I in 1 .. Current_Shape.Num_Rocks loop
	    Cave.Elements
	      (Y + Current_Shape.Pos(I).Y)
	      (X + Current_Shape.Pos(I).X) := Rock;
	    Cave.Max_Rock_Height := Natural'Max
	      (Cave.Max_Rock_Height, Y + Current_Shape.Pos(I).Y);
	 end loop;
      end Fill;	 
      
      Landed : Boolean := False;
   begin
      while not Landed loop
	 -- pushed by hot gas
	 Jet_Idx := Time mod Jet_Pattern'Length + 1;
	 if Jet_Pattern(Jet_Idx) = '<' then
	    if Rock_X /= Rock_Row'First then
	       if Try(Rock_X - 1, Rock_Y) then
		  Rock_X := Rock_X - 1;
	       end if;
	    end if;
	 else
	    if Rock_X /= Rock_Row'Last then
	       if Try(Rock_X + 1, Rock_Y) then
		  Rock_X := Rock_X + 1;
	       end if;
	    end if;
	 end if;
	 
	 -- fall one unit
	 if Try(Rock_X, Rock_Y - 1) then
	    Rock_Y := Rock_Y - 1;
	 else
	    Landed := True;
	    Fill(Rock_X, Rock_Y);	    
	 end if;
	 
	 Time := Time + 1;
      end loop;
   end Simulate_One_Rock;
   
   Cave : Cave_State;
   Current_Time : Natural := 0;
   Current_Shape_Id : Shape_Id := 0;
   Jet_Pattern : constant String := Get_Line;
begin
   for I in 1 .. 2022 loop
      Simulate_One_Rock(Cave, Current_Time, Current_Shape_Id, Jet_Pattern);
      Current_Shape_Id := Current_Shape_Id + 1;
   end loop;
   Put_Line(Natural'Image(Cave.Max_Rock_Height));
   
   Put_Line("current time is " & Natural'Image(Current_Time));
   for I in reverse 1 .. Cave.Max_Rock_Height loop
      for Elem of Cave.Elements(I) loop
	 if Elem = Rock then
	    Put('#');
	 else
	    Put('.');
	 end if;
      end loop;
      New_Line;
   end loop;
end Advent_17_1;

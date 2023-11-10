with Ada.Text_Io; use Ada.Text_Io;

procedure Advent_14 is
   type X_Index is range 0 .. 1000;
   type Y_Index is range 0 .. 1000;
   
   type State is (Air, Rock, Sand);
   
   Max_Depth : Y_Index := 0;
   
   Grids : array (X_Index, Y_Index) of State := (others => (others => Air));
   
   Answer_1 : Natural := 0;
   Answer_2 : Natural := 0;

   procedure Read_Trace is
      S : constant String := Get_Line;
      Last_X : X_Index;
      Last_Y : Y_Index;
      Current_X : X_Index;
      Current_Y : Y_Index;
      Pos : Positive := S'First;
      
      procedure Read_Point is
	 Scan_Pos : Positive := Pos;
      begin
	 while S(Scan_Pos) /= ',' loop
	    Scan_Pos := Scan_Pos + 1;
	 end loop;
	 Current_X := X_Index'Value(S(Pos .. Scan_Pos - 1));
	 Pos := Scan_Pos + 1;
	 Scan_Pos := Pos;
	 while Scan_Pos <= S'Last and then S(Scan_Pos) /= ' ' loop
	    Scan_Pos := Scan_Pos + 1;
	 end loop;
	 Current_Y := Y_Index'Value(S(Pos .. Scan_Pos - 1));
	 Pos := Scan_Pos;
      end Read_Point;
      
      procedure Skip is
      begin
	 while Pos < S'Last loop
	    if S(Pos .. Pos + 1) = "->" then
	       Pos := Pos + 2;
	    elsif S(Pos) = ' ' then
	       Pos := Pos + 1;
	    else
	       exit;
	    end if;
	 end loop;
      end Skip;
   begin
      -- read first point
      Read_Point;
      Last_X := Current_X;
      Last_Y := Current_Y;
      
      while Pos <= S'Last loop
	 Skip;
	 Read_Point;
	 if Current_X = Last_X then
	    for I in Last_Y .. Current_Y loop
	       Grids(Current_X, I) := Rock;
	    end loop;

	    for I in Current_Y .. Last_Y loop
	       Grids(Current_X, I) := Rock;
	    end loop;
	 end if;
	 
	 if Current_Y = Last_Y then
	    for I in Last_X .. Current_X loop
	       Grids(I, Current_Y) := Rock;
	    end loop;
	    
	    for I in Current_X .. Last_X loop
	       Grids(I, Current_Y) := Rock;
	    end loop;
	 end if;

	 Max_Depth := Y_Index'Max(Max_Depth, Y_Index'Max(Last_Y, Current_Y));

	 Last_X := Current_X;
	 Last_Y := Current_Y;
      end loop;
   end Read_Trace;
   
   function Put_Sand return Boolean is
      Sand_X : X_Index := 500;
      Sand_Y : Y_Index := 0;
   begin
      if Grids(Sand_X, Sand_Y) /= Air then
	 return False;
      end if;

      while Sand_Y < Max_Depth loop
	 if Grids(Sand_X, Sand_Y + 1) = Air then
	    Sand_Y := Sand_Y + 1;
	 elsif Grids(Sand_X - 1, Sand_Y + 1) = Air then
	    Sand_X := Sand_X - 1;
	    Sand_Y := Sand_Y + 1;
	 elsif Grids(Sand_X + 1, Sand_Y + 1) = Air then
	    Sand_X := Sand_X + 1;
	    Sand_Y := Sand_Y + 1;
	 else
	    -- come to rest
	    Grids(Sand_X, Sand_Y) := Sand;
	    return True;
	 end if;
      end loop;
      
      -- too deep
      return False;
   end Put_Sand;
begin
   while not End_Of_File loop
      Read_Trace;
   end loop;
   
   -- for I in Y_Index(0) .. Y_Index(9) loop
   --    for J in X_Index(494) .. X_Index(503) loop
   -- 	 case Grids(J, I) is
   -- 	    when Air => Put('.');
   -- 	    when Rock => Put('#');
   -- 	    when Sand => Put('o');
   -- 	 end case;
   --    end loop;
   --    New_Line;
   -- end loop;
   
   -- part 1
   while Put_Sand loop
      Answer_1 := Answer_1 + 1;
   end loop;

   Put_Line(Natural'Image(Answer_1));

   -- part 2
   for I in X_Index'Range loop
      Grids(I, Max_Depth + 2) := Rock;
   end loop;
   Max_Depth := Max_Depth + 2;

   Answer_2 := Answer_1;
   while Put_Sand loop
      Answer_2 := Answer_2 + 1;
   end loop;
   Put_Line(Natural'Image(Answer_2));
end Advent_14;

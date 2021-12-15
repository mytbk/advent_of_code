with Ada.Text_Io; use Ada.Text_Io;

procedure Advent_15 is
   type Row_Type is new Positive;
   type Column_Type is new Positive;
   
   type Risk_Map is array (Row_Type range <>, Column_Type range <>) of Integer;
   function Find_Lowest_Risk(Risks: Risk_Map; Rows: Row_Type; Columns: Column_Type) return Integer is
      Sum_Risks: Risk_Map(1 .. Rows, 1 .. Columns) := (others => (others => Integer'Last));
      
      -- a dumb DFS...
      -- it takes 3s for part2 with -O3 -gnatp
      procedure Search(Row: Row_Type; Column: Column_Type) is
	 Min_Risk: Integer := Integer'Last;
      begin
	 if Row > Row_Type'First then
	    Min_Risk := Integer'Min(Min_Risk, Sum_Risks(Row - 1, Column));
	 end if;
	 if Row < Rows then
	    Min_Risk := Integer'Min(Min_Risk, Sum_Risks(Row + 1, Column));
	 end if;
	 if Column > Column_Type'First then
	    Min_Risk := Integer'Min(Min_Risk, Sum_Risks(Row, Column - 1));
	 end if;
	 if Column < Columns then
	    Min_Risk := Integer'Min(Min_Risk, Sum_Risks(Row, Column + 1));
	 end if;
	 Min_Risk := Min_Risk + Risks(Row, Column);
	 if Min_Risk < Sum_Risks(Row, Column) then
	    Sum_Risks(Row, Column) := Min_Risk;
	    if Row > Row_Type'First then
	       Search(Row - 1, Column);
	    end if;
	    if Row < Rows then
	       Search(Row + 1, Column);
	    end if;
	    if Column > Column_Type'First then
	       Search(Row, Column - 1);
	    end if;
	    if Column < Columns then
	       Search(Row, Column + 1);
	    end if;
	 end if;
      end Search;
   begin
      Sum_Risks(1,1) := 0;
      
      Search(1, 2);
      Search(2, 1);
	   
      return Sum_Risks(Rows,Columns);
   end Find_Lowest_Risk;
   
   Risks: Risk_Map(1 .. 100, 1 .. 100);
   Rows: Row_Type := 1;
   Columns: Column_Type := 1;
   
   Expanded_Risks: Risk_Map(1 .. 500, 1 .. 500);
begin
   while not End_Of_File loop
      declare
	 L : constant String := Get_Line;
      begin
	 pragma Assert(Columns > 1 or else Columns = L'Length);
	 Columns := L'Length;
	 for I in 1 .. Columns loop
	    Risks(Rows,I) := Character'Pos(L(L'First + Positive(I) - 1)) - Character'Pos('0');
	 end loop;
 	 Rows := Rows + 1;
      end;
   end loop;
   Rows := Rows - 1;
   
   -- part 1
   Put_Line(Integer'Image(Find_Lowest_Risk(Risks,Rows,Columns)));
   
   -- part 2
   for Ext_X in 0 .. 4 loop
      for Ext_Y in 0 .. 4 loop
	 for X in 1 .. Rows loop
	    for Y in 1 .. Columns loop
	       declare
		  Cur_X : Row_Type := Row_Type(Positive(Rows) * Ext_X + Positive(X));
		  Cur_Y : Column_Type := Column_Type(Positive(Columns) * Ext_Y + Positive(Y));
		  Val : Integer := Risks(X,Y) + Ext_X + Ext_Y;
	       begin
		  if Val > 9 then
		     Expanded_Risks(Cur_X, Cur_Y) := Val mod 9;
		  else
		     Expanded_Risks(Cur_X, Cur_Y) := Val;
		  end if;
	       end;
	    end loop;
	 end loop;
      end loop;
   end loop;
   Put_Line(Integer'Image(Find_Lowest_Risk(Expanded_Risks, Rows * 5, Columns * 5)));

end Advent_15;

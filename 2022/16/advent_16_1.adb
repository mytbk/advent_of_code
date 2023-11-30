with Ada.Text_Io; use Ada.Text_Io;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Advent_16_1 is
   type Valve_Id is new Natural;
   package Valve_Destination_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Valve_Id);
   subtype Valve_Destination_Vector is Valve_Destination_Vectors.Vector;

   type Valve_Info is record
      Name : Unbounded_String;
      Flow_Rate : Natural;
      Destinations : Valve_Destination_Vector;
   end record;
   
   package Valve_Vectors is new Ada.Containers.Vectors
     (Index_Type => Valve_Id, Element_Type => Valve_Info);
   subtype Valve_Vector is Valve_Vectors.Vector;
   
   Valves : Valve_Vector;
   
   procedure Read_One_Valve is
      S : constant String := Get_Line;
      Cursor : Positive := S'First;
      The_Valve_Id : Valve_Id;
   begin
      -- "Valve <name> has ..."
      Cursor := Cursor + 6;
      
      -- eat spaces
      while Cursor <= S'Last and then S(Cursor) = ' ' loop
	 Cursor := Cursor + 1;
      end loop;
      
      -- read name
      declare
	 Name_Last_Idx : Positive := Cursor;
      begin
	 while Name_Last_Idx <= S'Last and then S(Name_Last_Idx) /= ' ' loop
	    Name_Last_Idx := Name_Last_Idx + 1;
	 end loop;
	 declare
	    Name : constant String := S(Cursor .. Name_Last_Idx - 1);
	    Found : Boolean := False;
	 begin
	    for I in Valves.First_Index .. Valves.Last_Index loop
	       if To_String(Valves(I).Name) = Name then
		  Found := True;
		  The_Valve_Id := I;
		  exit;
	       end if;
	    end loop;
	    if not Found then
	       Valves.Append
		 (Valve_Info'(Name => To_Unbounded_String(Name), others => <>));
	       The_Valve_Id := Valves.Last_Index;
	    end if;
	 end;
	 Cursor := Name_Last_Idx;
      end;
      
      -- find "has flow rate=<r>;"
      while S(Cursor .. Cursor + 13) /= "has flow rate=" loop
	 Cursor := Cursor + 1;
      end loop;
      Cursor := Cursor + 14;
      declare
	 Rate_Last_Idx : Positive := Cursor;
      begin
	 while Rate_Last_Idx <= S'Last and then S(Rate_Last_Idx) /= ';' loop
	    Rate_Last_Idx := Rate_Last_Idx + 1;
	 end loop;
	 Valves(The_Valve_Id).Flow_Rate :=
	   Natural'Value(S(Cursor .. Rate_Last_Idx - 1));
	 Cursor := Rate_Last_Idx;
      end;
      
      -- find "to valve(s) "
      while S(Cursor .. Cursor + 7) /= "to valve" loop
	 Cursor := Cursor + 1;
      end loop;
      Cursor := Cursor + 8;
      if S(Cursor) = 's' then
	 Cursor := Cursor + 1;
      end if;
      while S(Cursor) = ' ' loop
	 Cursor := Cursor + 1;
      end loop;
      -- read <name>, <name> ...
      loop
	 declare
	    Name_Last_Idx : Positive := Cursor;
	 begin
	    while Name_Last_Idx <= S'Last and then S(Name_Last_Idx) /= ',' loop
	       Name_Last_Idx := Name_Last_Idx + 1;
	    end loop;
	    declare
	       Name : constant String := S(Cursor .. Name_Last_Idx - 1);
	       Found : Boolean := False;
	       Dest_Idx : Valve_Id;
	    begin
	       for I in Valves.First_Index .. Valves.Last_Index loop
		  if To_String(Valves(I).Name) = Name then
		     Dest_Idx := I;
		     Found := True;
		     exit;
		  end if;
	       end loop;
	       if not Found then
		  Valves.Append
		    (Valve_Info'(Name => To_Unbounded_String(Name), others => <>));
		  Dest_Idx := Valves.Last_Index;
	       end if;
	       Valves(The_Valve_Id).Destinations.Append(Dest_Idx);
	    end;
	    Cursor := Name_Last_Idx;
	    -- eat ", "
	    if Cursor <= S'Last then
	       Cursor := Cursor + 1;
	    end if;
	    while Cursor <= S'Last and then S(Cursor) = ' ' loop
	       Cursor := Cursor + 1;
	    end loop;
	    exit when Cursor > S'Last;
	 end;
      end loop;
   end Read_One_Valve;
   
   type Valve_Open_State is array (Valve_Id range <>) of Boolean;

   function Dfs(Current_Position : Valve_Id; Open_State : Valve_Open_State;
		Remaining_Time: Positive; Minimal_Target : Natural)
	       return Natural is
      Max_Value : Natural := 0;
      Current_Open_State : Valve_Open_State := Open_State;
   begin
      if Remaining_Time <= 1 then
	 return 0;
      end if;
      
      -- debug
      -- Put_Line("Searching at " & To_String(Valves(Current_Position).Name) &
	--	 " with " & Positive'Image(Remaining_Time) &
	--	 " minutes remaining, minimal target is " &
	--	 Natural'Image(Minimal_Target));
      
      if Minimal_Target > 0 then
	 -- estimate how much pressure can release by the remaining valves
	 declare
	    Total_Flow : Natural := 0;
	    package Flow_Rate_Vectors is new Ada.Containers.Vectors
	      (Element_Type => Natural, Index_Type => Natural);
	    package Flow_Rate_Sorting is new Flow_Rate_Vectors.Generic_Sorting
	      ("<" => ">");
	    Remain_Flows : Flow_Rate_Vectors.Vector;
	    Est_Time : Integer := Remaining_Time - 1;
	 begin
	    for I in Valves.First_Index .. Valves.Last_Index loop
	       if Current_Open_State(I) = False then
		  Remain_Flows.Append(Valves(I).Flow_Rate);
	       end if;
	    end loop;
	    
	    Flow_Rate_Sorting.Sort(Remain_Flows);
	    
	    for Flow of Remain_Flows loop
	       Total_Flow := Total_Flow + Flow * Est_Time;
	       Est_Time := Est_Time - 2;
	       exit when Est_Time <= 0;
	    end loop;

	    if Total_Flow <= Minimal_Target then
	       return 0;
	    end if;
	 end;
      end if;

      if Current_Open_State(Current_Position) = False and
	Valves(Current_Position).Flow_Rate /= 0 then
	 -- try open this first
	 Current_Open_State(Current_Position) := True;
	 
	 declare
	    Current_Flow : Natural := Valves(Current_Position).Flow_Rate * (Remaining_Time - 1);
	    Next_Target : Natural;
	 begin
	    if Current_Flow > Minimal_Target then
	       Next_Target := 0;
	    else
	       Next_Target := Minimal_Target - Current_Flow;
	    end if;
	    Max_Value := Current_Flow +
	      Dfs(Current_Position, Current_Open_State, Remaining_Time - 1, Next_Target);
	 end;

	 Current_Open_State(Current_Position) := False;
      end if;
      
      -- try open the connected valves
      for Dest of Valves(Current_Position).Destinations loop
	 declare
	    Res : Natural;
	 begin
	    Res := Dfs(Dest, Current_Open_State, Remaining_Time - 1,
		       Natural'Max(Max_Value, Minimal_Target));
	    if Res > Max_Value then
	       Max_Value := Res;
	    end if;
	 end;
      end loop;
      
      if Max_Value > Minimal_Target then
	 return Max_Value;
      else
	 return 0;
      end if;
   end Dfs;

begin
   while not End_Of_File loop
      Read_One_Valve;
   end loop;
   
   for V of Valves loop
      Put_Line(To_String(V.Name) & ": rate = " & Natural'Image(V.Flow_Rate));
      Put("  Connects to: ");
      for Dest of V.Destinations loop
	 Put(To_String(Valves(Dest).Name) & " ");
      end loop;
      New_Line; New_Line;
   end loop;
   
   declare
      Open_State: Valve_Open_State(Valves.First_Index .. Valves.Last_Index) :=
	(others => False);
      Start_Pos : Valve_Id;
      Result : Natural;
   begin
      for I in Valves.First_Index .. Valves.Last_Index loop
	 if To_String(Valves(I).Name) = "AA" then
	    Start_Pos := I;
	    exit;
	 end if;
      end loop;
      Result := Dfs(Start_Pos, Open_State, 30, 0);
      Put_Line(Natural'Image(Result));
   end;
end Advent_16_1;

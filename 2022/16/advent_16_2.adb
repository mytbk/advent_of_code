with Ada.Text_Io; use Ada.Text_Io;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;

procedure Advent_16_2 is
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
   
   package Distance_Vectors is new Ada.Containers.Vectors
     (Index_Type => Valve_Id, Element_Type => Natural);
   subtype Distance_Vector is Distance_Vectors.Vector;
   function "=" (A, B : Distance_Vector) return Boolean is
   begin
      return False;
   end "=";
   package Distance_Matrices is new Ada.Containers.Vectors
     (Index_Type => Valve_Id, Element_Type => Distance_Vectors.Vector);
   subtype Distance_Matrix is Distance_Matrices.Vector;
   
   Valve_Dist : Distance_Matrix;
   Invalid_Dist : constant Natural := Natural'Last;

   type Person_Info is record
      Remaining_Time : Natural;
      Current_Valve : Valve_Id;
   end record;
   -- record how much time left and the current position
   
   package Person_Vectors is new Ada.Containers.Vectors
     (Element_Type => Person_Info, Index_Type => Natural);
   
   type Valve_State is (Closed, Open);

   package Valve_State_Vectors is new Ada.Containers.Vectors
     (Element_Type => Valve_State, Index_Type => Valve_Id);
    
   type Search_Info is record
      Person_States : Person_Vectors.Vector;
      Valve_States : Valve_State_Vectors.Vector;
      Total_Flow : Natural;
      Estimated_Max_Flow : Natural;
   end record;
   
   type Priority_Info is record
      Total_Flow : Natural;
      Estimated_Max_Flow : Natural;
   end record;

   function Get_Priority (Info : Search_Info) return Priority_Info is
   begin
      return (Total_Flow => Info.Total_Flow,
	      Estimated_Max_Flow => Info.Estimated_Max_Flow);
   end Get_Priority;
   
   function Before (Left, Right : Priority_Info) return Boolean is
   begin
      if Left.Estimated_Max_Flow = Right.Estimated_Max_Flow then
	 return Left.Total_Flow > Right.Total_Flow;
      else
	 return Left.Estimated_Max_Flow > Right.Estimated_Max_Flow;
      end if;
   end Before;
   
   package Search_Queues_If is new Ada.Containers.Synchronized_Queue_Interfaces
     (Element_Type => Search_Info);
   
   package Search_Queues is new Ada.Containers.Unbounded_Priority_Queues
     (Queue_Interfaces => Search_Queues_If,
      Queue_Priority => Priority_Info);
   
   Search_Queue : Search_Queues.Queue;
   
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
   
   -- recompute the estimated maximum flow, assume we can open all the valve at once
   procedure Recompute_Max_Flow (Info : in out Search_Info) is
   begin
      Info.Estimated_Max_Flow := Info.Total_Flow;
      for Vid in Info.Valve_States.First_Index .. Info.Valve_States.Last_Index loop
	 if Info.Valve_States(Vid) = Closed then
	    declare
	       Max_Added_Flow : Natural := 0;
	       Added_Flow : Natural;
	    begin
	       for P of Info.Person_States loop
		  if P.Remaining_Time > Valve_Dist(P.Current_Valve)(Vid) then
		     Added_Flow := Valves(Vid).Flow_Rate *
		       (P.Remaining_Time - Valve_Dist(P.Current_Valve)(Vid) - 1);
		     if Added_Flow > Max_Added_Flow then
			Max_Added_Flow := Added_Flow;
		     end if;
		  end if;
	       end loop;
	       Info.Estimated_Max_Flow := Info.Estimated_Max_Flow + Max_Added_Flow;
	    end;
	 end if;
      end loop;
   end Recompute_Max_Flow;

   function Astar_Search return Natural is
      use Ada.Containers;
      
      Current_Info : Search_Info;
      
      Current_Valve_States : Valve_State_Vectors.Vector
	renames Current_Info.Valve_States;
      Current_Person_States : Person_Vectors.Vector
	renames Current_Info.Person_States;
   begin
      while Search_Queue.Current_Use /= 0 loop
	 Search_Queue.Dequeue(Current_Info);
	 
	 -- the initial remaining time of each person is the same, we
	 -- can force the remaining time to be non-decreasing
	 --
	 -- If we don't consider this symmetry, the search space will
	 -- be an exponential growth
	 for Pid in Current_Person_States.First_Index + 1 .. Current_Person_States.Last_Index loop
	    if Current_Person_States(Pid).Remaining_Time < Current_Person_States(Pid - 1).Remaining_Time then
	       goto Continue;
	    end if;
	 end loop;
	 
	 if Current_Info.Estimated_Max_Flow = Current_Info.Total_Flow then
	    -- when estimated flow is total flow, it means we cannot
	    -- open more valves to release more pressure, and the way
	    -- we now find on the queue front is the best
	    return Current_Info.Total_Flow;
	 end if;
	 
--	 Put_Line("Current flow: " & Natural'Image(Current_Info.Total_Flow) & ", Estimate: "
--		    & Natural'Image(Current_Info.Estimated_Max_Flow));
	 
	 -- search by opening an arbitrary valve by an arbitrary person
	 for Vid in Current_Valve_States.First_Index .. Current_Valve_States.Last_Index loop
	    if Current_Valve_States(Vid) = Closed then
	       for Pid in Current_Person_States.First_Index .. Current_Person_States.Last_Index loop
		  declare
		     Person_Pos : constant Valve_Id := Current_Person_States(Pid).Current_Valve;
		     Person_Time : constant Natural := Current_Person_States(Pid).Remaining_Time;
		     Dist : constant Natural := Valve_Dist(Person_Pos)(Vid);
		     New_Person_State : Person_Info;
		     Added_Flow : Natural;
		     New_Search_Info : Search_Info := Current_Info;
		  begin
		     if Dist < Person_Time then
			New_Person_State := (Remaining_Time => Person_Time - Dist - 1,
					     Current_Valve => Vid);
			Added_Flow := Valves(Vid).Flow_Rate * New_Person_State.Remaining_Time;
			New_Search_Info.Person_States(Pid) := New_Person_State;
			New_Search_Info.Valve_States(Vid) := Open;
			New_Search_Info.Total_Flow := New_Search_Info.Total_Flow + Added_Flow;
			Recompute_Max_Flow (New_Search_Info);
			Search_Queue.Enqueue(New_Search_Info);
		     end if;
		  end;
	       end loop;
	    end if;
	 end loop;
	 
      <<Continue>>
      end loop;

      -- should not go here
      return 0;
   end Astar_Search;

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
   
   Valve_Dist.Set_Length(Valves.Length);
   for Dv of Valve_Dist loop
      Dv.Set_Length(Valves.Length);
   end loop;
   for I in Valves.First_Index .. Valves.Last_Index loop
      for J in Valves.First_Index .. Valves.Last_Index loop
	 if I = J then
	    Valve_Dist(I)(J) := 0;
	 else
	    Valve_Dist(I)(J) := Invalid_Dist;
	 end if;
      end loop;
      for Dest of Valves(I).Destinations loop
	 Valve_Dist(I)(Dest) := 1;
      end loop;
   end loop;
   
   -- Use Floyd-Warshall algorithm to calculate the distances
   for K in Valves.First_Index .. Valves.Last_Index loop
      for I in Valves.First_Index .. Valves.Last_Index loop
	 for J in Valves.First_Index .. Valves.Last_Index loop
	    if Valve_Dist(I)(K) /= Invalid_Dist and
	      Valve_Dist(K)(J) /= Invalid_Dist then
	       declare
		  New_Dist : constant Natural :=
		    Valve_Dist(I)(K) + Valve_Dist(K)(J);
	       begin
		  if Valve_Dist(I)(J) = Invalid_Dist or else
		    New_Dist < Valve_Dist(I)(J) then
		     Valve_Dist(I)(J) := New_Dist;
		  end if;
	       end;
	    end if;
	 end loop;
      end loop;
   end loop;

   Put_Line("Finished computing the shortest path of all vertex pairs.");

   declare
      Initial_Search_Info : Search_Info;
      Start_Pos : Valve_Id;
   begin
      for I in Valves.First_Index .. Valves.Last_Index loop
	 if To_String(Valves(I).Name) = "AA" then
	    Start_Pos := I;
	    exit;
	 end if;
      end loop;
      
      -- for part 1
--      Initial_Search_Info.Person_States.Append
--	(Person_Info'(Remaining_Time => 30, Current_Valve => Start_Pos));
      -- add a person and an elephant
      Initial_Search_Info.Person_States.Append
	(Person_Info'(Remaining_Time => 26, Current_Valve => Start_Pos));
      Initial_Search_Info.Person_States.Append
	(Person_Info'(Remaining_Time => 26, Current_Valve => Start_Pos));
      
      for Vid in Valves.First_Index .. Valves.Last_Index loop
	 if Valves(Vid).Flow_Rate = 0 then
	    -- assume the valve is open if the flow rate is 0
	    Initial_Search_Info.Valve_States.Append(Open);
	 else
	    Initial_Search_Info.Valve_States.Append(Closed);
	 end if;
      end loop;
      
      Initial_Search_Info.Total_Flow := 0;
      Recompute_Max_Flow(Initial_Search_Info);
      Search_Queue.Enqueue(Initial_Search_Info);
   end;
   
   declare
      Result : constant Natural := Astar_Search;
   begin
      Put_Line(Natural'Image(Result));
   end;
end Advent_16_2;

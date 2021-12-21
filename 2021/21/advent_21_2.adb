with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;

procedure Advent_21_2 is
   subtype Game_Position is Positive range 1 .. 10;
   
   function Next_Pos(Current_Pos: Game_Position; Step: Natural) return Game_Position is
      New_Pos : Natural := (Current_Pos + Step) mod Game_Position'Last;
   begin
      if New_Pos = 0 then
	 return Game_Position'Last;
      else
	 return New_Pos;
      end if;
   end Next_Pos;
   
   subtype Score is Natural range 0 .. 30;
   type Player_Id is mod 2;
   type Universe_Count is range 0 .. 2 ** 63 - 1;
   
   type State_Array is array (Score, Score, Game_Position, Game_Position, Player_Id) of Universe_Count;
   Num_Universes: State_Array;
   
   procedure Init_State_Array(State: out State_Array) is
   begin
      for S0 in Score'Range loop
	 for S1 in Score'Range loop
	    for P0 in Game_Position'Range loop
	       for P1 in Game_Position'Range loop
		  for P in Player_Id'Range loop
		     State(S0,S1,P0,P1,P) := 0;
		  end loop;
	       end loop;
	    end loop;
	 end loop;
      end loop;
   end Init_State_Array;

   Init_0: Game_Position;
   Init_1: Game_Position;
   
   Num_Win_Universe_0: Universe_Count := 0;
   Num_Win_Universe_1: Universe_Count := 0;
   
   Dice_Value: array (1 .. 27) of Positive;
   
   procedure Init_Dice_Value is
      Idx : Positive := 1;
   begin
      for I in 1 .. 3 loop
	 for J in 1 .. 3 loop
	    for K in 1 .. 3 loop
	       Dice_Value(Idx) := I + J + K;
	       Idx := Idx + 1;
	    end loop;
	 end loop;
      end loop;
   end Init_Dice_Value;
begin
   Init_Dice_Value;
   Get(Init_0);
   Get(Init_1);
   
   Init_State_Array(Num_Universes);
   -- initial universe
   Num_Universes(0, 0, Init_0, Init_1, 0) := 1;
   
   loop
      declare
	 Next_State: State_Array;
	 P_Next: array (1 .. 27) of Game_Position;
      begin
	 Init_State_Array(Next_State);
	 
	 for Player in Player_Id'Range loop -- next turn is Player
	    for S0 in 0 .. 20 loop
	       for S1 in 0 .. 20 loop
		  for P0 in Game_Position'Range loop
		     for P1 in Game_Position'Range loop
			if Num_Universes(S0, S1, P0, P1, Player) /= 0 then
			   -- Put_Line("(" & Score'Image(S0) & "," & Score'Image(S1) & "," & Game_Position'Image(P0)
			   --   & "," & Game_Position'Image(P1) & "," & Player_Id'Image(Player) & ")");
			   for I in 1 .. 27 loop
			      if Player = 0 then
				 P_Next(I) := Next_Pos(P0, Dice_Value(I));
				 Next_State(S0 + P_Next(I), S1, P_Next(I), P1, Player + 1) :=
				   Next_State(S0 + P_Next(I), S1, P_Next(I), P1, Player + 1)
				     + Num_Universes(S0, S1, P0, P1, Player);
			      else
				 P_Next(I) := Next_Pos(P1, Dice_Value(I));
				 Next_State(S0, S1 + P_Next(I), P0, P_Next(I), Player + 1) :=
				   Next_State(S0, S1 + P_Next(I), P0, P_Next(I), Player + 1)
				     + Num_Universes(S0, S1, P0, P1, Player);
			      end if;
			   end loop;
			end if;
		     end loop;
		  end loop;
	       end loop;
	    end loop;
	 end loop;
	 
	 -- also need to add the already finished game
	 for S0 in Score'Range loop
	    for S1 in Score'Range loop
	       for P0 in Game_Position'Range loop
		  for P1 in Game_Position'Range loop
		     for Player in Player_Id'Range loop
			if S0 >= 21 or else S1 >= 21 then
			   Next_State(S0,S1,P0,P1,Player) :=
			     Next_State(S0,S1,P0,P1,Player)
			       + Num_Universes(S0,S1,P0,P1,Player);
			end if;
		     end loop;
		  end loop;
	       end loop;
	    end loop;
	 end loop;
	 
	 exit when Num_Universes = Next_State;
	 Num_Universes := Next_State;
      end;
   end loop;
   
   for S0 in 21 .. Score'Last loop
      for S1 in Score'Range loop
	 for P0 in Game_Position'Range loop
	    for P1 in Game_Position'Range loop
	       Num_Win_Universe_0 := Num_Win_Universe_0 + Num_Universes(S0,S1,P0,P1,1);
	    end loop;
	 end loop;
      end loop;
   end loop;
   
   for S0 in Score'Range loop
      for S1 in 21 .. Score'Last loop
	 for P0 in Game_Position'Range loop
	    for P1 in Game_Position'Range loop
	       Num_Win_Universe_1 := Num_Win_Universe_1 + Num_Universes(S0,S1,P0,P1,0);
	    end loop;
	 end loop;
      end loop;
   end loop;
   
   Put_Line(Universe_Count'Image(Num_Win_Universe_0));
   Put_Line(Universe_Count'Image(Num_Win_Universe_1));
end Advent_21_2;

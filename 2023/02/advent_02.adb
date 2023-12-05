with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_Io; use Ada.Text_Io;
with String_Util; use String_Util;

procedure Advent_02 is
   
   type Cube_Set_Type is record
      Num_Red, Num_Green, Num_Blue : Integer;
   end record;
   
   package Cube_Set_Vectors is new Ada.Containers.Vectors
     (Element_Type => Cube_Set_Type, Index_Type => Positive);
   subtype Cube_Set_Vector is Cube_Set_Vectors.Vector;

   type Game_Info is record
      Game_Id : Integer;
      Cube_Sets : Cube_Set_Vector;
   end record;

   procedure Parse_Game(S : String; Info : out Game_Info) is
      Game_And_Content : constant String_Vec := Split_String(S, ':');
      Game_Sets : constant String_Vec := Split_String(To_String(Game_And_Content(2)), ';');
      Game_Number : Integer;
      Found_Game_Number : Boolean;
      Unused_Last : Positive;
   begin
      Find_Number(To_String(Game_And_Content(1)), Game_Number, Found_Game_Number, Unused_Last);
      Info.Game_Id := Game_Number;
      for Game_Set of Game_Sets loop
	 declare
	    Cube_Strings : constant String_Vec := Split_String(To_String(Game_Set), ',');
	 begin
	    declare
	       Cube_Set : Cube_Set_Type := (others => 0);
	       Cube_Number : Integer;
	       Found_Cube_Number : Boolean;
	       Unused_Last_2 : Positive;
	    begin
	       for Cube_Str of Cube_Strings loop
		  Find_Number(To_String(Cube_Str), Cube_Number, Found_Cube_Number, Unused_Last_2);
		  if Contains(To_String(Cube_Str), "red") then
		     Cube_Set.Num_Red := Cube_Number;
		  elsif Contains(To_String(Cube_Str), "green") then
		     Cube_Set.Num_Green := Cube_Number;
		  elsif Contains(To_String(Cube_Str), "blue") then
		     Cube_Set.Num_Blue := Cube_Number;
		  end if;
	       end loop;
	       Info.Cube_Sets.Append(Cube_Set);
	    end;
	 end;
      end loop;
   end Parse_Game;
   
   -- part 1
   Sum_Of_Possible : Integer := 0;
   
   -- part 2
   Total_Power : Integer := 0;
begin
   while not End_Of_File loop
      declare
	 S : constant String := Get_Line;
	 G : Game_Info;
	 Possible : Boolean := True;
	 Min_Num_Red, Min_Num_Green, Min_Num_Blue : Integer := 0;
	 Power : Integer;
      begin
	 Parse_Game(S, G);
--	 Put_Line("Game " & Integer'Image(G.Game_Id));
	 for S of G.Cube_Sets loop
--	    Put_Line(Integer'Image(S.Num_Red) & " red, " &
--		       Integer'Image(S.Num_Green) & " green, " &
--		       Integer'Image(S.Num_Blue) & " blue");
	    Min_Num_Red := Integer'Max(Min_Num_Red, S.Num_Red);
	    Min_Num_Green := Integer'Max(Min_Num_Green, S.Num_Green);
	    Min_Num_Blue := Integer'Max(Min_Num_Blue, S.Num_Blue);
	    if Min_Num_Red > 12 or Min_Num_Green > 13 or Min_Num_Blue > 14 then
	       Possible := False;
	    end if;
	 end loop;
	 
	 if Possible then
	    Sum_Of_Possible := Sum_Of_Possible + G.Game_Id;
	 end if;
	 
	 Power := Min_Num_Red * Min_Num_Green * Min_Num_Blue;
--	 Put_Line(Integer'Image(Power));
	 Total_Power := Total_Power + Power;
      end;
   end loop;
   Put_Line(Integer'Image(Sum_Of_Possible));
   Put_Line(Integer'Image(Total_Power));
end Advent_02;

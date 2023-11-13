with Ada.Text_Io; use Ada.Text_Io;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers.Vectors;

procedure Advent_15 is
   type Sensor_Info is record
      Sensor_X, Sensor_Y, Beacon_X, Beacon_Y : Integer;
   end record;
   
   type Integer_Range is record
      First, Last : Integer;
   end record;
   package Int_Range_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Integer_Range);
   subtype Int_Range_Vector is Int_Range_Vectors.Vector;
   
   procedure Find_Number(S : String; Num : out Integer; Found : out Boolean; Last : out Positive) is
      Pos, End_Pos : Positive;
      
      function Is_Num_Char(C : Character) return Boolean is
      begin
	 return C = '-' or (C >= '0' and C <= '9');
      end Is_Num_Char;
   begin
      Pos := S'First;
      while Pos <= S'Last and then (not Is_Num_Char(S(Pos))) loop
	 Pos := Pos + 1;
      end loop;
      
      if Pos > S'Last then
	 Found := False;
	 return;
      end if;

      End_Pos := Pos;
      while End_Pos <= S'Last and then Is_Num_Char(S(End_Pos)) loop
	 End_Pos := End_Pos + 1;
      end loop;
      
      Found := True;
      Num := Integer'Value(S(Pos .. End_Pos - 1));
      Last := End_Pos - 1;
   end Find_Number;

   procedure Parse_Sensor_Info(S: String; Info: out Sensor_Info) is
      Last : Positive;
      Found : Boolean;
   begin
      Find_Number(S, Info.Sensor_X, Found, Last);
      Find_Number(S(Last + 1 .. S'Last), Info.Sensor_Y, Found, Last);
      Find_Number(S(Last + 1 .. S'Last), Info.Beacon_X, Found, Last);
      Find_Number(S(Last + 1 .. S'Last), Info.Beacon_Y, Found, Last);
   end Parse_Sensor_Info;

   Target_Row : Integer;
   
   Ranges : Int_Range_Vector;
   Num_Cannot_Contain : Natural := 0;
begin
   if Argument_Count /= 1 then
      Put_Line("Usage: advent_15 <target row>");
      return;
   end if;
   
   Target_Row := Integer'Value(Argument(1));
   
   while not End_Of_File loop
      declare
	 Info: Sensor_Info;
	 S : constant String := Get_Line;
	 Sensor_Beacon_Distance : Natural;
	 Max_X_Distance : Integer;
	 X_Min, X_Max : Integer;
      begin
	 Parse_Sensor_Info(S, Info);
	 -- Put_Line(Integer'Image(Info.Sensor_X) & Integer'Image(Info.Sensor_Y) &
	 --	    Integer'Image(Info.Beacon_X) & Integer'Image(Info.Beacon_Y));
	 Sensor_Beacon_Distance := abs (Info.Sensor_X - Info.Beacon_X) + abs (Info.Sensor_Y - Info.Beacon_Y);
	 Max_X_Distance := Sensor_Beacon_Distance - abs (Target_Row - Info.Sensor_Y);
	 if Max_X_Distance >= 0 then
	    X_Min := Info.Sensor_X - Max_X_Distance;
	    X_Max := Info.Sensor_X + Max_X_Distance;
	    if Info.Beacon_Y /= Target_Row then
	       Ranges.Append(Integer_Range'(First => X_Min, Last => X_Max));
	    else
	       -- already has a beacon in this row, split at Beacon_X
	       pragma Assert(Info.Beacon_X = X_Min or Info.Beacon_X = X_Max);
	       if Info.Beacon_X = X_Min then
		  Ranges.Append(Integer_Range'(First => X_Min + 1, Last => X_Max));
	       else
		  Ranges.Append(Integer_Range'(First => X_Min, Last => X_Max - 1));
	       end if;
	    end if;
	 end if;
      end;
   end loop;
   
   for I in Ranges.First_Index .. Ranges.Last_Index loop
      for X in Ranges(I).First .. Ranges(I).Last loop
	 declare
	    In_Prev_Ranges : Boolean := False;
	 begin
	    for J in Ranges.First_Index .. I - 1 loop
	       if X >= Ranges(J).First and X <= Ranges(J).Last then
		  In_Prev_Ranges := True;
		  exit;
	       end if;
	    end loop;
	    if not In_Prev_Ranges then
	       Num_Cannot_Contain := Num_Cannot_Contain + 1;
	    end if;
	 end;
      end loop;
   end loop;
   
   Put_Line(Natural'Image(Num_Cannot_Contain));
end Advent_15;

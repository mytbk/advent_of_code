-- use z3 to solve part 2, use this program to print the smtlib expressions

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers.Vectors;

procedure Advent_15_Z3 is
   type Sensor_Info is record
      Sensor_X, Sensor_Y, Beacon_X, Beacon_Y : Integer;
   end record;
   
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

begin
   Put_Line("(declare-const x Int)");
   Put_Line("(declare-const y Int)");
   Put_Line("(assert (>= x 0))");
   Put_Line("(assert (>= y 0))");

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
	 Put_Line("(assert (> (+ (abs (- x " & Integer'Image(Info.Sensor_X) &
		    ")) (abs (- y " & Integer'Image(Info.Sensor_Y) & ")))" &
		    Natural'Image(Sensor_Beacon_Distance) & "))");
      end;
   end loop;
end Advent_15_Z3;

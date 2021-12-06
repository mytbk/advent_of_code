with Ada.Text_Io;
use Ada.Text_Io;
with Ada.Integer_Text_Io;
use Ada.Integer_Text_Io;

with String_Util;
use String_Util;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

procedure Advent_06 is
   type Timer_Value is range 0 .. 8;
   package Timer_Vectors is new Ada.Containers.Vectors
     (Element_Type => Timer_Value, Index_Type => Natural);
   subtype Timer_Vector is Timer_Vectors.Vector;
   
   type Fish_Count is range 0 .. 2 ** 63 - 1; -- should be enough
   subtype Day_Type is Natural range 0 .. 256;
   type Fish_Count_Array is array(Timer_Value) of Fish_Count;
   type Count_Per_Day_Array is array(Day_Type) of Fish_Count;
   
   -- F(6,N) = F(6, N-7) + F(8, N-7)
   -- F(8,N) = F(6, N-9) + F(8, N-9)
   Count_6 : Count_Per_Day_Array; -- store F(6,N)
   Count_8 : Count_Per_Day_Array; -- store F(8,N)
   
   procedure Init_Count_Tab is
   begin
      for I in Day_Type'Range loop
	 if I <= 6 then
	    Count_6(I) := 1;
	    Count_8(I) := 1;
	 elsif I <= 8 then
	    Count_6(I) := Count_6(I - 7) + Count_8(I - 7);
	    Count_8(I) := 1;
	 else
	    Count_6(I) := Count_6(I - 7) + Count_8(I - 7);
	    Count_8(I) := Count_6(I - 9) + Count_8(I - 9);
	 end if;
      end loop;
   end Init_Count_Tab;
   
   Fish_Count_Table : Fish_Count_Array;
   function Total_Fish_Number(T0: Timer_Value; N: Integer) return Fish_Count is
      Time_To_Produce_New : Integer := Integer(T0) + 1;
   begin
      if N < Time_To_Produce_New then
	 return 1;
      end if;
      
      return Count_6(N - Time_To_Produce_New) + Count_8(N - Time_To_Produce_New);
   end Total_Fish_Number;
   
   procedure Init_Table(Days: Integer) is
   begin
      for I in Timer_Value'Range loop
	 Fish_Count_Table(I) := Total_Fish_Number(I, Days);
      end loop;
   end Init_Table;
   
   Timers: Timer_Vector;
   Days: constant Positive := 80;
   
   Timers2 : Timer_Vector;
   Count2 : Fish_Count := 0;
begin
   declare
      L: String := Get_Line;
      Timer_Strings: StringVec := Split_String(L, ',');
   begin
      for T of Timer_Strings loop
	 Timers.Append(Timer_Value'Value(To_String(T)));
      end loop;
   end;
   
   Timers2 := Timers;
   
   for I in 1 .. Days loop
      declare
	 Produced: Natural := 0;
      begin
	 for T of Timers loop
	    if T = 0 then
	       T := 6;
	       Produced := Produced + 1;
	    else
	       T := T - 1;
	    end if;
	 end loop;
	 
	 for I in 1 .. Produced loop
	    Timers.Append(8);
	 end loop;
      end;
   end loop;
   
   Put(Natural(Timers.Length));
   
   Init_Count_Tab;
   Init_Table(256);
   for T of Timers2 loop
      Count2 := Count2 + Fish_Count_Table(T);
   end loop;
   Put_Line(Fish_Count'Image(Count2));
end Advent_06;

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Containers.Vectors;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with String_Util; use String_Util;

procedure Advent_06 is
   Time_String : constant String := Get_Line;
   Distance_String : constant String := Get_Line;
   
   package Int_Vectors is new Ada.Containers.Vectors
     (Element_Type => Integer, Index_Type => Natural);
   subtype Int_Vector is Int_Vectors.Vector;
   
   Time_Data : Int_Vector;
   Distance_Data : Int_Vector;
   
   Product_Of_Ways : Long_Long_Integer := 1;

   -- find the range of x such that x*(time-x)>distance, that is to solve
   -- x^2 - tx + d < 0
   function Solve_Range(T, D : Long_Long_Integer) return Long_Long_Integer is
      -- we need enough accuracy, my input has 15 decimal digits for distance
      type My_Float is digits 15;

      package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions
	(Float_Type => My_Float);
      use Float_Functions;
      T_F : My_Float := My_Float(T);
      D_F : My_Float := My_Float(D);
      Range_Low, Range_High : Long_Long_Integer;
   begin
      Range_Low := Long_Long_Integer((T_F - Sqrt(T_F * T_F - 4.0 * D_F)) / 2.0);
      Range_High := Long_Long_Integer((T_F + Sqrt(T_F * T_F - 4.0 * D_F)) / 2.0);
      
      -- I don't like Floor or Ceiling functions, so do this ...
      while Range_Low * (T - Range_Low) <= D loop
	 Range_Low := Range_Low + 1;
      end loop;
      while Range_High * (T - Range_High) <= D loop
	 Range_High := Range_High - 1;
      end loop;
      
      if Range_Low <= Range_High then
	 return (Range_High - Range_Low + 1);
      else
	 return 0;
      end if;
   end Solve_Range;
begin
   -- part 1
   declare
      Num : Integer;
      Found : Boolean;
      Last : Positive;
      Pos : Positive;
   begin
      Pos := Time_String'First;
      loop
	 Find_Number(Time_String(Pos .. Time_String'Last), Num, Found, Last);
	 exit when not Found;
	 Time_Data.Append(Num);
	 Pos := Last + 1;
      end loop;
      
      Pos := Distance_String'First;
      loop
	 Find_Number(Distance_String(Pos .. Distance_String'Last), Num, Found, Last);
	 exit when not Found;
	 Distance_Data.Append(Num);
	 Pos := Last + 1;
      end loop;
   end;
   
   for I in Time_Data.First_Index .. Time_Data.Last_Index loop
      declare
	 T : Integer := Time_Data(I);
	 D : Integer := Distance_Data(I);
      begin
	 
	 Product_Of_Ways := Product_Of_Ways *
	   Solve_Range (Long_Long_Integer(T), Long_Long_Integer(D));
      end;
   end loop;
   
   Put_Line(Long_Long_Integer'Image(Product_Of_Ways));
   
   -- part 2
   -- we just read all the digits
   declare
      Time, Distance : Long_Long_Integer := 0;
   begin
      for C of Time_String loop
	 if Is_Decimal_Digit(C) then
	    Time := Time * 10 + Character'Pos(C) - Character'Pos('0');
	 end if;
      end loop;
      
      for C of Distance_String loop
	 if Is_Decimal_Digit(C) then
	    Distance := Distance * 10 + Character'Pos(C) - Character'Pos('0');
	 end if;
      end loop;
      Put_Line(Long_Long_Integer'Image(Solve_Range(Time, Distance)));
   end;
end Advent_06;

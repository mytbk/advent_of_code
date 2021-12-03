-- version 2: a slightly cleaner version with some functional abstraction

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure advent_03 is
   package Report_Vectors is new Ada.Containers.Vectors(Element_Type => Unbounded_String, Index_Type => Natural);
   subtype Report_Vec is Report_Vectors.Vector;

   procedure Count_Bits (Reports: Report_Vec; Bidx: Positive; Num_Zero,Num_One: out Natural)
       with Post => Num_Zero + Num_One = Natural(Reports.Length);
   
   procedure Count_Bits (Reports: Report_Vec; Bidx: Positive; Num_Zero,Num_One: out Natural) is
   begin
      Num_Zero := 0;
      Num_One := 0;
      for R of Reports loop
	 if To_String(R)(Bidx) = '0' then
	    Num_Zero := Num_Zero + 1;
	 else
	    Num_One := Num_One + 1;
	 end if;
      end loop;
   end Count_Bits;
   
   function power_consumption(reports: Report_Vec) return Integer is
      BitLen : constant Integer := Length(reports(0));
      gamma, epsilon: String(1 .. BitLen);

      gamma_num, epsilon_num: Integer;
      Unused_Last: Positive;

      type Freq_Array is array (1 .. BitLen) of Natural;
      num_one, num_zero: Freq_Array := (others => 0);
   begin
      for I in 1 .. BitLen loop
	 Count_Bits(Reports, I, Num_Zero(I), Num_One(I));
      end loop;

      for I in 1 .. BitLen loop
	 if num_one(I) > num_zero(I) then
	    gamma(I) := '1';
	    epsilon(I) := '0';
	 else
	    gamma(I) := '0';
	    epsilon(I) := '1';
	 end if;
      end loop;

      Get("2#" & String(gamma) & "#", gamma_num, Unused_Last);
      Get("2#" & String(epsilon) & "#", epsilon_num, Unused_Last);
      return gamma_num * epsilon_num;
   end power_consumption;
   
   -- Try to write something functional...
   package Filter_Type is
      -- It's not necessary, but we just write a dispatching type here.
      type Filter is tagged null record;
      function Test(Self: Filter; S: Unbounded_String) return Boolean is (True);
      
      type Char_Sel_Filt is new Filter with record
	 The_Char: Character;
	 Position: Positive;
      end record;
      
      overriding function Test(Self: Char_Sel_Filt; S: Unbounded_String) return Boolean is
	(To_String(S)(Self.Position) = Self.The_Char);
   end Filter_Type;
   
   use Filter_Type;
   
   function Filter_Vec(Reports: Report_Vec; Filt: Filter'Class) return Report_Vec is
      Result: Report_Vec;
   begin
      for R of Reports loop
	 if Filt.Test(R) then
	    Result.Append(R);
	 end if;
      end loop;
      return Result;
   end Filter_Vec;
   
   type Get_Filter_Function is access function(Reports: Report_Vec; Bidx: Positive) return Char_Sel_Filt;

   function Get_Bit_Filter_O2(Reports: Report_Vec; bit_index: Positive) return Char_Sel_Filt is
      Num_Zero, Num_One: Natural;
   begin
      Count_Bits(Reports, Bit_Index, Num_Zero, Num_One);
      if num_zero > num_one then
	 return (The_Char => '0', Position => Bit_Index);
      else
	 return (The_Char => '1', Position => Bit_Index);
      end if;
   end Get_Bit_Filter_O2;
   
   function Get_Bit_Filter_CO2(Reports: Report_Vec; Bit_Index: Positive) return Char_Sel_Filt is
      Num_Zero, Num_One: Natural;
   begin
      Count_Bits(Reports, Bit_Index, Num_Zero, Num_One);
      if num_zero > num_one then
	 return (The_Char => '1', Position => Bit_Index);
      else
	 return (The_Char => '0', Position => Bit_Index);
      end if;
   end Get_Bit_Filter_CO2;
   
   function Rating_By_Filter(Reports: Report_Vec; F: Get_Filter_Function) return Integer is
      use Ada.Containers;
      
      Current_Reports: Report_Vec := Reports;
      rate: Integer;
      
      Bit_Index : Positive := 1;
      
      Unused_Last: Positive;
   begin
      while Current_Reports.Length > 1 loop
	 Current_Reports := Filter_Vec(Current_Reports, F(Current_Reports, Bit_Index));
	 Bit_Index := Bit_Index + 1;
      end loop;
      Get("2#" & To_String(current_reports.First_Element) & "#", rate, Unused_Last);
      return rate;
   end Rating_By_Filter;
   
   function O2_Rating(Reports: Report_Vec) return Integer is
      (Rating_By_Filter(Reports, Get_Bit_Filter_O2'Access));
   
   function CO2_Rating(Reports: Report_Vec) return Integer is
     (Rating_By_Filter(Reports, Get_Bit_Filter_CO2'Access));

   function life_rating(reports: Report_Vec) return Integer is
      O2rate : constant Integer := O2_Rating(Reports);
      CO2rate: constant Integer := CO2_Rating(Reports);
   begin
      return O2rate * CO2rate;
   end life_rating;

   reports: Report_Vec;
begin
   while not End_Of_File loop
      declare
	 L: String := Get_Line;
      begin
	 reports.Append(To_Unbounded_String(L));
      end;
   end loop;

   Put(power_consumption(reports));
   Put(life_rating(reports));
end advent_03;

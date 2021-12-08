-- very rough code and using brute force for part 2

with Ada.Text_Io;
use Ada.Text_Io;
with Ada.Integer_Text_Io;
use Ada.Integer_Text_Io;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with String_Util;
use String_Util;

procedure Advent_08 is
   Number_Of_1478: Natural := 0;
   
   type Pattern_Array is array (1 .. 10) of Unbounded_String;
   type Output_Value_Array is array (1 .. 4) of Unbounded_String;
   type Entry_Type is record
      Signal_Patterns : Pattern_Array;
      Output_Values: Output_Value_Array;
   end record;

   package Entry_Vectors is new Ada.Containers.Vectors
     (Element_Type => Entry_Type, Index_Type => Positive);
   subtype Entry_Vec is Entry_Vectors.Vector;
   Entries: Entry_Vec;
   
   subtype Segment_Digit is Character range 'a' .. 'g';
   type Segment_Map is array (Segment_Digit) of Segment_Digit;
   
   function Transform(Smap: Segment_Map; S: String) return String is
      T: String(1 .. S'Length) := S;
   begin
      for I in S'Range loop
	 T(I - S'First + 1) := Smap(S(I));
      end loop;
      return T;
   end Transform;
   
   type String_Access is access String;
   subtype Decimal_Digit is Integer range 0 .. 9;
   type String_Access_Array is array (Decimal_Digit) of String_Access;
   Digit_Segments : constant String_Access_Array :=
     (new String'("abcefg"), new String'("cf"), new String'("acdeg"), new String'("acdfg"),
      new String'("bcdf"), new String'("abdfg"), new String'("abdefg"), new String'("acf"),
      new String'("abcdefg"), new String'("abcdfg"));

   function Match_Digit(S: String) return Integer is
      function Match(T: String) return Boolean is
	 Has_Char: Boolean;
      begin
	 if S'Length /= T'Length then
	    return False;
	 end if;
	 
	 for Cs of S loop
	    Has_Char := False;
	    for Ct of T loop
	       if Ct = Cs then
		  Has_Char := True;
	       end if;
	    end loop;
	    if not Has_Char then
	       return False;
	    end if;
	 end loop;
	 return True;	       
      end Match;
   begin
      for I in Decimal_Digit'Range loop
	 if Match(Digit_Segments(I).all) then
	    return I;
	 end if;
      end loop;
      return -1;
   end Match_Digit;

   function Solve_Segment_Map(Patterns: Pattern_Array) return Segment_Map is
      Segmap: Segment_Map := ('a','b','c','d','e','f','g');
      type Test_Array is array (Decimal_Digit) of Boolean;
      Has_Digit : Test_Array;
      
      -- Now we iterate all posible maps
      -- TODO: calculate next permutation
      procedure Next is
	 I: Segment_Digit := Segment_Digit'Last;
      begin
	 while I >= Segment_Digit'First loop
	    if Segmap(I) /= Segment_Digit'Last then
	       Segmap(I) := Segment_Digit'Succ(Segmap(I));
	       return;
	    else
	       Segmap(I) := Segment_Digit'First;
	    end if;
	    I := Segment_Digit'Pred(I);
	 end loop;
      end Next;
   begin
      loop
	 Has_Digit := (others => False);
	 for P of Patterns loop
	    declare
	       S: String := To_String(P);
	       Num: Integer := Match_Digit(Transform(Segmap,S));
	    begin
	       if Num >= 0 then
		  Has_Digit(Num) := True;
	       end if;
	    end;
	 end loop;
	 
	 if Has_Digit = Test_Array'(others => True) then
	    return Segmap;
	 end if;
	 Next;
      end loop;
   end Solve_Segment_Map;
   
   Sum_Output: Natural := 0;
begin
   while not End_Of_File loop
      declare
	 L: constant String := Get_Line;
	 Instrs: constant Stringvec := Split_String(L, ' ');
	 E: Entry_Type;
      begin
	 for I in 1 .. 10 loop
	    E.Signal_Patterns(I) := Instrs(I);
	 end loop;
	 for I in 1 .. 4 loop
	    E.Output_Values(I) := Instrs(11 + I);
	 end loop;
	 Entries.Append(E);
      end;
   end loop;
   
   for E of Entries loop
      for O of E.Output_Values loop
	 case Length(O) is
	    when 2 | 4 | 3 | 7 =>
	       Number_Of_1478 := Number_Of_1478 + 1;
	    when others => null;
	 end case;
      end loop;
   end loop;
   
   Put(Number_Of_1478);
   New_Line;
   
   for E of Entries loop
      declare
	 Smap : Segment_Map := Solve_Segment_Map(E.Signal_Patterns);
	 Out_Digit : Natural;
	 Decoded : Natural := 0;
      begin
	 for O of E.Output_Values loop
	    Out_Digit := Match_Digit(Transform(Smap,To_String(O)));
	    Put(Out_Digit);
	    Decoded := Decoded * 10 + Out_Digit;
	 end loop;
	 Sum_Output := Sum_Output + Decoded;
      end;
      New_Line;
   end loop;
   
   Put(Sum_Output);
   New_Line;
end Advent_08;

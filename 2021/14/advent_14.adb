with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_Io; use Ada.Text_Io;

procedure Advent_14 is
   type Source_Pair is array (1 .. 2) of Character;

   function Pair_Hash(P: Source_Pair) return Hash_Type is
   begin
      return Hash_Type(Character'Pos(P(1)) * 256) or
	Hash_Type(Character'Pos(P(2)));
   end Pair_Hash;

   package Insertion_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Source_Pair, Element_Type => Character,
      Hash => Pair_Hash, Equivalent_Keys => "=", "=" => "=");
   subtype Insertion_Map is Insertion_Maps.Map;
   
   Insertion_Rules: Insertion_Map;
   
   procedure Insert_Rule(S: String) is
      Src: Source_Pair;
   begin
      Src := Source_Pair(S(S'First .. S'First + 1));
      Insertion_Rules.Insert(Src, S(S'Last));
   end Insert_Rule;
   
   function Pair_Insertion(S: Unbounded_String) return Unbounded_String is
      Result: Unbounded_String;
      Len: constant Natural := Length(S);
      New_Char: Character;
   begin
      if Len < 2 then
	 return S;
      end if;
      Append(Result,Element(S,1));
      for I in 2 .. Len loop
	 New_Char := Insertion_Rules(Source_Pair(Slice(S, I - 1, I)));
	 Append(Result,New_Char);
	 Append(Result,Element(S,I));
      end loop;
      return Result;
   end Pair_Insertion;

   Polymer_Template: Unbounded_String;
   Generated_Polymer: Unbounded_String;
   
   Char_Count : array (Character) of Natural := (others => 0);
   Most_Used_Char: Character := Character'First;
   Least_Used_Char: Character := Character'First;
   
   -- part 2: we need to store the count of all the possible pairs
   type Pair_Count is range 0 .. 2 ** 63 - 1;
   package Pair_Count_Hashed_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Source_Pair, Element_Type => Pair_Count,
      Hash => Pair_Hash, Equivalent_Keys => "=", "=" => "=");
   use Pair_Count_Hashed_Maps;
   subtype Pair_Count_Map is Pair_Count_Hashed_Maps.Map;
   
   Pair_Counts: Pair_Count_Map;
   
   procedure Add_Count(Pm: in out Pair_Count_Map; S: Source_Pair; Count: Pair_Count) is
   begin
      if Pm.Contains(S) then
	 Pm(S) := Pm(S) + Count;
      else
	 Pm.Insert(S, Count);
      end if;
   end Add_Count;
   
   procedure Pair_Insertion is
      Pair_Counts_After_Insert: Pair_Count_Map;
   begin
      for P in Pair_Counts.Iterate loop
	 declare
	    Orig_Pair : Source_Pair := Key(P);
	    Count: Pair_Count := Element(P);
	    New_Char: Character;
	    New_P1, New_P2: Source_Pair;
	 begin
	    if Insertion_Rules.Contains(Orig_Pair) then
	       New_Char := Insertion_Rules(Orig_Pair);
	       New_P1 := Orig_Pair(1) & New_Char;
	       New_P2 := New_Char & Orig_Pair(2);
	       Add_Count(Pair_Counts_After_Insert, New_P1, Count);
	       Add_Count(Pair_Counts_After_Insert, New_P2, Count);
	    end if;
	 end;
      end loop;
      Move(Pair_Counts, Pair_Counts_After_Insert);
   end Pair_Insertion;
   
   Char_Count_Part2 : array (Character) of Pair_Count := (others => 0);
   Most_Used_Char_Part2: Character := Character'First;
   Least_Used_Char_Part2: Character := Character'First;
begin
   Polymer_Template := To_Unbounded_String(Get_Line);
   
   while not End_Of_File loop
      declare
	 L: constant String := Get_Line;
      begin
	 if L'Length >= 7 then
	    Insert_Rule(L);
	 end if;
      end;
   end loop;
   
   -- part 1: we just simulate
   Generated_Polymer := Polymer_Template;
   for Step in 1 .. 10 loop
      Generated_Polymer := Pair_Insertion(Generated_Polymer);
   end loop;
   
   for C of To_String(Generated_Polymer) loop
      Char_Count(C) := Char_Count(C) + 1;
   end loop;
   
   for C in Character'Range loop
      if Char_Count(C) > Char_Count(Most_Used_Char) then
	 Most_Used_Char := C;
      end if;
      if Char_Count(C) /= 0 and then
	(Char_Count(Least_Used_Char) = 0 or Char_Count(C) < Char_Count(Least_Used_Char)) then
	 Least_Used_Char := C;
      end if;
   end loop;
   
   Put_Line(Natural'Image(Char_Count(Most_Used_Char) - Char_Count(Least_Used_Char)));
   
   -- part 2
   for I in 2 .. Length(Polymer_Template) loop
      declare
	 Pair: constant Source_Pair := Source_Pair(Slice(Polymer_Template, I - 1, I));
      begin
	 Add_Count(Pair_Counts, Pair, 1);
      end;
   end loop;
   
   for I in 1 .. 40 loop
      Pair_Insertion;
   end loop;
   
   -- add each character of the pairs to char_count
   for P in Pair_Counts.Iterate loop
      for C of Key(P) loop
	 Char_Count_Part2(C) := Char_Count_Part2(C) + Element(P);
      end loop;
   end loop;
   -- note that the first and last character of the original polymer
   -- is counted only once, so add them
   declare
      First_Char: Character := Element(Polymer_Template, 1);
   begin
      Char_Count_Part2(First_Char) := Char_Count_Part2(First_Char) + 1;
   end;
   declare
      Last_Char: Character := Element(Polymer_Template, Length(Polymer_Template));
   begin
      Char_Count_Part2(Last_Char) := Char_Count_Part2(Last_Char) + 1;
   end;
   -- at last half all the counts
   for I in Character'Range loop
      Char_Count_Part2(I) := Char_Count_Part2(I) / 2;
   end loop;
   
   for C in Character'Range loop
      if Char_Count_Part2(C) > Char_Count_Part2(Most_Used_Char_Part2) then
	 Most_Used_Char_Part2 := C;
      end if;
      if Char_Count_Part2(C) /= 0 and then
	(Char_Count_Part2(Least_Used_Char_Part2) = 0 or else
	   Char_Count_Part2(C) < Char_Count_Part2(Least_Used_Char_Part2)) then
	 Least_Used_Char_Part2 := C;
      end if;
   end loop;
   
   Put_Line(Pair_Count'Image(Char_Count_Part2(Most_Used_Char_Part2) - Char_Count_Part2(Least_Used_Char_Part2)));
   
end Advent_14;

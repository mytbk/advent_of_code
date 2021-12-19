with Scanners; use Scanners;
with Positions; use Positions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Containers.Hashed_Sets;

procedure Advent_19 is
   function Parse_Position(S: String) return Position is
      Last: Positive;
      S_Cur: Positive;
      P: Position;
   begin
      Get(S, P.X, Last);
      S_Cur := Last + 2; -- skip ','
      Get(S(S_Cur .. S'Last), P.Y, Last);
      S_Cur := Last + 2; -- skip ','
      Get(S(S_Cur .. S'Last), P.Z, Last);
      return P;
   end Parse_Position;
   
   package Point_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Position, Hash => Hash, Equivalent_Elements => "=", "=" => "=");
   subtype Point_Set is Point_Sets.Set;
   
   Scanners: Scanner_Vector;
   All_Beacons: Point_Set;
begin
   while not End_Of_File loop
      declare
	 L: constant String := Get_Line;
      begin
	 if L'Length > 12 and then L(L'First .. L'First + 11) = "--- scanner " then
	    Scanners.Append((others => <>));
	 elsif L'Length > 3 then
	    Scanners(Scanners.Last_Index).Detected_Beacons.Append(Parse_Position(L));
	 end if;
      end;
   end loop;
   
   Find_All_Relative_Beacons(Scanners);
   for Scanner of Scanners loop
      for Beacon of Scanner.Transformed.Beacons loop
	 All_Beacons.Include(Beacon);
      end loop;
   end loop;
   
   Put(Natural(All_Beacons.Length)); New_Line;
end Advent_19;

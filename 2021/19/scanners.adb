with Positions; use Positions;
with Positions.Transforms; use Positions.Transforms;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;

with Ada.Text_Io; use Ada.Text_Io;

package body Scanners is
   package Position_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Position, Element_Type => Natural, Hash => Hash,
      Equivalent_Keys => "=", "=" => "=");
   subtype Position_Count_Map is Position_Maps.Map;

   procedure Find_All_Relative_Beacons(Scanners: in out Scanner_Vector) is
      function Scanner_Overlapped(S1, S2: Scanner; Tid: out Transform_Type; S2_Pos: out Position) return Boolean is
	 S1_Beacon_Positions: Position_Vector renames S1.Transformed.Beacons;
      begin
	 if S1_Beacon_Positions.Length = 0 then
	    return False;
	 end if;
	 
	 for T in All_Transforms'Range loop
	    declare
	       Trans_Pos: Position_Vector;
	       Position_Counts: Position_Count_Map;
	    begin
	       for P of S2.Detected_Beacons loop
		  Trans_Pos.Append(All_Transforms(T)(P));
	       end loop;
	       
	       for P1 of S1_Beacon_Positions loop
		  for P2 of Trans_Pos loop
		     declare
			Scanner_Pos: Position := P1 - P2;
		     begin
			if Position_Counts.Contains(Scanner_Pos) then
			   Position_Counts(Scanner_Pos) := Position_Counts(Scanner_Pos) + 1;
			   if Position_Counts(Scanner_Pos) >= 12 then
			      Tid := T;
			      S2_Pos := Scanner_Pos;
			      return True;
			   end if;
			else
			   Position_Counts.Insert(Scanner_Pos, 1);
			end if;
		     end;
		  end loop;
	       end loop;
	    end;
	 end loop;
	 return False;
      end Scanner_Overlapped;
      
      Not_Solved : Natural := Natural(Scanners.Length);
   begin
      if Scanners.Length <= 0 then
	 return;
      end if;
      
      -- we don't transform the position of the first scanner
      Scanners(Scanners.First_Index).Transformed.Transform_Id := 1;
      Scanners(Scanners.First_Index).Transformed.Beacons := Scanners(Scanners.First_Index).Detected_Beacons;
      
      Not_Solved := Not_Solved - 1;
      
      while Not_Solved > 0 loop
	 Put_Line(Natural'Image(Not_Solved));
	 for I in Scanners.First_Index + 1 .. Scanners.Last_Index loop
	    declare
	       Found: Boolean := False;
	    begin
	       if Scanners(I).Transformed.Beacons.Length > 0 then
		  Found := True;
	       end if;
	       
	       for Prev in Scanners.First_Index .. Scanners.Last_Index loop
		  exit when Found;
		  declare
		     Tid: Transform_Type;
		     Scanner_Pos: Position;
		     T_Beacons: Position_Vector renames Scanners(I).Transformed.Beacons;
		  begin
		     if Scanner_Overlapped(Scanners(Prev), Scanners(I), Tid, Scanner_Pos) then
			Found := True;
			Not_Solved := Not_Solved - 1;
			Put_Line("scanner " & Positive'Image(I) & " position: " & Image(Scanner_Pos));
			Scanners(I).Transformed.Transform_Id := Tid;
			T_Beacons.Clear;
			for P of Scanners(I).Detected_Beacons loop
			   T_Beacons.Append(Scanner_Pos + All_Transforms(Tid)(P));
			end loop;
		     end if;
		  end;
	       end loop;
	    end;
	 end loop;
      end loop;
   end Find_All_Relative_Beacons;
end Scanners;

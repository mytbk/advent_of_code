with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;

package body Height_Maps is
   function Is_Low_Point(H_Map: Height_Map; X,Y: Natural) return Boolean is
      Map: Map_Type renames H_Map.Map_Data;
   begin
      if (X /= 1 and then Map(X,Y) >= Map(X-1,Y)) or else
	(X /= H_Map.X_Last and then Map(X,Y) >= Map(X+1,Y)) or else
	(Y /= 1 and then Map(X,Y) >= Map(X,Y-1)) or else
	(Y /= H_Map.Y_Last and then Map(X,Y) >= Map(X,Y+1)) then
	 return False;
      else
	 return True;
      end if;
   end Is_Low_Point;
   
   function Find_All_Low_Points(H_Map : Height_Map) return Point_List is
      Pts: Point_List;
   begin
      for X in 1 .. H_Map.X_Last loop
	 for Y in 1 .. H_Map.Y_Last loop
	    if Is_Low_Point(H_Map, X, Y) then
	       Pts.Append((X => X, Y => Y));
	    end if;
	 end loop;
      end loop;
      return Pts;
   end Find_All_Low_Points;

   function Risc_Level(H_Map: Height_Map; Pts: Point_List) return Natural is
      Sum_Risc : Natural := 0;
      Map: Map_Type renames H_Map.Map_Data;
   begin
      for Pt of Pts loop
	 Sum_Risc := Sum_Risc + Map(Pt.x,Pt.Y) + 1;
      end loop;
      return Sum_Risc;
   end Risc_Level;
   
   function Generate_Basin_Map(H_Map: Height_Map; Pts: Point_List) return Basin_Map is
      B_Map: Basin_Map(H_Map.X_Last, H_Map.Y_Last);
      Bmap: Basin_Map_Type renames B_Map.Map_Data;
      Basin_Id: Integer := 0;
   begin
      Bmap := (others => (others => -1));
      for Pt of Pts loop
	 Fill_Basin(B_Map, H_Map, Pt.X, Pt.Y, Basin_Id);
	 Basin_Id := Basin_Id + 1;
      end loop;
      return B_Map;
   end Generate_Basin_Map;
   
   procedure Fill_Basin(B_Map: in out Basin_Map; H_Map: Height_Map; X,Y : Natural; Basin_Id : Integer) is
      Bmap: Basin_Map_Type renames B_Map.Map_Data;
      Hmap: Map_Type renames H_Map.Map_Data;
      X_Last: constant Positive := B_Map.X_Last;
      Y_Last: constant Positive := B_Map.Y_Last;
      
      procedure Try_Fill(X,Y: Natural) is
      begin
	 if Hmap(X,Y) = 9 then
	    return;
	 end if;
	 -- if (X,Y) can flow to neighboring cells which don't have basin_id, don't fill
	 if X /= 1 and then Hmap(X-1,Y) < Hmap(X,Y) and then Bmap(X-1,Y) /= Basin_Id then
	    return;
	 end if;
	 if X /= X_Last and then Hmap(X+1,Y) < Hmap(X,Y) and then Bmap(X+1,Y) /= Basin_Id then
	    return;
	 end if;
	 if Y /= 1 and then Hmap(X,Y-1) < Hmap(X,Y) and then Bmap(X,Y-1) /= Basin_Id then
	    return;
	 end if;
	 if Y /= Y_Last and then Hmap(X,Y+1) < Hmap(X,Y) and then Bmap(X,Y+1) /= Basin_Id then
	    return;
	 end if;
	 -- ok to fill, recursively call fill_basin
	 Fill_Basin(B_Map, H_Map, X, Y, Basin_Id);
      end Try_Fill;
   begin
      Bmap(X,Y) := Basin_Id;
      if X /= 1 and then Bmap(X-1,Y) = -1 then
	 Try_Fill(X-1,Y);
      end if;
      if X /= X_Last and then Bmap(X+1,Y) = -1 then
	 Try_Fill(X+1,Y);
      end if;
      if Y /= 1 and then Bmap(X,Y-1) = -1 then
	 Try_Fill(X,Y-1);
      end if;
      if Y /= Y_Last and then Bmap(X,Y+1) = -1 then
	 Try_Fill(X,Y+1);
      end if;
   end Fill_Basin;
   
   procedure Print_Basin_Map(B_Map: Basin_Map) is
      X_Last: constant Positive := B_Map.X_Last;
      Y_Last: constant Positive := B_Map.Y_Last;
   begin
      for X in 1 .. X_Last loop
	 for Y in 1 .. Y_Last loop
	    Put(B_Map.Map_Data(X,Y));
	 end loop;
	 New_Line;
      end loop;
   end;
end Height_Maps;

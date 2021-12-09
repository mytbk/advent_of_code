package body Height_Maps is
   function Find_All_Low_Points_Risc_Level(H_Map: Height_Map) return Natural is
      Sum_Risc : Natural := 0;
      Map: Map_Type renames H_Map.Map_Data;
      Is_Low_Point: Boolean;
   begin
      for I in 1 .. H_Map.X_Last loop
	 for J in 1 .. H_Map.Y_Last loop
	    Is_Low_Point := True;
	    if (I /= 1 and then Map(I,J) >= Map(I-1,J)) or else
	      (I /= H_Map.X_Last and then Map(I,J) >= Map(I+1,J)) or else
	      (J /= 1 and then Map(I,J) >= Map(I,J-1)) or else
	      (J /= H_Map.Y_Last and then Map(I,J) >= Map(I,J+1)) then
	       Is_Low_Point := False;
	    end if;
	    
	    if Is_Low_Point then
	       Sum_Risc := Sum_Risc + Map(I,J) + 1;
	    end if;
	 end loop;
      end loop;
      return Sum_Risc;
   end Find_All_Low_Points_Risc_Level;
   
   function Generate_Basin_Map(H_Map: Height_Map) return Basin_Map is
      B_Map: Basin_Map(H_Map.X_Last, H_Map.Y_Last);
   begin
      return B_Map;
   end Generate_Basin_Map;
end Height_Maps;

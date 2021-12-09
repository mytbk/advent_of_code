package Height_Maps is
   type Map_Type is array (Natural range <>, Natural range <>) of Natural;

   type Height_Map(X_Last, Y_Last : Positive) is record
      Map_Data : Map_Type (1 .. X_Last, 1 .. Y_Last);
   end record;
   
   type Basin_Map(X_Last, Y_Last : Positive) is new Height_Map(X_Last, Y_Last);
   
   function Find_All_Low_Points_Risc_Level(H_Map: Height_Map) return Natural;
end Height_Maps;

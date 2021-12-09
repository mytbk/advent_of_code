with Ada.Containers.Vectors;

package Height_Maps is
   type Map_Type is array (Natural range <>, Natural range <>) of Natural;
   
   type Point_Type is record
      X,Y: Positive;
   end record;
   
   package Point_Vectors is new Ada.Containers.Vectors
     (Element_Type => Point_Type, Index_Type => Natural);
   subtype Point_List is Point_Vectors.Vector;

   type Height_Map(X_Last, Y_Last : Positive) is record
      Map_Data : Map_Type (1 .. X_Last, 1 .. Y_Last);
   end record;
   
   type Basin_Map_Type is array (Natural range <>, Natural range <>) of Integer;
   type Basin_Map(X_Last, Y_Last : Positive) is record
      Map_Data : Basin_Map_Type(1 .. X_Last, 1 .. Y_Last);
   end record;
   
   function Find_All_Low_Points(H_Map : Height_Map) return Point_List;
   function Risc_Level(H_Map: Height_Map; Pts: Point_List) return Natural;
   function Generate_Basin_Map(H_Map: Height_Map; Pts: Point_List) return Basin_Map;
   procedure Print_Basin_Map(B_Map: Basin_Map);
private
   function Is_Low_Point(H_Map: Height_Map; X,Y: Natural) return Boolean;
   -- Mark basin map cell (X,Y) with Basin_Id, and its neighboring cells that will
   -- eventually only flow to (X,Y)
   procedure Fill_Basin(B_Map: in out Basin_Map; H_Map: Height_Map; X,Y: Natural; Basin_Id : Integer) with
     Pre => (B_Map.X_Last = H_Map.X_Last and then B_Map.Y_Last = H_Map.Y_Last);
end Height_Maps;

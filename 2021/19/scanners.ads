with Ada.Containers.Vectors;
with Positions; use Positions;
with Positions.Transforms; use Positions.Transforms;

package Scanners is
   package Position_Vectors is new Ada.Containers.Vectors
     (Element_Type => Position, Index_Type => Positive);
   subtype Position_Vector is Position_Vectors.Vector;
   
   type Position_Transform is record
      -- with transform All_Transforms(Transform_Id), the orientation of the scanner
      -- becomes the same as the first scanner
      Transform_Id : Transform_Type := 1;
      -- scanner position related to the first scanner
      Scanner_Position: Position;
      -- beacon positions related to the first scanner
      Beacons: Position_Vector;
   end record;
   
   type Scanner is record
      Detected_Beacons: Position_Vector;
      Transformed: Position_Transform;
   end record;
   
   package Scanner_Vectors is new Ada.Containers.Vectors
     (Element_Type => Scanner, Index_Type => Natural);
   subtype Scanner_Vector is Scanner_Vectors.Vector;
   
   -- calculate all the positions of the beacons relative to the first scanner
   procedure Find_All_Relative_Beacons(Scanners: in out Scanner_Vector);
end Scanners;

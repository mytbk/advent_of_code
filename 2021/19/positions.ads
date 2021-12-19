with Ada.Containers;

package Positions is
   type Position is record
      X,Y,Z: Integer;
   end record;
   
   function "+" (P1: Position; P2: Position) return Position is
     ((P1.X + P2.X, P1.Y + P2.Y, P1.Z + P2.Z));
   function "-" (P1: Position; P2: Position) return Position is
     ((P1.X - P2.X, P1.Y - P2.Y, P1.Z - P2.Z));
   
   function Image(P: Position) return String;
   
   function Hash(P: Position) return Ada.Containers.Hash_Type;
end Positions;

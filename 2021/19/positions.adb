with Ada.Containers; use Ada.Containers;

package body Positions is
   function Image(P: Position) return String is
   begin
      return "(" & Integer'Image(P.X) & "," & Integer'Image(P.Y) & "," & Integer'Image(P.Z) & ")";
   end Image;
   
   function Hash(P: Position) return Hash_Type is
   begin
      return (Hash_Type(abs P.X) * 1 + Hash_Type(abs P.Y) * 2 + Hash_Type(abs P.Z) * 4);
   end Hash;
end Positions;

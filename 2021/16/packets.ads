with Ada.Containers.Vectors;

package Packets is
   subtype Binary_Digit is Natural range 0 .. 1;
   type Binary_String is array (Positive range <>) of Binary_Digit;
   
   type Packet;
   type Packet_Access is access Packet;
   package Packet_Access_Vectors is new Ada.Containers.Vectors
     (Element_Type => Packet_Access, Index_Type => Positive);
   subtype Sub_Packet_Vector is Packet_Access_Vectors.Vector;
   
   type Packet_Value is range 0 .. 2 ** 63 - 1;
   type Packet is record
      Version : Natural;
      Type_Id: Natural;
      Literal_Value: Packet_Value;
      Sub_Packets: Sub_Packet_Vector;
   end record;
   
   function Parse_Packet(Packet_Bin: Binary_String; Packet_Last: out Positive) return Packet_Access;
   function Sum_Packet_Version(Pkt: Packet) return Natural;
   function Evaluate_Packet(Pkt: Packet) return Packet_Value;

   function Hex_Digit_To_Bin_String(C: Character) return Binary_String;
   function Hex_To_Bin_String(S: String) return Binary_String;
   
   function Binary_To_Number(S: Binary_String) return Natural;
   
end Packets;

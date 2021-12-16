with Ada.Text_Io; use Ada.Text_Io;
with Packets; use Packets;

procedure Advent_16 is
   Bin_String : Binary_String := Hex_To_Bin_String(Get_Line);
   Unused_Last: Positive;
   Pkt : Packet_Access := Parse_Packet(Bin_String, Unused_Last);
   Ver_Sum : constant Natural := Sum_Packet_Version(Pkt.all);
begin
   Put_Line(Natural'Image(Ver_Sum));
   Put_Line(Packet_Value'Image(Evaluate_Packet(Pkt.all)));
end Advent_16;

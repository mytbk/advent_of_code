with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Text_Io.Unbounded_Io; use Ada.Text_Io.Unbounded_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;
with Height_Maps; use Height_Maps;

procedure Advent_09 is
   subtype Map_Index is Positive;
   
   package String_Vectors is new Ada.Containers.Vectors
     (Element_Type => Unbounded_String, Index_Type => Map_Index);
   subtype String_Vector is String_Vectors.Vector;
   
   Height_Map_Strings: String_Vector;
   
   function To_Height_Map(Svec: String_Vector) return Height_Map is
      X_Last : constant Positive := Positive(String_Vectors.Length(Svec));
      Y_Last : constant Positive := Length(Svec(1));
      Hm: Height_Map(X_Last, Y_Last);
      Map: Map_Type renames Hm.Map_Data;
   begin
      for I in 1 .. X_Last loop
	 declare
	    S: constant String := To_String(Svec(I));
	 begin
	    for J in 1 .. Y_Last loop
	       Map(I, J) := Character'Pos(S(J)) - Character'Pos('0');
	    end loop;
	 end;
      end loop;
      return Hm;
   end To_Height_Map;
   
begin
   while not End_Of_File loop
      Height_Map_Strings.Append(Get_Line);
   end loop;
   
   declare
      H_Map: Height_Map := To_Height_Map(Height_Map_Strings);
   begin
      Put(Find_All_Low_Points_Risc_Level(H_Map));
   end;
   
end Advent_09;

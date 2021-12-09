with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Text_Io.Unbounded_Io; use Ada.Text_Io.Unbounded_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;
with Height_Maps; use Height_Maps;
with Ada.Containers.Generic_Array_Sort;

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
   
   type Natural_Array is array (Natural range <>) of Natural;
   procedure Integer_Sort is new Ada.Containers.Generic_Array_Sort
	(Index_Type => Natural, Element_Type => Natural, Array_Type => Natural_Array, "<" => ">");
begin
   while not End_Of_File loop
      Height_Map_Strings.Append(Get_Line);
   end loop;
   
   declare
      H_Map: Height_Map := To_Height_Map(Height_Map_Strings);
      Low_Points: Point_List := Find_All_Low_Points(H_Map);
      
      -- part 2
      B_Map: Basin_Map := Generate_Basin_Map(H_Map, Low_Points);
      Basin_Sizes: Natural_Array(0 .. Positive(Low_Points.Length) - 1) := (others => 0);
      Sorted_Basin_Sizes : Natural_Array(Basin_Sizes'Range);
      Size_Multiplied : Natural := 1;
   begin
      Put(Risc_Level(H_Map, Low_Points));
      New_Line;
      -- Print_Basin_Map(B_Map);
      for X in 1 .. B_Map.X_Last loop
	 for Y in 1 .. B_Map.Y_Last loop
	    if B_Map.Map_Data(X,Y) >= 0 then
	       Basin_Sizes(B_Map.Map_Data(X,Y)) := Basin_Sizes(B_Map.Map_Data(X,Y)) + 1;
	    end if;
	 end loop;
      end loop;
      
      Sorted_Basin_Sizes := Basin_Sizes;
      Integer_Sort(Sorted_Basin_Sizes);
      --  for I of Sorted_Basin_Sizes loop
      --  	 Put(I);
      --  end loop;
      --  New_Line;
      for I in 0 .. 2 loop
	 Size_Multiplied := Size_Multiplied * Sorted_Basin_Sizes(I);
      end loop;
      Put(Size_Multiplied);
      New_Line;
   end;
   
end Advent_09;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Text_Io; use Ada.Text_Io;

procedure Advent_11 is
   package Galaxy_Images is new Ada.Containers.Vectors
     (Element_Type => Unbounded_String, Index_Type => Positive);
   
   subtype Galaxy_Image is Galaxy_Images.Vector;
   
   type Point is record
      X, Y : Positive;
   end record;
   
   package Point_Vectors is new Ada.Containers.Vectors
     (Element_Type => Point, Index_Type => Positive);

   subtype Point_Vector is Point_Vectors.Vector;
   
   Original_Image : Galaxy_Image;
   Galaxy_Positions_1 : Point_Vector;
   Galaxy_Positions_2 : Point_Vector;
   
   subtype Result_Type is Long_Long_Integer;
   
   function Get_Sum_Of_Distances (Points : Point_Vector) return Result_Type is
      Sum : Result_Type := 0;
   begin
      for I in Points.First_Index .. Points.Last_Index loop
	 for J in I + 1 .. Points.Last_Index loop
	    Sum := Sum +
	      Result_Type(abs(Points(I).X - Points(J).X)) +
	      Result_Type(abs(Points(I).Y - Points(J).Y));
	 end loop;
      end loop;
      return Sum;
   end Get_Sum_Of_Distances;

   type Boolean_Map is array (Positive range <>) of Boolean;
   -- get the galaxy positions of the expanded image
   procedure Expand(Original_Image : Galaxy_Image;
		    Height, Width : Positive;
		    Is_Row_Empty, Is_Column_Empty : Boolean_Map;
		    Expand_Times : Positive;
		    Galaxy_Positions : in out Point_Vector) is
      Real_X, Real_Y : Natural := 0;
   begin
      Galaxy_Positions.Clear;
      for I in 1 .. Height loop
	 if Is_Row_Empty(I) then
	    Real_X := Real_X + Expand_Times;
	 else
	    Real_X := Real_X + 1;
	    Real_Y := 0;
	    for J in 1 .. Width loop
	       if Is_Column_Empty(J) then
		  Real_Y := Real_Y + Expand_Times;
	       else
		  Real_Y := Real_Y + 1;
		  if Element(Original_Image(I), J) = '#' then
		     Galaxy_Positions.Append
		       (Point'(X => Real_X, Y => Real_Y));
		  end if;
	       end if;
	    end loop;
	 end if;
      end loop;
   end Expand;

begin
   while not End_Of_File loop
      Original_Image.Append(To_Unbounded_String(Get_Line));
   end loop;
   
   if Original_Image.Length = 0 then
      return;
   end if;

   declare
      Height : constant Natural := Natural(Original_Image.Length);
      Width : constant Natural := Length(Original_Image(1));
      
      subtype Empty_Row_Map is Boolean_Map (1 .. Height);
      subtype Empty_Column_Map is Boolean_Map (1 .. Width);
      Is_Row_Empty : Empty_Row_Map := (others => False);
      Is_Column_Empty : Empty_Column_Map := (others => False);
   begin
      for I in 1 .. Height loop
	 declare
	    Has_Galaxy : Boolean := False;
	 begin
	    for J in 1 .. Width loop
	       if Element(Original_Image(I), J) = '#' then
		  Has_Galaxy := True;
		  exit;
	       end if;
	    end loop;
	    if not Has_Galaxy then
	       Is_Row_Empty(I) := True;
	    end if;
	 end;
      end loop;
      
      for J in 1 .. Width loop
	 declare
	    Has_Galaxy : Boolean := False;
	 begin
	    for I in 1 .. Height loop
	       if Element(Original_Image(I), J) = '#' then
		  Has_Galaxy := True;
		  exit;
	       end if;
	    end loop;
	    if not Has_Galaxy then
	       Is_Column_Empty(J) := True;
	    end if;
	 end;
      end loop;
      
      Expand(Original_Image,
	     Height, Width,
	     Is_Row_Empty, Is_Column_Empty,
	     2,
	     Galaxy_Positions_1);
      Expand(Original_Image,
	     Height, Width,
	     Is_Row_Empty, Is_Column_Empty,
	     100_0000,
	     Galaxy_Positions_2);
   end;
   
   Put_Line(Result_Type'Image(Get_Sum_Of_Distances(Galaxy_Positions_1)));
   Put_Line(Result_Type'Image(Get_Sum_Of_Distances(Galaxy_Positions_2)));
end Advent_11;

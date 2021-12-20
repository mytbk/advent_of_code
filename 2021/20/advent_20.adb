with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

procedure Advent_20 is
   subtype Pixel is Natural range 0 .. 1;
   type Enhancement_Program is array (0 .. 511) of Pixel;
   
   Enhancement: Enhancement_Program;
   
   package Pixel_Vectors is new Ada.Containers.Vectors
     (Element_Type => Pixel, Index_Type => Natural);
   use Pixel_Vectors;

   subtype Image_Row is Pixel_Vectors.Vector;
   package Images is new Ada.Containers.Vectors
     (Element_Type => Image_Row, Index_Type => Natural);
   
   subtype Image is Images.Vector;
   
   Input_Image: Image;
   
   function Enhance(E: Enhancement_Program; Input_Image: Image; Surrounding: in out Pixel) return Image is
      Input_Height: constant Natural := Natural(Input_Image.Length);
      Input_Width : constant Natural := Natural(Input_Image(0).Length);
      
      function Img_Slice(X,Y: Integer) return Natural is
	 Px,Py: Integer;
	 Sum: Natural := 0;
	 Pix: Pixel;
      begin
	 for I in -1 .. 1 loop
	    for J in -1 .. 1 loop
	       Px := X + I;
	       Py := Y + J;
	       if Px < 0 or else Px >= Input_Height or else Py < 0 or else Py >= Input_Width then
		  Pix := Surrounding;
	       else
		  Pix := Input_Image(Px)(Py);
	       end if;
	       Sum := Sum * 2 + Pix;
	    end loop;
	 end loop;
	 return Sum;
      end Img_Slice;
      
      Output_Image: Image;
   begin
      Output_Image.Set_Length(Count_Type(Input_Height + 2));
      for I in 0 .. Input_Height + 1 loop
	 Output_Image(I).Set_Length(Count_Type((Input_Width + 2)));
      end loop;
      for I in -1 .. Input_Height loop
	 for J in -1 .. Input_Width loop
	    Output_Image(I+1)(J+1) := E(Img_Slice(I,J));
	 end loop;
      end loop;
      
      Surrounding := E(Img_Slice(-2,-2));
      return Output_Image;
   end Enhance;
   
   Surrounding : Pixel := 0;
   Count_Lit: Natural := 0;
begin
   declare
      E_String: String := Get_Line;
   begin
      for I in 0 .. 511 loop
	 if E_String(I + 1) = '#' then
	    Enhancement(I) := 1;
	 else
	    Enhancement(I) := 0;
	 end if;
      end loop;
   end;
   
   while not End_Of_File loop
      declare
	 L: String := Get_Line;
	 Row: Image_Row;
      begin
	 if L'Length > 0 then
	    for C of L loop
	       Row.Append(if C = '#' then 1 else 0);
	    end loop;
	    Input_Image.Append(Row);
	 end if;
      end;
   end loop;
   
   -- part 1: enhance twice
   Input_Image := Enhance(Enhancement, Input_Image, Surrounding);
   -- Put_Line(Pixel'Image(Surrounding));
   Input_Image := Enhance(Enhancement, Input_Image, Surrounding);
   
   -- part 2: 48 times more ...
   for I in 1 .. 48 loop
      Input_Image := Enhance(Enhancement, Input_Image, Surrounding);
   end loop;

   for Row of Input_Image loop
      for Pix of Row loop
	 if Pix = 1 then
	    Count_Lit := Count_Lit + 1;
	    --Put('#');
	 else
	    --Put('.');
	    null;
	 end if;
      end loop;
      --New_Line;
   end loop;
   Put_Line(Pixel'Image(Surrounding));
   Put_Line(Natural'Image(Count_Lit));
end Advent_20;

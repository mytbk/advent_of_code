with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Text_Io; use Ada.Text_Io;

procedure Advent_08 is
   package String_Vectors is new Ada.Containers.Vectors
     (Element_Type => Unbounded_String, Index_Type => Positive);
   subtype Tree_Grid is String_Vectors.Vector;
   
   Grid : Tree_Grid;
   
   function Visible_From_Left(I, J : Positive) return Boolean is
      S : Unbounded_String renames Grid(I);
   begin
      for Col in 1 .. J - 1 loop
	 if Element(S,Col) >= Element(S,J) then
	    return False;
	 end if;
      end loop;
      return True;
   end Visible_From_Left;
   
   function Left_View_Distance(I, J : Positive) return Natural is
      S : Unbounded_String renames Grid(I);
      Dist : Natural := 0;
   begin
      for Col in reverse 1 .. J - 1 loop
	 Dist := Dist + 1;
	 exit when Element(S, Col) >= Element(S, J);
      end loop;
      return Dist;
   end Left_View_Distance;
   
   function Visible_From_Right(I, J: Positive) return Boolean is
      S : Unbounded_String renames Grid(I);
   begin
      for Col in J + 1 .. Length(S) loop
	 if Element(S, Col) >= Element(S, J) then
	    return False;
	 end if;
      end loop;
      return True;
   end Visible_From_Right;
   
   function Right_View_Distance(I, J : Positive) return Natural is
      S : Unbounded_String renames Grid(I);
      Dist : Natural := 0;
   begin
      for Col in J + 1 .. Length(S) loop
	 Dist := Dist + 1;
	 exit when Element(S, Col) >= Element(S, J);
      end loop;
      return Dist;
   end Right_View_Distance;
   
   function Visible_From_Top(I, J: Positive) return Boolean is
      H : constant Character := Element(Grid(I), J);
   begin
      for Line in 1 .. I - 1 loop
	 declare
	    S : Unbounded_String renames Grid(Line);
	 begin
	    if J <= Length(S) and then Element(S, J) >= H then
	       return False;
	    end if;
	 end;
      end loop;
      return True;
   end Visible_From_Top;
   
   function Top_View_Distance(I, J : Positive) return Natural is
      H : constant Character := Element(Grid(I), J);
      Dist : Natural := 0;
   begin
      for Line in reverse 1 .. I - 1 loop
	 declare
	    S : Unbounded_String renames Grid(Line);
	 begin
	    Dist := Dist + 1;
	    exit when J <= Length(S) and then Element(S, J) >= H;
	 end;
      end loop;
      return Dist;
   end Top_View_Distance;
   
   function Visible_From_Bottom(I, J: Positive) return Boolean is
      H : constant Character := Element(Grid(I), J);
   begin
      for Line in I + 1 .. Natural(String_Vectors.Length(Grid)) loop
	 declare
	    S : Unbounded_String renames Grid(Line);
	 begin
	    if J <= Length(S) and then Element(S, J) >= H then
	       return False;
	    end if;
	 end;
      end loop;
      return True;
   end Visible_From_Bottom;
   
   function Bottom_View_Distance(I, J : Positive) return Natural is
      H : constant Character := Element(Grid(I), J);
      Dist : Natural := 0;
   begin
      for Line in I + 1 .. Natural(String_Vectors.Length(Grid)) loop
	 declare
	    S : Unbounded_String renames Grid(Line);
	 begin
	    Dist := Dist + 1;
	    exit when J <= Length(S) and then Element(S, J) >= H;
	 end;
      end loop;
      return Dist;
   end Bottom_View_Distance;
   
   function Visible(I, J : Positive) return Boolean is
   begin
      return Visible_From_Left(I, J) or else
	Visible_From_Right(I, J) or else
	Visible_From_Top(I, J) or else
	Visible_From_Bottom(I, J);
   end Visible;
   
   function Scenic_Score(I, J : Positive) return Natural is
   begin
      return Left_View_Distance(I, J) * Right_View_Distance(I, J)
	* Top_View_Distance(I, J) * Bottom_View_Distance(I, J);
   end Scenic_Score;
   
   Num_Visible : Natural := 0;
   Max_Scenic_Score : Natural := 0;
begin
   while not End_Of_File loop
      Grid.Append(To_Unbounded_String(Get_Line));
   end loop;
   
   for I in 1 .. Natural(String_Vectors.Length(Grid)) loop
      declare
	 S : Unbounded_String renames Grid(I);
	 L : constant Positive := Length(S);
      begin
	 for J in 1 .. L loop
	    if Visible(I, J) then
	       Num_Visible := Num_Visible + 1;
	    end if;
	 end loop;
      end;
   end loop;
   
  for I in 1 .. Natural(String_Vectors.Length(Grid)) loop
      declare
	 S : Unbounded_String renames Grid(I);
	 L : constant Positive := Length(S);
	 Score : Natural;
      begin
	 for J in 1 .. L loop
	    Score := Scenic_Score(I, J);
	    if Score > Max_Scenic_Score then
	       Max_Scenic_Score := Score;
	    end if;
	 end loop;
      end;
   end loop;
   
   Put_Line(Natural'Image(Num_Visible));
   Put_Line(Natural'Image(Max_Scenic_Score));
	    
end Advent_08;

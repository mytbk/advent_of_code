with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers.Vectors;

procedure Advent_05 is
   subtype Coordinate is Natural range 0 .. 999;
   type Point_Type is record
      X, Y: Coordinate;
   end record;
   
   type Segment_Type is record
      P1, P2 : Point_Type;
   end record;
   
   -- parse p1x,p1y
   function Parse_Point(S: String) return Point_Type is
      Point: Point_Type;
      Unused_Last: Positive;
   begin
      for I in S'Range loop
	 if S(I) = ',' then
	    Get(S(S'First .. I - 1), Point.X, Unused_Last);
	    Get(S(I + 1 .. S'Last), Point.Y, Unused_Last);
	 end if;
      end loop;
      return Point;
   end Parse_Point;
   
   -- parse "p1x,p1y -> p2x,p2y"
   function Parse_Segment(S: String) return Segment_Type is
      Seg: Segment_Type;
   begin
      for I in S'Range loop
	 if S(I .. I + 1) = "->" then
	    Seg.P1 := Parse_Point(S(S'First .. I - 1));
	    Seg.P2 := Parse_Point(S(I + 2 .. S'Last));
	    return Seg;
	 end if;
      end loop;
      raise Data_Error;
   end Parse_Segment;
   
   type Diagram_Row is array (Coordinate) of Natural;
   type Diagram_Type is array (Coordinate) of Diagram_Row;

   package Segment_Vectors is new Ada.Containers.Vectors
     (Element_Type => Segment_Type, Index_Type => Natural);
   subtype Segment_Vector is Segment_Vectors.Vector;
   
   procedure Mark_Vertical(Diagram: in out Diagram_Type; Col, X1, X2: Coordinate) is
      Minx: constant Coordinate := Coordinate'Min(X1, X2);
      Maxx: constant Coordinate := Coordinate'Max(X1, X2);
   begin
      for X in Minx .. Maxx loop
	 Diagram(X)(Col) := Diagram(X)(Col) + 1;
      end loop;
   end Mark_Vertical;
   
   procedure Mark_Horizontal(Diagram: in out Diagram_Type; Row, Y1, Y2: Coordinate) is
      Miny: constant Coordinate := Coordinate'Min(Y1, Y2);
      Maxy: constant Coordinate := Coordinate'Max(Y1, Y2);
   begin
      for Y in Miny .. Maxy loop
	 Diagram(Row)(Y) := Diagram(Row)(Y) + 1;
      end loop;
   end Mark_Horizontal;
   
   procedure Mark_Diagonal(Diagram: in out Diagram_Type; X1, Y1, X2, Y2: Coordinate) is
      Xstep, Ystep : Integer;
      Xcur : Coordinate := X1;
      Ycur : Coordinate := Y1;
   begin
      if X2 - X1 = Y2 - Y1 or else X2 - X1 + Y2 - Y1 = 0 then
	 if X2 > X1 then
	    Xstep := 1;
	 else
	    Xstep := -1;
	 end if;
	 if Y2 > Y1 then
	    Ystep := 1;
	 else
	    Ystep := -1;
	 end if;
	 
	 loop
	    Diagram(Xcur)(Ycur) := Diagram(Xcur)(Ycur) + 1;
	    exit when Xcur = X2;
	    Xcur := Xcur + Xstep;
	    Ycur := Ycur + Ystep;
	 end loop;
      end if;
   end Mark_Diagonal;
   
   procedure Mark_Diagram(Diagram: in out Diagram_Type; Seg: Segment_Type) is
   begin
      if Seg.P1.X = Seg.P2.X then
	 Mark_Horizontal(Diagram, Row => Seg.P1.X, Y1 => Seg.P1.Y, Y2 => Seg.P2.Y);
      elsif Seg.P1.Y = Seg.P2.Y then
	 Mark_Vertical(Diagram, Col => Seg.P1.Y, X1 => Seg.P1.X, X2 => Seg.P2.X);
      end if;
   end Mark_Diagram;
   
   procedure Mark_Diagram2(Diagram: in out Diagram_Type; Seg: Segment_Type) is
   begin
      Mark_Diagram(Diagram, Seg);
      if Seg.P1.X /= Seg.P2.X and then Seg.P1.Y /= Seg.P2.Y then
	 Mark_Diagonal(Diagram => Diagram, X1 => Seg.P1.X, X2 => Seg.P2.X, Y1 => Seg.P1.Y, Y2 => Seg.P2.Y);
      end if;
   end Mark_Diagram2;
   
   function Count_Overlapped(Diagram: Diagram_Type) return Natural is
      Count: Natural := 0;
   begin
      for Row of Diagram loop
	 for Cell of Row loop
	    if Cell > 1 then
	       Count := Count + 1;
	    end if;
	 end loop;
      end loop;
      return Count;
   end Count_Overlapped;
   
   Segments : Segment_Vector;	       
   Diagram : Diagram_Type := (others => (others => 0));
   Diagram2 : Diagram_Type := (others => (others => 0));
begin
   while not End_Of_File loop
      declare
	 L: constant String := Get_Line;
	 Seg: constant Segment_Type := Parse_Segment(L);
      begin
	 Segments.Append(Seg);
      end;
   end loop;
   
   for Seg of Segments loop
      Mark_Diagram(Diagram, Seg);
      Mark_Diagram2(Diagram2, Seg);
   end loop;
   
   Put(Count_Overlapped(Diagram));
   Put(Count_Overlapped(Diagram2));
   
   -- Print the final diagram for the test input
   --  New_Line;
   --  for I in 0 .. 9 loop
   --     for J in 0 .. 9 loop
   --  	 Put(Diagram2(I)(J));
   --     end loop;
   --     New_Line;
   --  end loop;
   
end Advent_05;

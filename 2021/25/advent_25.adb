with Ada.Text_Io; use Ada.Text_Io;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Advent_25 is
   package String_Vectors is new Ada.Containers.Vectors
     (Element_Type => Unbounded_String, Index_Type => Positive);
   
   Map: String_Vectors.Vector;
   
   function Simulate return Boolean is
      Height: Positive := Positive(Map.Length);
      Width: Positive := Length(Map(1));
      Can_Move_Right: array (1 .. Width) of Boolean;
      Can_Move_Down: array (1 .. Height) of Boolean;
      Moved: Boolean := False;
   begin
      for H in 1 .. Height loop
	 Can_Move_Right := (others => False);
	 declare
	    Line: String(1 .. Width) := To_String(Map(H));
	 begin
	    for I in 1 .. Width loop
	       if Line(I) = '>' then
		  if I < Width and then Line(I+1) = '.' then
		     Can_Move_Right(I) := True;
		  elsif I = Width and then Line(1) = '.' then
		     Can_Move_Right(I) := True;
		  end if;
	       end if;
	    end loop;
	 end;
	 for I in 1 .. Width loop
	    if Can_Move_Right(I) then
	       Replace_Element(Map(H), I, '.');
	       if I < Width then
		  Replace_Element(Map(H), I + 1, '>');
	       else
		  Replace_Element(Map(H), 1, '>');
	       end if;
	       Moved := True;
	    end if;
	 end loop;
      end loop;
      
      for W in 1 .. Width loop
	 Can_Move_Down := (others => False);
	 for H in 1 .. Height loop
	    if Element(Map(H), W) = 'v' then
	       if H < Height and then Element(Map(H + 1), W) = '.' then
		  Can_Move_Down(H) := True;
	       elsif H = Height and then Element(Map(1), W) = '.' then
		  Can_Move_Down(H) := True;
	       end if;
	    end if;
	 end loop;
	 
	 for H in 1 .. Height loop
	    if Can_Move_Down(H) then
	       Replace_Element(Map(H), W, '.');
	       if H < Height then
		  Replace_Element(Map(H + 1), W, 'v');
	       else
		  Replace_Element(Map(1), W, 'v');
	       end if;
	       Moved := True;
	    end if;
	 end loop;
      end loop;
      
      return Moved;
   end Simulate;
   
   Steps: Natural := 0;
begin
   while not End_Of_File loop
      Map.Append(To_Unbounded_String(Get_Line));
   end loop;
   
   while Simulate loop
      Steps := Steps + 1;
      --  Put_Line(Natural'Image(Steps));
      --  for Row of Map loop
      --  	 Put_Line(To_String(Row));
      --  end loop;
      --  New_Line;
   end loop;
   
   Put_Line(Natural'Image(Steps + 1));
end Advent_25;

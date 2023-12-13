with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_Io; use Ada.Text_Io;

procedure Advent_13 is
   package String_Vectors is new Ada.Containers.Vectors
     (Element_Type => Unbounded_String, Index_Type => Positive);
   
   type Pattern is record
      Content : String_Vectors.Vector;
   end record;
   
   function Num_Rows(P : Pattern) return Positive is
   begin
      return Positive(P.Content.Length);
   end Num_Rows;
   
   function Num_Columns(P : Pattern) return Positive is
   begin
      return Length(P.Content(1));
   end Num_Columns;
   
   function Element(P : Pattern; X, Y : Positive) return Character is
   begin
      return Element(P.Content(X), Y);
   end Element;
   
   procedure Get_Pattern(P : out Pattern) is
   begin
      P.Content.Clear;
      while not End_Of_File loop
	 declare
	    S : constant String := Get_Line;
	 begin
	    if S'Length = 0 then
	       return;
	    end if;
	    P.Content.Append(To_Unbounded_String(S));
	 end;
      end loop;
   end Get_Pattern;

   function Summarize(P : Pattern; Num_Smudge : Natural := 0) return Natural is
      R : constant Positive := Num_Rows (P);
      C : constant Positive := Num_Columns (P);
      
      function Has_Vertical_Line (I : Positive) return Boolean is
	 Num_Diff : Natural := 0;
      begin
	 for J in 1 .. I loop
	    declare
	       Mirrored_Column : constant Positive := I * 2 + 1 - J;
	    begin
	       if Mirrored_Column <= C then
		  for K in 1 .. R loop
		     if Element (P, K, J) /= Element (P, K, Mirrored_Column) then
			Num_Diff := Num_Diff + 1;
			if Num_Diff > Num_Smudge then
			   return False;
			end if;
		     end if;
		  end loop;
	       end if;
	    end;
	 end loop;
	 return Num_Diff = Num_Smudge;
      end Has_Vertical_Line;
      
      function Has_Horizontal_Line (I : Positive) return Boolean is
	 Num_Diff : Natural := 0;
      begin
	 for J in 1 .. I loop
	    declare
	       Mirrored_Row : constant Positive := I * 2 + 1 - J;
	    begin
	       if Mirrored_Row <= R then
		  for K in 1 .. C loop
		     if Element (P, J, K) /= Element (P, Mirrored_Row, K) then
			Num_Diff := Num_Diff + 1;
			if Num_Diff > Num_Smudge then
			   return False;
			end if;
		     end if;
		  end loop;
	       end if;
	    end;
	 end loop;
	 return Num_Diff = Num_Smudge;
      end Has_Horizontal_Line;
   begin
      -- Find vertical line of reflection
      for I in 1 .. C - 1 loop
	 if Has_Vertical_Line(I) then
	    return I;
	 end if;
      end loop;
      
      -- Find horizontal line of reflection
      for I in 1 .. R - 1 loop
	 if Has_Horizontal_Line(I) then
	    return I * 100;
	 end if;
      end loop;
      
      -- no mirror is found
      return 0;
   end Summarize;
   
   Sum_1 : Natural := 0;
   Sum_2 : Natural := 0;
   
   P : Pattern;   
begin
   while not End_Of_File loop
      Get_Pattern(P);
      Sum_1 := Sum_1 + Summarize(P);
      Sum_2 := Sum_2 + Summarize(P, 1);
   end loop;
   Put_Line(Natural'Image(Sum_1));
   Put_Line(Natural'Image(Sum_2));
end Advent_13;

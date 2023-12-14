with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_Io; use Ada.Text_Io;

procedure Advent_14 is
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
   
   procedure Set_Element (P : in out Pattern; X, Y : Positive; C : Character) is
   begin
      Replace_Element(P.Content(X), Y, C);
   end Set_Element;

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
   
   Original_Rock_Pattern : Pattern;
   Next_Pattern : Pattern;
   Sum_Of_Load : Natural := 0;
   
   procedure Roll_North (Rock_Pattern : in out Pattern) is
   begin
      for Line in 1 .. Num_Rows(Rock_Pattern) loop
	 for Column in 1 .. Num_Columns(Rock_Pattern) loop
	    if Element(Rock_Pattern, Line, Column) = 'O' then
	       declare
		  Line_To_Roll : Positive := Line;
	       begin
		  for North_Line in reverse 1 .. Line - 1 loop
		     if Element(Rock_Pattern, North_Line, Column) = '.' then
			Line_To_Roll := North_Line;
		     else
			exit;
		     end if;
		  end loop;
		  
		  if Line_To_Roll /= Line then
		     Set_Element(Rock_Pattern, Line_To_Roll, Column, 'O');
		     Set_Element(Rock_Pattern, Line, Column, '.');
		  end if;
	       end;
	    end if;
	 end loop;
      end loop;
   end Roll_North;
   
   procedure Roll_West (Rock_Pattern : in out Pattern) is
   begin
      for Line in 1 .. Num_Rows(Rock_Pattern) loop
	 for Column in 1 .. Num_Columns(Rock_Pattern) loop
	    if Element(Rock_Pattern, Line, Column) = 'O' then
	       declare
		  Column_To_Roll : Positive := Column;
	       begin
		  for West_Column in reverse 1 .. Column - 1 loop
		     if Element(Rock_Pattern, Line, West_Column) = '.' then
			Column_To_Roll := West_Column;
		     else
			exit;
		     end if;
		  end loop;
		  
		  if Column_To_Roll /= Column then
		     Set_Element(Rock_Pattern, Line, Column_To_Roll, 'O');
		     Set_Element(Rock_Pattern, Line, Column, '.');
		  end if;
	       end;
	    end if;
	 end loop;
      end loop;
   end Roll_West;
   
   procedure Roll_South (Rock_Pattern : in out Pattern) is
   begin
      for Line in reverse 1 .. Num_Rows(Rock_Pattern) loop
	 for Column in 1 .. Num_Columns(Rock_Pattern) loop
	    if Element(Rock_Pattern, Line, Column) = 'O' then
	       declare
		  Line_To_Roll : Positive := Line;
	       begin
		  for South_Line in Line + 1 .. Num_Rows(Rock_Pattern) loop
		     if Element(Rock_Pattern, South_Line, Column) = '.' then
			Line_To_Roll := South_Line;
		     else
			exit;
		     end if;
		  end loop;
		  
		  if Line_To_Roll /= Line then
		     Set_Element(Rock_Pattern, Line_To_Roll, Column, 'O');
		     Set_Element(Rock_Pattern, Line, Column, '.');
		  end if;
	       end;
	    end if;
	 end loop;
      end loop;
   end Roll_South;
   
   procedure Roll_East (Rock_Pattern : in out Pattern) is
   begin
      for Line in  1 .. Num_Rows(Rock_Pattern) loop
	 for Column in reverse 1 .. Num_Columns(Rock_Pattern) loop
	    if Element(Rock_Pattern, Line, Column) = 'O' then
	       declare
		  Column_To_Roll : Positive := Column;
	       begin
		  for East_Column in Column + 1 .. Num_Columns(Rock_Pattern) loop
		     if Element(Rock_Pattern, Line, East_Column) = '.' then
			Column_To_Roll := East_Column;
		     else
			exit;
		     end if;
		  end loop;
		  
		  if Column_To_Roll /= Column then
		     Set_Element(Rock_Pattern, Line, Column_To_Roll, 'O');
		     Set_Element(Rock_Pattern, Line, Column, '.');
		  end if;
	       end;
	    end if;
	 end loop;
      end loop;
   end Roll_East;
   
   function Is_Identical (P1, P2 : Pattern) return Boolean is
      N_Row : constant Positive := Num_Rows(P1);
   begin
      if N_Row /= Num_Rows(P2) then
	 return False;
      end if;
      for I in 1 .. N_Row loop
	 if P1.Content(I) /= P2.Content(I) then
	    return False;
	 end if;
      end loop;
      return True;
   end Is_Identical;

   function Total_Load (Rock_Pattern : Pattern) return Natural is
      Sum_Of_Load : Natural := 0;
   begin
      -- calculate the load
      for Line in 1 .. Num_Rows(Rock_Pattern) loop
	 for Column in 1 .. Num_Columns(Rock_Pattern) loop
	    if Element(Rock_Pattern, Line, Column) = 'O' then
	       Sum_Of_Load := Sum_Of_Load + (Num_Rows(Rock_Pattern) - Line + 1);
	    end if;
	 end loop;
      end loop;
      return Sum_Of_Load;
   end Total_Load;
   
   package Pattern_Vectors is new Ada.Containers.Vectors
     (Element_Type => Pattern, Index_Type => Positive);
   
   Prev_Patterns : Pattern_Vectors.Vector;
   Has_Same : Boolean := False;
   Last_Same : Positive;
   Loop_Size : Positive;

begin
   Get_Pattern (Original_Rock_Pattern);
   
   -- part 1
   Next_Pattern := Original_Rock_Pattern;
   Roll_North(Next_Pattern);
   Sum_Of_Load := Total_Load(Next_Pattern);   
   Put_Line(Natural'Image(Sum_Of_Load));
   
   -- part 2 : we need to find a loop of patterns
   Next_Pattern := Original_Rock_Pattern;
   for I in 1 .. 1_000_000_000 loop
      Roll_North(Next_Pattern);
      Roll_West(Next_Pattern);
      Roll_South(Next_Pattern);
      Roll_East(Next_Pattern);
      Prev_Patterns.Append(Next_Pattern);
      for J in reverse
	Prev_Patterns.First_Index .. Prev_Patterns.Last_Index - 1 loop
	 if Is_Identical(Next_Pattern, Prev_Patterns(J)) then
	    Has_Same := True;
	    Last_Same := J;
	    Loop_Size := Prev_Patterns.Last_Index - J;
	    exit;
	 end if;
      end loop;
      exit when Has_Same;
   end loop;
   
   if Has_Same then
      declare
	 Final_Pattern_Index : constant Positive :=
	   Last_Same + (1_000_000_000 - Last_Same) mod Loop_Size;
      begin
	 Put_Line(Natural'Image
		    (Total_Load(Prev_Patterns(Final_Pattern_Index))));
      end;
   end if;
end Advent_14;

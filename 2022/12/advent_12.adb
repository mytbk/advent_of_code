with Ada.Text_Io; use Ada.Text_Io;
with Ada.Containers.Vectors;
with Ada.Containers.Doubly_Linked_Lists;

procedure Advent_12 is
   type Grid_Info is record
      Height : Integer;
      Is_Start : Boolean := False;
      Is_End : Boolean := False;
      Searched : Boolean := False;
   end record;
   
   type Search_Info is record
      X, Y : Natural;
      Distance : Natural;
   end record;
   
   package Grid_Info_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Grid_Info);
   
   subtype Grid_Info_Vector is Grid_Info_Vectors.Vector;
   function "=" (A, B : Grid_Info_Vector) return Boolean is
   begin
      return False;
   end "=";
   
   package Grid_Info_Matrices is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Grid_Info_Vectors.Vector);
   
   subtype Grid_Matrix is Grid_Info_Matrices.Vector;
   
   package Search_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Search_Info);
   
   Input_Grid : Grid_Matrix;
   Start_X, Start_Y : Natural;
   Search_List : Search_Lists.List;
   
   procedure Do_Bfs is
   begin
      while not Search_List.Is_Empty loop
	 declare
	    Grid_Info : constant Search_Info := Search_List.First_Element;
	    Current_Height : constant Integer := Input_Grid(Grid_Info.X)(Grid_Info.Y).Height;
	    New_Dist : Natural := Grid_Info.Distance + 1;
	 begin
	    Search_List.Delete_First;
	    
	    if not Input_Grid(Grid_Info.X)(Grid_Info.Y).Searched then
	       Input_Grid(Grid_Info.X)(Grid_Info.Y).Searched := True;	 
	       
	       -- Put_Line(Natural'Image(Grid_Info.X) & Natural'Image(Grid_Info.Y) & Natural'Image(Grid_Info.Distance));

	       if Input_Grid(Grid_Info.X)(Grid_Info.Y).Is_End then
		  Put_Line(Natural'Image(Grid_Info.Distance));
		  exit;
	       end if;
	       
	       if Grid_Info.X > Input_Grid.First_Index and then
		 Input_Grid(Grid_Info.X - 1)(Grid_Info.Y).Height - Current_Height <= 1 and then
		 not Input_Grid(Grid_Info.X - 1)(Grid_Info.Y).Searched then
		  Search_List.Append(Search_Info'(X => Grid_Info.X - 1, Y => Grid_Info.Y, Distance => New_Dist));
	       end if;
	       
	       if Grid_Info.X < Input_Grid.Last_Index and then
		 Input_Grid(Grid_Info.X + 1)(Grid_Info.Y).Height - Current_Height <= 1 and then
		 not Input_Grid(Grid_Info.X + 1)(Grid_Info.Y).Searched then
		  Search_List.Append(Search_Info'(X => Grid_Info.X + 1, Y => Grid_Info.Y, Distance => New_Dist));
	       end if;
	       
	       if Grid_Info.Y > Input_Grid(Grid_Info.X).First_Index and then
		 Input_Grid(Grid_Info.X)(Grid_Info.Y - 1).Height - Current_Height <= 1 and then
		 not Input_Grid(Grid_Info.X)(Grid_Info.Y - 1).Searched then
		  Search_List.Append(Search_Info'(X => Grid_Info.X, Y => Grid_Info.Y - 1, Distance => New_Dist));
	       end if;
	       
	       if Grid_Info.Y < Input_Grid(Grid_Info.X).Last_Index and then
		 Input_Grid(Grid_Info.X)(Grid_Info.Y + 1).Height - Current_Height <= 1 and then
		 not Input_Grid(Grid_Info.X)(Grid_Info.Y + 1).Searched then
		  Search_List.Append(Search_Info'(X => Grid_Info.X, Y => Grid_Info.Y + 1, Distance => New_Dist));
	       end if;
	    end if;
	 end;
      end loop;
   end Do_Bfs;
begin
   while not End_Of_File loop
      declare
	 S : constant String := Get_Line;
	 V : Grid_Info_Vector;
      begin
	 Input_Grid.Append(V);
	 for C of S loop
	    declare
	       Grid : Grid_Info;
	    begin
	       if C >= 'a' and C <= 'z' then
		  Grid.Height := Character'Pos(C) - Character'Pos('a');
	       elsif C = 'S' then
		  Grid.Height := 0;
		  Grid.Is_Start := True;
	       elsif C = 'E' then
		  Grid.Height := 25;
		  Grid.Is_End := True;
	       end if;
	       
	       Input_Grid(Input_Grid.Last_Index).Append(Grid);
	    end;
	 end loop;
      end;      
   end loop;

   for I in Input_Grid.First_Index .. Input_Grid.Last_Index loop
      for J in Input_Grid(I).First_Index .. Input_Grid(I).Last_Index loop
	 if Input_Grid(I)(J).Is_Start then
	    Start_X := I;
	    Start_Y := J;
	    exit;
	 end if;
      end loop;
   end loop;
      
   -- Put_Line(Natural'Image(Start_X) & Natural'Image(Start_Y));
   
   -- for I in Input_Grid.First_Index .. Input_Grid.Last_Index loop
   --    for J in Input_Grid(I).First_Index .. Input_Grid(I).Last_Index loop
   -- 	 if Input_Grid(I)(J).Is_End then
   -- 	    Put_Line(Natural'Image(I) & Natural'Image(J));
   -- 	    -- exit;
   -- 	 end if;
   --    end loop;
   -- end loop;

   -- question 1 : single starting point
   Search_List.Append(Search_Info'(X => Start_X, Y => Start_Y, Distance => 0));
   Do_Bfs;

   -- question 2 : all places with height 0 can be starting points
   -- to solve question 2, we need to clear the search state first
   for Line of Input_Grid loop
      for Elem of Line loop
	 Elem.Searched := False;
      end loop;
   end loop;
   
   Search_List.Clear;
   
   for I in Input_Grid.First_Index .. Input_Grid.Last_Index loop
      for J in Input_Grid(I).First_Index .. Input_Grid(I).Last_Index loop
	 if Input_Grid(I)(J).Height = 0 then
	    Search_List.Append(Search_Info'(X => I, Y => J, Distance => 0));
	 end if;
      end loop;
   end loop;
   
   Do_Bfs;
   
end Advent_12;

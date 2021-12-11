with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
  
package body Octopus is
   procedure Flash_Step(Grid: in out Octopus_Grid; Num_Flashes: out Natural) is
      procedure Flash(I: Extended_Line_Index; J: Extended_Column_Index) is
      begin
	 if Grid(I,J).Flashing = True then
	    return;
	 end if;
	 Grid(I,J).Flashing := True;
	 for X in I - 1 .. I + 1 loop
	    for Y in J - 1 .. J + 1 loop
	       if X in Line_Index'Range and then Y in Column_Index'Range and then not Grid(X,Y).Flashing then
		  Grid(X,Y).Energy := Grid(X,Y).Energy + 1;
		  if Grid(X,Y).Energy > Energy_Range'Last then
		     Flash(X,Y);
		  end if;
	       end if;
	    end loop;
	 end loop;
      end Flash;
   begin
      Num_Flashes := 0;
      
      for O of Grid loop
	 O.Flashing := False;
      end loop;
      
      for I in Line_Index'Range loop
	 for J in Column_Index'Range loop
	    Grid(I,J).Energy := Grid(I,J).Energy + 1;
	    if Grid(I,J).Energy > Energy_Range'Last then
	       Flash(I,J);
	    end if;
	 end loop;
      end loop;
      
      for I in Line_Index'Range loop
	 for J in Column_Index'Range loop
	    if Grid(I,J).Flashing then
	       Num_Flashes := Num_Flashes + 1;
	       Grid(I,J).Energy := 0;
	    end if;
	 end loop;
      end loop;
   end Flash_Step;
   
   procedure Get_Octopus_Grid(Grid: out Octopus_Grid) is
   begin
      for X in Line_Index'Range loop
	 declare
	    Line : constant String := Get_Line;
	 begin
	    for Y in Column_Index'Range loop
	       Grid(X,Y).Energy := Character'Pos(Line(Positive(Y))) - Character'Pos('0');
	       Grid(X,Y).Flashing := False;
	    end loop;
	 end;
      end loop;
   end Get_Octopus_Grid;
   
   procedure Print_Grid(Grid: Octopus_Grid) is
   begin
      for X in Line_Index'Range loop
	 for Y in Column_Index'Range loop
	    if Grid(X,Y).Flashing then
	       Put('*');
	    else
	       Put(Integer(Grid(X,Y).Energy), Width => 1);
	    end if;
	 end loop;
	 New_Line;
      end loop;
   end Print_Grid;
   
   function All_Flashing(Grid: Octopus_Grid) return Boolean is
   begin
      for O of Grid loop
	 if not O.Flashing then
	    return False;
	 end if;
      end loop;
      return True;
   end All_Flashing;
end Octopus;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Filesystem; use Filesystem;

procedure Advent_07 is
   Root_Directory : Directory_Access := New_Directory(null);
   Current_Directory : Directory_Access := Root_Directory;
   
   procedure Parse_Commands is
   begin
      while not End_Of_File loop
	 declare
	    Line: constant String := Get_Line;
	 begin
	    if Line(1) = '$' then
	       -- command
	       if Line(3 .. 4) = "cd" then
		  declare
		     Dirname : constant String := Line(6 .. Line'Last);
		  begin
		     if Dirname = "/" then
			Current_Directory := Root_Directory;
		     elsif Dirname = ".." then
			Current_Directory := Current_Directory.Parent;
		     else
			for E of Current_Directory.Entries loop
			   if E.T = Dir and then E.Name.all = Dirname then
			      Current_Directory := E.Dir_Access;
			      exit;
			   end if;
			end loop;
		     end if;
		  end;
	       end if;
	       
	       if Line(3 .. 4) = "ls" then
		  null; -- ...
	       end if;
	    else -- not command, listing files
	       if Line'Length > 3 and then Line(1 .. 3) = "dir" then
		  declare
		     Dirname : constant String := Line(5 .. Line'Last);
		  begin
		     Add_Directory(Current_Directory, Dirname);
		  end;
	       else
		  -- parse "size name" pattern
		  for I in Line'Range loop
		     if Line(I) = ' ' then
			declare
			   Filesize : Positive := Positive'Value(Line(1 .. I - 1));
			   Filename : String := Line(I + 1 .. Line'Last);
			begin
			   Add_File(Current_Directory.all, Filename, Filesize);
			end;
			exit;
		     end if;
		  end loop;
	       end if;
	    end if;
	 end;
      end loop;
   end Parse_Commands;
   
   Sum_Size : Natural := 0;
   
   procedure Solve_1(D : Directory) is
      Size : Natural := Total_Directory_Size(D);
   begin
      if Size <= 100000 then
	 Sum_Size := Sum_Size + Size;
      end if;
      
      for Ent of D.Entries loop
	 if Ent.T = Dir then
	    Solve_1(Ent.Dir_Access.all);
	 end if;
      end loop;
   end Solve_1;
   
   function Solve_2(D : Directory) return Natural is
      Total_Space : constant Positive := 7000_0000;
      Unused_Space_Needed : constant Positive := 3000_0000;
      Current_Used_Space : constant Positive := Total_Directory_Size(D);
      Need_To_Free : constant Integer := Current_Used_Space - (Total_Space - Unused_Space_Needed);
      
      function Find_Smallest_To_Remove(Subdir : Directory) return Positive is
	 Smallest : Positive := Total_Directory_Size(Subdir);
      begin
	 for Ent of Subdir.Entries loop
	    if Ent.T = Dir then
	       declare
		  Dir_Size : constant Natural := Total_Directory_Size(Ent.Dir_Access.all);
		  Smallest_Sub : Positive;
	       begin
		  if Dir_Size >= Need_To_Free then
		     Smallest_Sub := Find_Smallest_To_Remove(Ent.Dir_Access.all);
		     if Smallest_Sub < Smallest then
			Smallest := Smallest_Sub;
		     end if;
		  end if;
	       end;
	    end if;
	 end loop;
	 return Smallest;
      end Find_Smallest_To_Remove;
   begin
      if Need_To_Free <= 0 then
	 return 0;
      end if;
      
      return Find_Smallest_To_Remove(D);
   end Solve_2;
begin
   Parse_commands;
   -- Print_Tree(Root_Directory.all);
   Solve_1(Root_Directory.all);
   Put_Line(Natural'Image(Sum_Size));
   Put_Line(Natural'Image(Solve_2(Root_Directory.all)));
end Advent_07;

with Ada.Text_Io; use Ada.Text_Io;

package body Filesystem is
   function New_Directory(Parent: Directory_Access) return Directory_Access is
   begin
      return new Directory'(Parent => Parent, Entries => <>);
   end New_Directory;

   function Has_File(D: in out Directory; Name: String) return Boolean is
   begin
      for E of D.Entries loop
	 if E.Name.all = Name then
            return True;
	 end if;
      end loop;
      return False;
   end Has_File;

   procedure Add_File(D: in out Directory; Name: String; Size: Positive) is
   begin
      if not Has_File(D, Name) then
	 declare
            New_File: Directory_Entry_Access := new Directory_Entry'
	      (T => File, Name => new String'(Name), Size => Size);
	 begin
            D.Entries.Append(New_File);
	 end;
      end if;
   end Add_File;

   procedure Add_Directory(D: Directory_Access; Name: String) is
   begin
      if not Has_File(D.all, Name) then
	 D.Entries.Append(new Directory_Entry'
			    (T => Dir, Name => new String'(Name), Dir_Access => New_Directory(D)));
      end if;
   end Add_Directory;
   
   procedure Print_Tree(D : Directory; Level : Natural := 0) is
      procedure Indent is
      begin
	 for I in 1 .. Level * 3 loop
	    Put(" ");
	 end loop;
      end Indent;
   begin
      for Ent of D.Entries loop
	 Indent;
	 if Ent.T = Dir then
	    Put_Line("Dir " & Ent.Name.all);
	    Print_Tree(Ent.Dir_Access.all, Level + 1);
	 else
	    Put_Line("File " & Ent.Name.all & " size" & Positive'Image(Ent.Size));
	 end if;
      end loop;
   end Print_Tree;
   
   function Total_Directory_Size(D: Directory) return Natural is
      Total_Size : Natural := 0;
   begin
      for Ent of D.Entries loop
	 if Ent.T = Dir then
	    Total_Size := Total_Size + Total_Directory_Size(Ent.Dir_Access.all);
	 else
	    Total_Size := Total_Size + Ent.Size;
	 end if;
      end loop;
      return Total_Size;
   end Total_Directory_Size;
end Filesystem;

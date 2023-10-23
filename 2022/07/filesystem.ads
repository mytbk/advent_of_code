with Ada.Containers.Vectors;

package Filesystem is
   type Directory;
   type Directory_Access is access all Directory;

   type Directory_Entry_Type is (File,Dir);
   type Directory_Entry(T: Directory_Entry_Type);
   type Directory_Entry_Access is access Directory_Entry;

   type Directory_Entry(T: Directory_Entry_Type) is record
      Name: access String;
      case T is
	 when File =>
            Size: Positive;
	 when Dir =>
            Dir_Access: Directory_Access;
      end case;
   end record;
   -- A Directory_Entry is either a file or directory under a directory

   package Directory_Entry_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Directory_Entry_Access);
   subtype Directory_Entry_Vector is Directory_Entry_Vectors.Vector;
   type Directory is record
      Parent: Directory_Access := null;
      Entries: Directory_Entry_Vector;
   end record;

   function New_Directory(Parent: Directory_Access) return Directory_Access;
   function Parent_Directory(D: Directory) return Directory_Access is (D.Parent);
   function Has_File(D: in out Directory; Name: String) return Boolean;
   procedure Add_File(D: in out Directory; Name: String; Size: Positive);
   procedure Add_Directory(D: Directory_Access; Name: String);
   procedure Print_Tree(D: Directory; Level : Natural := 0);
   function Total_Directory_Size(D: Directory) return Natural;
end Filesystem;

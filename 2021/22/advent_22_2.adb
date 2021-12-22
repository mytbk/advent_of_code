with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

procedure Advent_22_2 is
   type Cube_Range is record
      Xmin, Xmax, Ymin, Ymax, Zmin, Zmax : Integer;
   end record;
   
   type Reboot_Step is record
      Is_Turn_On: Boolean;
      Cuboid: Cube_Range;
   end record;
   
   procedure Parse_Reboot_Step(S: String; Step: out Reboot_Step) is
      Idx: Positive := S'First;
      Last: Positive;
      Cuboid: Cube_Range renames Step.Cuboid;
   begin
      if S(S'First .. S'First + 1) = "on" then
	 Step.Is_Turn_On := True;
	 Idx := Idx + 3;
      else
	 Step.Is_Turn_On := False;
	 Idx := Idx + 4;
      end if;
      
      Idx := Idx + 2;
      -- skip "x="
      Get(S(Idx .. S'Last), Cuboid.Xmin, Last);
      Idx := Last + 3;
      Get(S(Idx .. S'Last), Cuboid.Xmax, Last);
      
      -- skip ",y="
      Idx := Last + 4;
      Get(S(Idx .. S'Last), Cuboid.Ymin, Last);
      Idx := Last + 3;
      Get(S(Idx .. S'Last), Cuboid.Ymax, Last);
      
      -- skip ",z="
      Idx := Last + 4;
      Get(S(Idx .. S'Last), Cuboid.Zmin, Last);
      Idx := Last + 3;
      Get(S(Idx .. S'Last), Cuboid.Zmax, Last);
   end Parse_Reboot_Step;
   
   package Reboot_Step_Vectors is new Ada.Containers.Vectors
     (Element_Type => Reboot_Step, Index_Type => Positive);
   
   subtype Reboot_Step_Vector is Reboot_Step_Vectors.Vector;
   
   Reboot_Steps: Reboot_Step_Vector;

   package Point_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Integer);
   subtype Point_Set is Point_Sets.Set;
   
   package Point_Vectors is new Ada.Containers.Vectors
     (Element_Type => Integer, Index_Type => Positive);
   subtype Point_Vector is Point_Vectors.Vector;
   
   X_Points, Y_Points, Z_Points: Point_Set;

   -- these point list is converted from sets
   X_Point_List, Y_Point_List, Z_Point_List: Point_Vector;
   Num_X_Points, Num_Y_Points, Num_Z_Points: Positive;
   
   type Cube_Array is array (Positive range <>, Positive range <>, Positive range <>)
     of Boolean;
   type Cube_Array_Access is access Cube_Array;
   
   Cube_Array_Ptr : Cube_Array_Access;
   
   type Cube_Count_Type is range 0 .. 2 ** 63 - 1;
   Num_Cube_On : Cube_Count_Type := 0;
begin
   while not End_Of_File loop
      declare
	 L: constant String := Get_Line;
	 Step: Reboot_Step;
      begin
	 Parse_Reboot_Step(L, Step);
	 Reboot_Steps.Append(Step);
      end;
   end loop;
   
   for Step of Reboot_Steps loop
      declare
	 Cub: Cube_Range renames Step.Cuboid;
      begin
	 X_Points.Include(Cub.Xmin);
	 X_Points.Include(Cub.Xmax + 1);
	 Y_Points.Include(Cub.Ymin);
	 Y_Points.Include(Cub.Ymax + 1);
	 Z_Points.Include(Cub.Zmin);
	 Z_Points.Include(Cub.Zmax + 1);
      end;
   end loop;
   
   for X of X_Points loop
      X_Point_List.Append(X);
   end loop;
   for Y of Y_Points loop
      Y_Point_List.Append(Y);
   end loop;
   for Z of Z_Points loop
      Z_Point_List.Append(Z);
   end loop;
   Num_X_Points := Positive(X_Point_List.Length) - 1;
   Num_Y_Points := Positive(Y_Point_List.Length) - 1;
   Num_Z_Points := Positive(Z_Point_List.Length) - 1;
   
   Cube_Array_Ptr := new Cube_Array
     (1 .. Num_X_Points, 1 .. Num_Y_Points, 1 .. Num_Z_Points);
   
   for I in 1 .. Num_X_Points loop
      for J in 1 .. Num_Y_Points loop
	 for K in 1 .. Num_Z_Points loop
	    Cube_Array_Ptr.all(I,J,K) := False;
	 end loop;
      end loop;
   end loop;

   for Step of Reboot_Steps loop
      declare
	 Cub: Cube_Range renames Step.Cuboid;
      begin
	 for I in 1 .. Num_X_Points loop
	    exit when X_Point_List(I) > Cub.Xmax;
	    if X_Point_List(I) >= Cub.Xmin and then X_Point_List(I) <= Cub.Xmax then
	       for J in 1 .. Num_Y_Points loop
		  exit when Y_Point_List(J) > Cub.Ymax;
		  if Y_Point_List(J) >= Cub.Ymin and then Y_Point_List(J) <= Cub.Ymax then
		     for K in 1 .. Num_Z_Points loop
			exit when Z_Point_List(K) > Cub.Zmax;
			if Z_Point_List(K) >= Cub.Zmin and then Z_Point_List(K) <= Cub.Zmax then
			   --Put(positive'Image(I) & " " & positive'Image(J) & " " & Positive'Image(K));
			   --New_Line;
			   Cube_Array_Ptr.all(I,J,K) := Step.Is_Turn_On;
			end if;
		     end loop;
		  end if;
	       end loop;
	    end if;
	 end loop;
      end;
   end loop;
   
   for I in 1 .. Num_X_Points loop
      for J in 1 .. Num_Y_Points loop
	 for K in 1 .. Num_Z_Points loop
	    if Cube_Array_Ptr.all(I,J,K) then
	       Num_Cube_On := Num_Cube_On +
		 Cube_Count_Type(X_Point_List(I+1) - X_Point_List(I)) *
		 Cube_Count_Type(Y_Point_List(J+1) - Y_Point_List(J)) *
		 Cube_Count_Type(Z_Point_List(K+1) - Z_Point_List(K));
	    end if;
	 end loop;
      end loop;
   end loop;

   Put_Line(Cube_Count_Type'Image(Num_Cube_On));
end Advent_22_2;

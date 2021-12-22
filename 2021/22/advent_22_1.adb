with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Containers.Vectors;

procedure Advent_22_1 is
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
   
   subtype Init_Cube_Region is Integer range -50 .. 50;
   Region_Begin : constant := Init_Cube_Region'First;
   Region_End : constant := Init_Cube_Region'Last;

   type Init_Cube_Array is array (Init_Cube_Region, Init_Cube_Region, Init_Cube_Region) of Boolean;
   Init_Cube: Init_Cube_Array := (others => (others => (others => False)));
   Num_Init_Cube_On: Natural := 0;
   
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
	 Xbegin : constant Integer := Integer'Max(Step.Cuboid.Xmin, Region_Begin);
	 Xend: constant Integer := Integer'Min(Step.Cuboid.Xmax, Region_End);
	 Ybegin : constant Integer := Integer'Max(Step.Cuboid.Ymin, Region_Begin);
	 Yend : constant Integer := Integer'Min(Step.Cuboid.Ymax, Region_End);
	 Zbegin : constant Integer := Integer'Max(Step.Cuboid.Zmin, Region_Begin);
	 Zend : constant Integer := Integer'Min(Step.Cuboid.Zmax, Region_End);
      begin
	 for X in Xbegin .. Xend loop
	    for Y in Ybegin .. Yend loop
	       for Z in Zbegin .. Zend loop
		  Init_Cube(X,Y,Z) := Step.Is_Turn_On;
	       end loop;
	    end loop;
	 end loop;
      end;
   end loop;
   
   for X in Init_Cube_Region'Range loop
      for Y in Init_Cube_Region'Range loop
	 for Z in Init_Cube_Region'Range loop
	    if Init_Cube(X,Y,Z) then
	       Num_Init_Cube_On := Num_Init_Cube_On + 1;
	    end if;
	 end loop;
      end loop;
   end loop;
   
   Put(Num_Init_Cube_On); New_Line;
end Advent_22_1;

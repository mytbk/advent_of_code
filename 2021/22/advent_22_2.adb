with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Containers.Vectors;
with Segment_Tree; use Segment_Tree;

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

   Segment_Cube : Segment_Access := New_Segment_Cube;
   
   Num_Cube_On : Seg_Count_Type;
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
	 Insert(Segment_Cube.all, Cub.Xmin, Cub.Xmax, Cub.Ymin, Cub.Ymax, Cub.Zmin, Cub.Zmax,
		Step.Is_Turn_On);
      end;
   end loop;
   
   Num_Cube_On := Count_Value(Segment_Cube.all, True);
   Put_Line(Seg_Count_Type'Image(Num_Cube_On));
end Advent_22_2;

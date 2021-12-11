package Octopus is
   type Extended_Energy_Range is new Natural;
   subtype Energy_Range is Extended_Energy_Range range 0 .. 9;
   type Octopus_State is record
      Energy: Extended_Energy_Range;
      Flashing: Boolean;
   end record;
   
   type Extended_Line_Index is range 0 .. 11;
   type Extended_Column_Index is range 0 .. 11;
   subtype Line_Index is Extended_Line_Index range 1 .. 10;
   subtype Column_Index is Extended_Column_Index range 1 .. 10;
   
   type Octopus_Grid is array (Line_Index, Column_Index) of Octopus_State;
   
   procedure Flash_Step(Grid: in out Octopus_Grid; Num_Flashes: out Natural);
   procedure Get_Octopus_Grid(Grid: out Octopus_Grid);
   procedure Print_Grid(Grid: Octopus_Grid);
   function All_Flashing(Grid: Octopus_Grid) return Boolean;
end Octopus;

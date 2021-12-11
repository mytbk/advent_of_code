with Ada.Text_Io; use Ada.Text_Io;
with Octopus; use Octopus;

procedure Advent_11_1 is
   Grid: Octopus_Grid;
   Num_Flashes: Natural;
   Total_Flashes: Natural := 0;
begin
   Get_Octopus_Grid(Grid);
   
   -- Put_Line("Initial grid.");
   -- Print_Grid(Grid);

   for I in 1 .. 100 loop
      Flash_Step(Grid, Num_Flashes);
      -- Put_Line("Step " & Integer'Image(I));
      -- Print_Grid(Grid);
      Total_Flashes := Total_Flashes + Num_Flashes;
   end loop;
   
   Put_Line(Natural'Image(Total_Flashes));
end Advent_11_1;

with Ada.Text_Io; use Ada.Text_Io;
with Octopus; use Octopus;

procedure Advent_11_2 is
   Grid: Octopus_Grid;
   Unused: Natural;
   First_All_Flash: Natural := 0;
begin
   Get_Octopus_Grid(Grid);
   
   while not All_Flashing(Grid) loop
      Flash_Step(Grid, Unused);
      First_All_Flash := First_All_Flash + 1;
   end loop;
   
   Put_Line(Natural'Image(First_All_Flash));
end Advent_11_2;

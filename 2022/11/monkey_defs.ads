with Monkeys; use Monkeys;
with Ada.Containers.Vectors;

package Monkey_Defs is
   package Monkey_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Monkey_Desc);
   
   function Common_Mod return Worry_Level;
   All_Monkeys : Monkey_Vectors.Vector;
   
   procedure Init_Monkeys;
end Monkey_Defs;

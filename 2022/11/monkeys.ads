with Ada.Containers.Doubly_Linked_Lists;

package Monkeys is
   
   type Worry_Level is new Long_Long_Integer;
   
   package Int_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Worry_Level);
   
   type Monkey_State is record
      Items : Int_Lists.List;
   end record;

   procedure Insert_Item(Monkey: in out Monkey_State; Item : Worry_Level);
   
   type Monkey_Init_Proc is access procedure(Monkey : in out Monkey_State);
   type Monkey_Operation is access function(Old : Worry_Level) return Worry_Level;
   type Monkey_Next is access function(Value: Worry_Level) return Integer;
   
   type Monkey_Subprogs is record
      Operation : Monkey_Operation;
      Next_Func : Monkey_Next;
   end record;
   
   type Monkey_Desc is record
      State : Monkey_State;
      Subprogs : Monkey_Subprogs;
   end record;
end Monkeys;

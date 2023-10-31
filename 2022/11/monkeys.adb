package body Monkeys is
   procedure Insert_Item(Monkey: in out Monkey_State; Item: Worry_Level) is
   begin
      Monkey.Items.Append(Item);
   end Insert_Item;
end Monkeys;
